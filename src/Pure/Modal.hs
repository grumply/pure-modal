{-# LANGUAGE PatternSynonyms, ExistentialQuantification, ViewPatterns, MultiParamTypeClasses, TypeFamilies, DuplicateRecordFields, RecordWildCards, CPP, DeriveGeneric, OverloadedStrings #-}
module Pure.Modal where

import Pure hiding (Open,Content_,Content)
import Pure.Portal as Portal hiding (child)
import Pure.Data.Prop
import Pure.Data.Cond
import Pure.Theme
import Pure.Data.CSS
import Pure.Data.Lifted

import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.IORef
import Data.Maybe
import GHC.Generics as G

import Data.Function ((&))

data Modal = Modal_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , basic :: Bool
    , closeOnDimmerClick :: Bool
    , closeOnDocumentClick :: Bool
    , defaultOpen :: Bool
    , dimmer :: Maybe Txt
    , mountNode :: Maybe Element
    , onClose :: IO ()
    , onMount :: IO ()
    , onOpen :: IO ()
    , onUnmount :: IO ()
    , open :: Bool
    , scrollable :: Bool
    , size :: Txt
    , styles :: [(Txt,Txt)]
    , withPortal :: Portal.Portal -> Portal.Portal
    , trigger :: View
    , themed :: SomeModalT
    } deriving (Generic)

data SomeModalT = forall t. Themeable t => SomeModalT t
instance Default SomeModalT where
    def = SomeModalT ModalT 

instance Default Modal where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        , dimmer = Just ""
        , closeOnDimmerClick = True
        , closeOnDocumentClick = True
        , withPortal = id
        }

pattern Modal :: Modal -> Modal
pattern Modal m = m

data ModalState = MS
    { topMargin :: Maybe Int
    , scrolling :: Maybe Bool
    , active :: Bool
    , ref :: IORef (Maybe JSV)
    , pendingAnimation :: IORef (IO ())
    }

instance Pure Modal where
    view =
        LibraryComponentIO $ \self ->
            let
                getMountNode = do
                    Modal_ {..} <- ask self
                    b <- getBody
                    return $ fromMaybe (Element $ toJSV b) mountNode

                handleRef (Node n) = do
                    MS {..} <- get self
                    writeIORef ref (Just n)

                handleOpen _ = do
                    Modal_ {..} <- ask self
                    onOpen
                    modify_ self $ \_ MS {..} -> MS { active = True, .. }

                handleClose = do
                    Modal_ {..} <- ask self
                    onClose
                    modify_ self $ \_ MS {..} -> MS { active = False, .. }

                handlePortalMount = do
                    Modal_ {..} <- ask self
                    modify_ self $ \_ MS {..} -> MS { scrolling = Just False, .. }
                    setPositionAndClassNames
                    onMount

                handlePortalUnmount = do
                    Modal_ {..} <- ask self
                    MS     {..} <- get self
                    n <- toJSV <$> getMountNode
                    traverse_ (removeClass n) ["blurring","dimmable","dimmed","scrolling"]
                    writeIORef pendingAnimation def
                    onUnmount

                setPositionAndClassNames = do
                    Modal_ {..} <- ask self
                    MS     {..} <- get self
                    n           <- toJSV <$> getMountNode

                    when (isJust dimmer) $
                      traverse_ (addClass n) ["dimmable","dimmed"]
                    when (dimmer == Just "blurring") $ do
                      addClass n "blurring"

                    mr <- readIORef ref

                    for_ mr $ \r -> do
                      BR { brHeight = h } <- boundingRect (Element r)

                      ih <- innerHeight

                      let topMargin' = negate (round (h / 2))
                          scrolling' = h >= fromIntegral ih

                          scrollingChange = scrolling /= Just scrolling'
                          topMarginChange = topMargin /= Just topMargin'

                      when scrollingChange $
                        (scrolling' ? addClass n $ removeClass n)
                          "scrolling"

                      (scrollingChange || topMarginChange) #
                        modify_ self (\_ MS {..} ->
                          MS { topMargin = Just topMargin'
                             , scrolling = Just scrolling'
                             , ..
                             })

                    writeIORef pendingAnimation setPositionAndClassNames
                    void $ addAnimation (join $ readIORef pendingAnimation)

            in def
                { construct = do
                    Modal_ {..} <- ask self
                    MS def def (open || defaultOpen) <$> newIORef def <*> newIORef def
                , receive = \newprops oldstate -> return $
                    (Pure.Modal.open newprops /= Pure.Modal.active oldstate)
                      ? oldstate { Pure.Modal.active = Pure.Modal.open newprops }
                      $ oldstate
                , unmounted = do
                    Modal_ {..} <- ask self
                    handlePortalUnmount
                , render = \Modal_ {..} MS {..} ->
                    let
                        dimmerClasses
                          | isJust dimmer =
                                [ (dimmer == Just "inverted") # "inverted"
                                , "page modals dimmer transition visible active"
                                ]
                          | otherwise = []

                        viewContent f =
                            let
                                ss = maybe [] (\tm -> [(marginTop,pxs tm)]) topMargin

                                cs =
                                    [ size
                                    , basic # "basic"
                                    , (scrolling == Just True) # "scrolling"
                                    , "modal transition visible active"
                                    ]

                            in
                                as (f $ features & Classes cs & Styles ss & Lifecycle (HostRef handleRef)) children

                    in (View :: Portal.Portal -> View) $ Portal.Portal $ withPortal $ def
                        & (closeOnDocumentClick ? CloseOnDocumentClick True $ id)
                        & (closeOnDimmerClick   ? CloseOnRootNodeClick True $ id)
                        & Portal.PortalNode viewContent
                        & MountNode mountNode
                        & Classes dimmerClasses
                        & Open active
                        & Portal.OnClose handleClose
                        & OnMount handlePortalMount
                        & OnOpen handleOpen
                        & OnUnmounted handlePortalUnmount
                        & Children [ trigger ]
                }

#ifdef __GHCJS__
foreign import javascript unsafe
    "window.innerHeight" innerHeight_js :: IO Int
#endif

innerHeight :: IO Int
innerHeight =
#ifdef __GHCJS__
    innerHeight_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.classList.add($2)" addClass_js :: JSV -> Txt -> IO ()
#endif

addClass :: JSV -> Txt -> IO ()
addClass n c =
#ifdef __GHCJS__
    addClass_js n c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.classList.remove($2)" removeClass_js :: JSV -> Txt -> IO ()
#endif

removeClass :: JSV -> Txt -> IO ()
removeClass n c =
#ifdef __GHCJS__
    removeClass_js n c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$r = $1.getBoundingClientRect()" bounding_client_rect_js :: Element -> IO JSV
#endif

data BoundingRect = BR
    { brLeft :: Double
    , brTop :: Double
    , brRight :: Double
    , brBottom :: Double
    , brWidth :: Double
    , brHeight :: Double
    } deriving (Eq)

instance Default BoundingRect where def = BR 0 0 0 0 0 0

boundingRect :: Element -> IO BoundingRect
boundingRect node = do
#ifdef __GHCJS__
  o <- bounding_client_rect_js node
  return $ fromMaybe (error "Semantic.Utils.boundingRect: fromMaybe got Nothing") $ do
    brLeft   <- o .# "left"
    brTop    <- o .# "top"
    brRight  <- o .# "right"
    brBottom <- o .# "bottom"
    brWidth  <- o .# "width"
    brHeight <- o .# "height"
    return BR {..}
#else
    return $ BR 0 0 0 0 0 0
#endif

data ModalT = ModalT
instance Themeable ModalT where
    theme c _ = do
        is c $ do
            apply $ do
                fontSize =: rems 1
                display =: none
                zIndex =: int 1001
                textAlign =: left
                background =: "#FFFFFF"
                border =: none
                for [ "-webkit-box-shadow", "box-shadow" ] $ \bs ->
                    bs =: pxs 1 <<>> pxs 3 <<>> pxs 3 <<>> pxs 0 <<>> rgba(0,0,0,0.2) <&>> pxs 1 <<>> pxs 3 <<>> pxs 15 <<>> pxs 2 <<>> rgba(0,0,0,0.2)
                for [ "-webkit-transform-origin" , "transform-origin"] $ \to ->
                    to =: per 50 <<>> per 25
                "-webkit-box-flex" =: zero
                for [ "-ms-flex", "flex" ] $ \f ->
                    f =: zero <<>> zero <<>> auto 
                borderRadius =: rems 0.28571429
                for_ [ "-webkit-user-slect", "-moz-user-select", "-ms-user-select", "user-select"] $ \us ->
                    us =: "text"
                "will-change" =: top <&>> left <&>> margin <&>> transform <&>> opacity

            is ".basic" .> do
                backgroundColor =: transparent
                border =: none
                borderRadius =: ems 0
                important $ "-webkit-box-shadow" =: none
                important $ "box-shadow" =: none
                color =: "#FFFFFF"

            is ".loading" .> do
                display =: block
                visibility =: hidden
                zIndex =: neg one

            is ".active" .> do
                display =: block

            is ".scrolling" . is ".dimmable" $ do
                apply $ overflow =: hidden

                child ".dimmer" .> do
                    for [ "-webkit-box-pack", "-ms-flex-pack" ] $ \jc ->
                        jc =: "start"
                    "justify-content" =: "flex-start"
                
                is ".dimmed" . has ".dimmer" .> do
                    overflow =: auto
                    "-webkit-overflow-scrolling" =: "touch"

                has ".dimmer" .> position =: fixed

                
        let responsive sel wid = void $ 
                atMedia ("only screen and " <<>> sel) $ is c .> do
                width =: wid
                margin =: ems 0 <<>> ems 0 <<>> ems 0 <<>> ems 0
        responsive "(max-width: 767px)" (per 95)
        responsive "(min-width: 768px)" (per 88)
        responsive "(min-width: 992px)" (pxs 850)
        responsive "(min-width: 1200px)" (pxs 900)
        responsive "(min-width: 1920px)" (pxs 950)

        let responsive sel siz wid = void $ 
                atMedia ("only screen and " <<>> sel) $ is c . is siz .> do
                width =: wid
                margin =: ems 0 <<>> ems 0 <<>> ems 0 <<>> ems 0
        responsive "(max-width: 767px)" ".mini" (per 95)
        responsive "(min-width: 768px)" ".mini" (per 35.2)
        responsive "(min-width: 992px)" ".mini" (pxs 340)
        responsive "(min-width: 1200px)" ".mini" (pxs 360)
        responsive "(min-width: 1920px)" ".mini" (pxs 380)

        responsive "(max-width: 767px)" ".tiny" (per 95)
        responsive "(min-width: 768px)" ".tiny" (per 52.8)
        responsive "(min-width: 992px)" ".tiny" (pxs 510)
        responsive "(min-width: 1200px)" ".tiny" (pxs 540)
        responsive "(min-width: 1920px)" ".tiny" (pxs 570)

        responsive "(max-width: 767px)" ".small" (per 95)
        responsive "(min-width: 768px)" ".small" (per 70.4)
        responsive "(min-width: 992px)" ".small" (pxs 680)
        responsive "(min-width: 1200px)" ".small" (pxs 720)
        responsive "(min-width: 1920px)" ".small" (pxs 760)

        responsive "(max-width: 767px)" ".large" (per 95)
        responsive "(min-width: 768px)" ".large" (per 88)
        responsive "(min-width: 992px)" ".large" (pxs 1020)
        responsive "(min-width: 1200px)" ".large" (pxs 1080)
        responsive "(min-width: 1920px)" ".large" (pxs 1140)

        is ".dimmer" $ do
            child c $ apply $ do
                for_ [ "-webkit-box-shadow", "box-shadow" ] $ \bs ->
                    bs =: pxs 1 <<>> pxs 3 <<>> pxs 10 <<>> pxs 2 <<>> rgba(0,0,0,0.2)

            is ".top" . is ".aligned" . has c .> do
                margin =: vhs 5 <<>> auto

            is ".inverted" . child c .> do
                color =: rgba(0,0,0,0.87)

        void $ is ".modals" . is ".dimmer" . has ".scrolling" . is c .> important (margin =: rems 1 <<>> auto)

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data Trigger = Trigger_
pattern Trigger :: HasProp Trigger a => Prop Trigger a -> a -> a
pattern Trigger p a <- (getProp Trigger_ &&& id -> (p,a)) where
    Trigger p a = setProp Trigger_ p a

data Basic = Basic_
pattern Basic :: HasProp Basic a => Prop Basic a -> a -> a
pattern Basic p a <- (getProp Basic_ &&& id -> (p,a)) where
    Basic p a = setProp Basic_ p a

data CloseOnDimmerClick = CloseOnDimmerClick_
pattern CloseOnDimmerClick :: HasProp CloseOnDimmerClick a => Prop CloseOnDimmerClick a -> a -> a
pattern CloseOnDimmerClick p a <- (getProp CloseOnDimmerClick_ &&& id -> (p,a)) where
    CloseOnDimmerClick p a = setProp CloseOnDimmerClick_ p a

data DimmerType = DimmerType_
pattern DimmerType :: HasProp DimmerType a => Prop DimmerType a -> a -> a
pattern DimmerType p a <- (getProp DimmerType_ &&& id -> (p,a)) where
    DimmerType p a = setProp DimmerType_ p a

data Scrollable = Scrollable_
pattern Scrollable :: HasProp Scrollable a => Prop Scrollable a -> a -> a
pattern Scrollable p a <- (getProp Scrollable_ &&& id -> (p,a)) where
    Scrollable p a = setProp Scrollable_ p a

data Size = Size_
pattern Size :: HasProp Size a => Prop Size a -> a -> a
pattern Size p a <- (getProp Size_ &&& id -> (p,a)) where
    Size p a = setProp Size_ p a

data IsImage = IsImage_
pattern IsImage :: HasProp IsImage a => Prop IsImage a -> a -> a
pattern IsImage p a <- (getProp IsImage_ &&& id -> (p,a)) where
    IsImage p a = setProp IsImage_ p a

data Scrolling = Scrolling_
pattern Scrolling :: HasProp Scrolling a => Prop Scrolling a -> a -> a
pattern Scrolling p a <- (getProp Scrolling_ &&& id -> (p,a)) where
    Scrolling p a = setProp Scrolling_ p a

instance HasProp As Modal where
    type Prop As Modal = Features -> [View] -> View
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasFeatures Modal where
    getFeatures = features
    setFeatures as m = m { features = as }

instance HasChildren Modal where
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasProp Basic Modal where
    type Prop Basic Modal = Bool
    getProp _ = basic
    setProp _ b m = m { basic = b }

instance HasProp CloseOnDimmerClick Modal where
    type Prop CloseOnDimmerClick Modal = Bool
    getProp _ = closeOnDimmerClick
    setProp _ codc m = m { closeOnDimmerClick = codc }

instance HasProp CloseOnDocumentClick Modal where
    type Prop CloseOnDocumentClick Modal = Bool
    getProp _ = closeOnDocumentClick
    setProp _ codc m = m { closeOnDocumentClick = codc }

instance HasProp DefaultOpen Modal where
    type Prop DefaultOpen Modal = Bool
    getProp _ = defaultOpen
    setProp _ o m = m { defaultOpen = o }

instance HasProp DimmerType Modal where
    type Prop DimmerType Modal = Maybe Txt
    getProp _ = dimmer
    setProp _ d m = m { dimmer = d }

instance HasProp MountNode Modal where
    type Prop MountNode Modal = Maybe Element
    getProp _ = mountNode
    setProp _ mn m = m { mountNode = mn }

instance HasProp OnClose Modal where
    type Prop OnClose Modal = IO ()
    getProp _ = onClose
    setProp _ oc m = m { onClose = oc }

instance HasProp OnMount Modal where
    type Prop OnMount Modal = IO ()
    getProp _ = onMount
    setProp _ om m = m { onMount = om }

instance HasProp OnOpen Modal where
    type Prop OnOpen Modal = IO ()
    getProp _ = onOpen
    setProp _ oo m = m { onOpen = oo }

instance HasProp OnUnmounted Modal where
    type Prop OnUnmounted Modal = IO ()
    getProp _ = onUnmount
    setProp _ ou m = m { onUnmount = ou }

instance HasProp Open Modal where
    type Prop Open Modal = Bool
    getProp _ = open
    setProp _ o m = m { open = o }

instance HasProp Scrollable Modal where
    type Prop Scrollable Modal = Bool
    getProp _ = scrollable
    setProp _ s m = m { scrollable = s }

instance HasProp Size Modal where
    type Prop Size Modal = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp WithPortal Modal where
    type Prop WithPortal Modal = Portal.Portal -> Portal.Portal
    getProp _ = withPortal
    setProp _ wp m = m { withPortal = wp }

instance HasProp Trigger Modal where
    type Prop Trigger Modal = View
    getProp _ = trigger
    setProp _ t m = m { trigger = t }
