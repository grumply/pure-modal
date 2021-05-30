module Pure.Modal where

import Pure (Pure(..))
import Pure.Elm hiding (Open,active,Start,props,ref,not)
import Pure.Data.Prop.TH
import Pure.Data.Lifted

import Data.Coerce

import Data.Function ((&))

data Blurring

data Status 
  = Mounted
  | Opened
  | Closed 
  | Unmounted Bool -- ^ is open
  deriving (Eq,Ord,Show)

instance Theme Opened
instance Theme Closed

-- TODO: clean these notes up and move them to documentation on purehs.org
--
-- By abstracting all callback functionality into a single `onStatus`
-- callback with access to the mount Node and the Status, it is 
-- possible to control what themes are applied to the mount Node 
-- manually. 
--
-- By returning a (Features -> Features) morphism we can modify 
-- the modal itself based on that status. See the blurring 
-- implementation below that is plugged into the `onStatus` callback
-- by default.
--
-- Note that if the Modal is defaulted to open, the Mounted feature
-- morphism will not be applied, and instead the Opened feature
-- morphism is applied. That seemed a more reasonable choice
-- than composition of the two, which was the other option. 
--
-- Unmounted feature morphisms also don't get applied, but for obvious 
-- reasons.
--
-- The returned feature morphism has a higher precedence than the core
-- component feature application, allowing the onStatus callback's
-- feature morphism to override the default features of the Modal
-- component view.
--
-- Note that when the modal is open, it is `Themed @Opened` and 
-- `Themed @Closed` when closed.
-- 
-- I considered the possibility of having a callback in the Opened
-- and Closed Status values instead of the returned function, but it 
-- seemed a little more cumbersome, though it is obviously a more 
-- logical implementation.
--
-- I think a correct implementation of status would look more like:
--
-- data Status 
--   = Mounted (Maybe ((Features -> Features) -> IO ())) 
--   | Opened  (Maybe Node) ((Features -> Features) -> IO ())
--   | Closed  Node ((Features -> Features) -> IO ())
--   | Unmounted Node Bool
--
-- Which is unwieldy.

-- | A view for toggle-able out-of-tree rendering.
--
-- Use:
--
-- > Modal def <| Open someBool |> 
-- >   [ Div <| OnClickWith intercept def |> 
-- >     [ "Some Content" ] 
-- >   ]
-- 
-- Note the `OnClickWith intercept def` that prevents clicks and 
-- highlights of the modal content from closing the modal.
--
-- The default target for the modal is the page body.
--
-- By default this modal will apply the `Blurring` theme to
-- non-modal content on the page. To change this behavior,
-- apply your own theme by setting the `onStatus` callback to
-- `onStatusDefault @MyTheme`. Many other complex use-cases 
-- can be custom crafted with the `onStatus` callback. 
--
-- The core theme can be modified via the `variant` record field.
data Modal = Modal_
  { as :: Features -> [View] -> View
  , features :: Features
  , children :: [View]
  , open :: Bool
  , onStatus :: IO () -> Node -> Status -> IO (Features -> Features)
  , variant :: SomeTheme
  }

deriveLocalComponent ''Modal

instance Default Modal where
  def = 
    Modal_
      { as = \fs cs -> Portal (coerce body) (Div & Features fs & Children cs)
      , features = mempty
      , children = mempty
      , open = False
      , onStatus = onStatusDefault @Blurring (coerce body)
      , variant = mkSomeTheme @Modal
      }

onStatusDefault :: forall t. Theme t => Node -> IO () -> Node -> Status -> IO (Features -> Features)
onStatusDefault root toggle n = \case
  Mounted -> 
    pure id

  Opened -> do
    addThemeClass @t root
    pure (Themed @Opened)

  Closed -> do
    removeThemeClass @t root
    pure (Themed @Closed)

  Unmounted open
    | open -> do
      removeThemeClass @t root
      pure id
    | otherwise -> 
      pure id

data Model = Model
  { active :: Bool
  , ref    :: Node
  , props  :: Features -> Features
  }

data Msg = SetNode Node | Toggle | Receive | Shutdown

instance Pure Modal where
  view m = run (App [] [Receive] [Shutdown] (pure mdl) upon render) m
    where
      mdl = Model False (coerce nullJSV) id

      upon :: Elm Msg => Msg -> Modal -> Model -> IO Model
      upon (SetNode n) modal model = do
        m <- (onStatus modal) (command Toggle) n Mounted
        f <- if open modal 
             then (onStatus modal) (command Toggle) n Opened
             else pure m
        pure model 
          { ref = n
          , active = open modal
          , props = f 
          }

      upon Toggle modal model
        | active model = do
          f <- (onStatus modal) (command Toggle) (ref model) Closed
          pure model
            { active = False
            , props = f
            }
        | otherwise = do
          f <- (onStatus modal) (command Toggle) (ref model) Opened
          pure model
            { active = True
            , props = f
            }

      upon Receive modal model
        | open modal && not (active model) = do
          f <- (onStatus modal) (command Toggle) (ref model) Opened
          pure model 
            { active = True
            , props = f 
            }

        | not (open modal) && active model = do
          f <- (onStatus modal) (command Toggle) (ref model) Closed 
          pure model 
            { active = False
            , props = f 
            }

        | otherwise = 
          pure model

      upon Shutdown modal model = do
        (onStatus modal) (command Toggle) (ref model) (Unmounted (active model))
        pure model
      
      render Modal_ {..} Model {..} =
        let 
          fs = someThemed variant 
             . WithHost (command . SetNode)
             . (if active then 
                  Themed @Opened 
                else 
                  Themed @Closed
               )
        in 
          as (props (fs features)) children

instance Theme Modal where
  theme c =
    is c do
      apply do
        display =: none
        position =: absolute
        top =: 0
        left =: 0
        width =: (100%)
        height =: (100%)
        background-color =: rgba(0,0,0,0.85)
        opacity =: 0
        animation-fill-mode =: both
        animation-duration =: 0.5s
        transition =* [background-color,0.5s,linear]
        user-select =: none
        will-change =* [opacity]
        z-index =: 1000

      -- user-select is not inherited, but the default value of auto
      -- semi-inherits, so we reset to text here.
      child "*" do
        apply do
          user-select =: text

      is (subtheme @Closed) do
        apply do
          important (width =: 0)
          important (height =: 0)

      is (subtheme @Opened) do
        apply do
          opacity =: 1
          display =: block

instance Theme Blurring where
  theme c =
    is c do
      apply do
        overflow =: hidden
      
      child (subtheme @Modal) do
        apply do
          position =: fixed
          background-color =: rgba(0,0,0,0.6)

      child "" do
        isn't (subtheme @Modal) do
          apply do
            "filter" =* [blur(5px),"grayscale(" <> 0.7 <> ")"]
            transition =* [0.8s,"filter",ease]
