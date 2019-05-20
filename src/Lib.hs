module Lib where

import           Brick.Main
import           Brick.Types
import           Control.Applicative
import qualified Graphics.Vty.Input.Events as Vty

data Pattern a r = Pattern { runPattern :: (a -> Maybe r) }

instance Monoid r => Monoid (Pattern a r) where
  mempty = Pattern $ const Nothing
  mappend (Pattern a) (Pattern b) = Pattern $ \x -> a x <> b x
  moncat = foldr (<>) mempty

instance Functor (Pattern a) where
  fmap f (Pattern a) = Pattern $ \x -> f <$> a x

instance Applicative (Pattern a) where
  pure = Pattern . const . Just
  (Pattern a) <*> (Pattern b) = Pattern $ \x -> a x <*> b x

instance Alternative (Pattern a) where
  empty = Pattern $ const Nothing
  (Pattern a) <|> (Pattern b) = Pattern $ \x -> a x <|> b x

key :: Char -> [Vty.Modifier] -> (s -> EventM n (Next s)) -> Pattern (BrickEvent n e) (s -> EventM n (Next s))
key c m f = keySpecial (Vty.KChar c) m f

keySpecial :: Vty.Key -> [Vty.Modifier] -> (s -> EventM n (Next s)) -> Pattern (BrickEvent n e) (s -> EventM n (Next s))
keySpecial k m f = Pattern $ \x ->
  case x of
    VtyEvent (Vty.EvKey ke me) | ke == k && me == m -> Just f
    _                                               -> Nothing

appEvent :: Eq e => e -> (s -> EventM n (Next s)) -> Pattern (BrickEvent n e) (s -> EventM n (Next s))
appEvent e f = Pattern $ \x ->
  case x of
    AppEvent ee | ee == e -> Just f
    _                     -> Nothing

otherwise :: r -> Pattern i r
otherwise = pure
