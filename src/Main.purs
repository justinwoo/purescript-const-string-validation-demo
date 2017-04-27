module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Char (toCharCode)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Foldable (and, or, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), indexOf, split, toCharArray)

foreign import kind CONDITION
foreign import data YES :: CONDITION

foreign import data Validations :: # CONDITION -> Type

type E a = Either String a
type C v = Const String (Validations v)

startsWithTag :: forall v.
  C v
  -> E (C (startsWithTag :: YES | v))
startsWithTag (Const s)
  | Just a <- indexOf (Pattern "[") s
  , Just b <- indexOf (Pattern "]") s
  , a < b = Right (Const s)
  | otherwise = Left "where's the tag"

containsEpisodeNumber :: forall v.
  C v
  -> E (C (containsEpisodeNumber :: YES | v))
containsEpisodeNumber (Const s)
  | [_, a] <- split (Pattern " - ") s
  , [b, _] <- split (Pattern ".") a
  , c <- toCharArray b
  , upper <- (>=) (toCharCode '9')
  , lower <- (<=) (toCharCode '0')
  , check <- conj upper lower
  , test <- and $ check <<< toCharCode <$> c
  , test == true = Right (Const s)
  | otherwise = Left "where's the episode number"

endsWithExtension :: forall v.
  C v
  -> E (C (endsWithExtension :: YES | v))
endsWithExtension (Const s)
  | [_, a] <- split (Pattern " - ") s
  , [_, b] <- split (Pattern ".") a
  , mkv <- (==) "mkv"
  , avi <- (==) "avi"
  , mp4 <- (==) "mp4"
  , test <- or [mkv, avi, mp4] b
  , test == true = Right (Const s)
  | otherwise = Left "where's the extension"

-- our action which is to be performed only on validated strings
action :: forall e v.
  C
    ( startsWithTag :: YES
    , containsEpisodeNumber :: YES
    , endsWithExtension :: YES
    | v
    )
  -> Eff (console :: CONSOLE | e) Unit
action (Const s) = log $ "yay, we validated " <> s

-- make our Const String with no validations
mkConst :: String -> C ()
mkConst = Const

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  traverse_ go
    [ "Wheresthetag - 01.mkv"
    , "[Crap] WrongFileExtension - 01.app"
    , "[Crap] NoEp numer.mkv"
    , "[Crap] ABCDEF - 01.mkv"
    ]
  where
    go str =
      -- validations are ordered indepedent of the ordering in action
      -- (because they are rows!)
      case endsWithExtension
        =<< startsWithTag
        =<< containsEpisodeNumber (mkConst str) of
        Right s -> action s
        Left e -> log $ show e <> " in " <> str
    -- go2 str =
    --   case endsWithExtension
    --     =<< containsEpisodeNumber (mkConst str) of
    --     Right s -> action s -- errors because startsWithTag validation is missing!
    --     Left e -> log $ show e <> " in " <> str

