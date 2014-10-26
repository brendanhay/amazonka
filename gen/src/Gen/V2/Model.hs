{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.V2.Model
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Model where

import           Control.Applicative  ((<$>))
import           Control.Error
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import           Data.Function        (on)
import qualified Data.HashMap.Strict  as Map
import           Data.Jason           (eitherDecode')
import           Data.Jason.Types     hiding (object)
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import           Gen.V2.JSON
import           Gen.V2.Log
import           Gen.V2.Types
import           System.Directory
import           System.FilePath

storeS2 :: ToJSON a => FilePath -> Model S1 -> a -> Script ()
storeS2 d m x = scriptIO $ do
    p <- doesFileExist f
    say (if p then "Overwrite" else "Create") f
    LBS.writeFile f (encodePretty x)
  where
    f = pathS2 d m

loadS2 :: FilePath -> FilePath -> Model S1 -> Script (Model S2)
loadS2 o d m =
    Model (_mName m) (_mVersion m) d . merge <$> sequence
        [ required (o </> _mName m <.> "json")
        , required (pathS2 d m)
        ]

pathS2 :: FilePath -> Model S1 -> FilePath
pathS2 d m = d </> _mName m <.> _mVersion m <.> "json"

loadS1 :: FilePath -> Script (Model S1)
loadS1 d = do
    v <- version
    Model name v d . merge <$> sequence
        [ required (api v)
        , optional "waiters"    (waiters v)
        , optional "pagination" (pagers  v)
        ]
  where
    version = do
        fs <- scriptIO (getDirectoryContents d)
        f  <- tryHead ("Failed to get model version from " ++ d) (filter dots fs)
        return (takeWhile (/= '.') f)

    api     = path "api.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    path e v = d </> v <.> e

    name = takeBaseName (dropTrailingPathSeparator d)

required :: FromJSON a => FilePath -> Script a
required f = object f >>= hoistEither

optional :: Text -> FilePath -> Script Object
optional k = fmap (fromMaybe (Obj [(k, Object mempty)]) . hush) . object

object :: FromJSON a => FilePath -> Script (Either String a)
object f = scriptIO $ do
    p <- doesFileExist f
    when p (say "Parse Model" f)
    bool (return  . Left $ "Failed to find " ++ f)
         (eitherDecode' <$> LBS.readFile f)
         p

merge :: [Object] -> Object
merge = foldl' go mempty
  where
    go :: Object -> Object -> Object
    go (Obj a) (Obj b) = Obj (assoc value a b)

    value :: Value -> Value -> Value
    value l r =
        case (l, r) of
            (Object x, Object y) -> Object (x `go` y)
            (_,        _)        -> l

    assoc :: Eq k => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
    assoc f xs ys = unionBy ((==) `on` fst) (map g xs) ys
      where
        g (k, x) | Just y <- lookup k ys = (k, f x y)
                 | otherwise             = (k, x)
