{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import           Data.List            (unionBy)
import           Data.Monoid
import           Data.Ord
import qualified Data.Text            as Text
import           Gen.V2.Log
import           Gen.V2.Types
import           System.Directory
import           System.FilePath

loadModel :: FilePath -> FilePath -> Script Model
loadModel o d = do
    v <- version
    Model name v . foldl' merge mempty <$> sequence
        [ required override
        , required (api     v)
        , optional (waiters v)
        , optional (pagers  v)
        ]
  where
    version = do
        fs <- scriptIO (getDirectoryContents d)
        f  <- tryHead ("Failed to get model version from " ++ d) (filter dots fs)
        return (takeWhile (/= '.') f)

    required f = object f >>= hoistEither
    optional   = fmap (fromMaybe mempty . hush) . object

    object f = scriptIO $ do
        p <- doesFileExist f
        when p (say "Decode Model" f)
        bool (return  . Left $ "Failed to find " ++ f)
             (eitherDecode' <$> LBS.readFile f)
             p

    api     = path "api.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    path e v = d </> v <.> e

    override = o </> name <.> "json"

    name = takeBaseName (dropTrailingPathSeparator d)

merge :: Object -> Object -> Object
merge (Obj a) (Obj b) = Obj (assoc value a b)
  where
    value :: Value -> Value -> Value
    value l r =
        case (l, r) of
            (Object x, Object y) -> Object (x `merge` y)
            (_,        _)        -> l

    assoc :: Eq k => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
    assoc f xs ys = unionBy ((==) `on` fst) (map g xs) ys
      where
        g (k, x) | Just y <- lookup k ys = (k, f x y)
                 | otherwise             = (k, x)
