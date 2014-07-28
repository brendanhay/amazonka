{-# LANGUAGE OverloadedStrings #-}

-- Module      : Generator.Models
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Models
    ( Model (..)
    , models
    ) where

import Control.Applicative
import Control.Error
import Control.Monad
import Data.List
import Data.Ord
import Generator.Log
import System.Directory
import System.FilePath

data Model = Model
    { modPath    :: FilePath
    , modVersion :: String
    , modGlobal  :: Maybe FilePath
    , modLocal   :: Maybe FilePath
    } deriving (Show, Eq)

instance Ord Model where
    compare = comparing modVersion

models :: Int -> FilePath -> [FilePath] -> Script [Model]
models n o xs = concat . fmap (take n . reverse . sort) <$> mapM model xs
  where
    model d = (json <$> scriptIO (getDirectoryContents d))
        >>= mapM (fromPath o d)

    json = filter (isSuffixOf ".json")

fromPath :: FilePath -> FilePath -> String -> Script Model
fromPath o d f = do
    let file   = d </> f
        global = o </> takeBaseName d <.> ".override.json"
        local  = o </> takeBaseName d </> replaceExtension f ".override.json"

    let exists True  = Just
        exists False = const Nothing

    p <- scriptIO $ (,,)
        <$> doesFileExist file
        <*> doesFileExist global
        <*> doesFileExist local

    case p of
        (False, _, _) -> left ("Unable to locate: " ++ file)
        (_,     g, l) -> do
            say "Located Model" file

            when g (say "Located Override" global)
            when l (say "Located Override" local)

            return $
                Model file (dropExtension f) (exists g global) (exists l local)

