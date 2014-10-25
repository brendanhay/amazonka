{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Gen.V2.Asset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Asset where

import Control.Applicative
import Control.Error
import Gen.V2.Log
import Gen.V2.Types
import System.Directory
import System.FilePath     hiding (normalise)

copyAssets :: FilePath -> FilePath -> Script ()
copyAssets s d = do
    fs <- map (combine s) . filter dots <$> scriptIO (getDirectoryContents s)
    scriptIO (mapM_ copy fs)
  where
    copy f@(dest -> p) = say "Copying Asset" p >> copyFile f p

    dest f = d </> takeFileName f
