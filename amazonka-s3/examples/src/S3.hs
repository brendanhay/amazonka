{-# LANGUAGE OverloadedStrings #-}

-- Module      : S3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3 where

import Control.Lens
import Control.Monad.Trans.AWS
import Data.Conduit
import Network.AWS.S3
import System.IO

example :: IO (Either Error ListBucketsResponse)
example = do
    lgr <- newLogger Debug stdout
    env <- getEnv Ireland Discover <&> envLogger .~ lgr
    runAWST env $ do
         paginate listBuckets $$ awaitForever logInfo
