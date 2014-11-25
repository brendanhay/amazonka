{-# LANGUAGE OverloadedStrings #-}

-- Module      : Examples.S3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Examples.S3 where

import Control.Monad.Trans.AWS
import Examples.Internal
import Network.AWS.S3

integration :: Bool -> IO (Either Error ListBucketsResponse)
integration dbg = do
    env <- discoverEnv dbg
    runAWST env $ send listBuckets
