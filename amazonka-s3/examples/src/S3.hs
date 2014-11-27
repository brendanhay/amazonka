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

import qualified Data.ByteString.Builder as Build
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Conduit
import qualified Data.Conduit.List       as Conduit
import           Network.AWS.Data
import           Network.AWS.S3
import           System.IO

example :: IO (Either Error ())
example = do
    lgr <- newLogger Debug stdout
    env <- getEnv Ireland Discover <&> envLogger .~ lgr
    runAWST env $ do
        bs <- view lbrBuckets <$> send listBuckets
        forM_ bs $ \b ->
            paginate (listObjects (b ^. bName))
                $$ Conduit.mapM_ (logInfo . view oKey)
