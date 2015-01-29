{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : S3
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.AWS
import           Data.ByteString.Builder (Builder)
import           Data.Conduit
import qualified Data.Conduit.List       as Conduit
import           Data.Monoid
import           Data.Text               (Text)
import           Data.Time.Clock
import           Network.AWS.S3
import           Network.HTTP.Client
import           System.IO

default (Builder)

listAllObjects :: IO (Either Error ())
listAllObjects = do
    l <- newLogger Info stdout
    e <- getEnv Ireland Discover <&> envLogger .~ l
    runAWST e $ do
        info "Listing Buckets ..."
        bs <- view lbrBuckets <$> send listBuckets
        forM_ bs $ \(view bName -> b) -> do
            info $ "Listing Keys in " <> build b
            paginate (listObjects b)
                =$ Conduit.concatMap (view lorContents)
                $$ Conduit.mapM_ (info . view oKey)
        info "Completed."

presignGetObject :: Text -> Text -> IO (Either Error ())
presignGetObject b k = do
    l <- newLogger Info stdout
    e <- getEnv Ireland Discover <&> envLogger .~ l
    t <- getCurrentTime
    runAWST e $ do
        sg <- _sgRequest <$> presign (getObject b k) t 360
        let rq = _sgRequest sg
        info ("https://" <> host rq <> path rq <> queryString rq)
