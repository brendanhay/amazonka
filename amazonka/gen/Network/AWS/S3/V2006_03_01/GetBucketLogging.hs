{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.GetBucketLogging where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Default GetBucketLogging request.
getBucketLogging :: BucketName -- ^ 'gblrBucket'
                 -> GetBucketLogging
getBucketLogging p1 = GetBucketLogging
    { gblrBucket = p1
    }

data GetBucketLogging = GetBucketLogging
    { gblrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketLogging where
    toPath GetBucketLogging{..} = mconcat
        [ "/"
        , toBS gblrBucket
        ]

instance ToQuery GetBucketLogging

instance ToHeaders GetBucketLogging

instance ToBody GetBucketLogging

instance AWSRequest GetBucketLogging where
    type Sv GetBucketLogging = S3

    request  = get

data instance Rs GetBucketLogging = GetBucketLoggingResponse
    { gbloLoggingEnabled :: Maybe LoggingEnabled
    } deriving (Eq, Show, Generic)
