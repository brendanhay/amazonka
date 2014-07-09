{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return the notification configuration of a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketNotification where

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

-- | Default GetBucketNotification request.
getBucketNotification :: BucketName -- ^ 'gbnrBucket'
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { gbnrBucket = p1
    }

data GetBucketNotification = GetBucketNotification
    { gbnrBucket :: BucketName
    } deriving (Eq, Show, Generic)

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toBS gbnrBucket
        ]

instance ToQuery GetBucketNotification

instance ToHeaders GetBucketNotification

instance ToBody GetBucketNotification

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3

    request  = get

data instance Rs GetBucketNotification = GetBucketNotificationResponse
    { gbnoTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Eq, Show, Generic)

