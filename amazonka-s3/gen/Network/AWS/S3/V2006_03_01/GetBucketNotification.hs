{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.S3.V2006_03_01.GetBucketNotification
    (
    -- * Request
      GetBucketNotification
    -- ** Request constructor
    , mkGetBucketNotificationRequest
    -- ** Request lenses
    , gbnrBucket

    -- * Response
    , GetBucketNotificationResponse
    -- ** Response lenses
    , gbnoTopicConfiguration
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketNotification' request.
mkGetBucketNotificationRequest :: BucketName -- ^ 'gbnrBucket'
                               -> GetBucketNotification
mkGetBucketNotificationRequest p1 = GetBucketNotification
    { _gbnrBucket = p1
    }
{-# INLINE mkGetBucketNotificationRequest #-}

newtype GetBucketNotification = GetBucketNotification
    { _gbnrBucket :: BucketName
    } deriving (Show, Generic)

gbnrBucket :: Lens' GetBucketNotification (BucketName)
gbnrBucket = lens _gbnrBucket (\s a -> s { _gbnrBucket = a })
{-# INLINE gbnrBucket #-}

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toBS _gbnrBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery GetBucketNotification{..} = mconcat
        [ "notification"
        ]

instance ToHeaders GetBucketNotification

instance ToBody GetBucketNotification

newtype GetBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnoTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Show, Generic)

gbnoTopicConfiguration :: Lens' GetBucketNotificationResponse (Maybe TopicConfiguration)
gbnoTopicConfiguration = lens _gbnoTopicConfiguration (\s a -> s { _gbnoTopicConfiguration = a })
{-# INLINE gbnoTopicConfiguration #-}

instance FromXML GetBucketNotificationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationResponse

    request = get
    response _ = xmlResponse
