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
    , getBucketNotification
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

-- | Minimum specification for a 'GetBucketNotification' request.
getBucketNotification :: BucketName -- ^ 'gbnrBucket'
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { _gbnrBucket = p1
    }
{-# INLINE getBucketNotification #-}

data GetBucketNotification = GetBucketNotification
    { _gbnrBucket :: BucketName
    } deriving (Show, Generic)

gbnrBucket :: Lens' GetBucketNotification (BucketName)
gbnrBucket f x =
    f (_gbnrBucket x)
        <&> \y -> x { _gbnrBucket = y }
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

data GetBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnoTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Show, Generic)

gbnoTopicConfiguration :: Lens' GetBucketNotificationResponse (Maybe TopicConfiguration)
gbnoTopicConfiguration f x =
    f (_gbnoTopicConfiguration x)
        <&> \y -> x { _gbnoTopicConfiguration = y }
{-# INLINE gbnoTopicConfiguration #-}

instance FromXML GetBucketNotificationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationResponse

    request = get
    response _ = xmlResponse
