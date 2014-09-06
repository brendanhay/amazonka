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
    , mkGetBucketNotification
    -- ** Request lenses
    , gbnBucket

    -- * Response
    , GetBucketNotificationResponse
    -- ** Response lenses
    , gbnrsTopicConfiguration
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketNotification = GetBucketNotification
    { _gbnBucket :: BucketName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketNotification' request.
mkGetBucketNotification :: BucketName -- ^ 'gbnBucket'
                        -> GetBucketNotification
mkGetBucketNotification p1 = GetBucketNotification
    { _gbnBucket = p1
    }
{-# INLINE mkGetBucketNotification #-}

gbnBucket :: Lens' GetBucketNotification BucketName
gbnBucket = lens _gbnBucket (\s a -> s { _gbnBucket = a })
{-# INLINE gbnBucket #-}

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toBS _gbnBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery GetBucketNotification{..} = mconcat
        [ "notification"
        ]

instance ToHeaders GetBucketNotification

instance ToBody GetBucketNotification

newtype GetBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnrsTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Show, Generic)

gbnrsTopicConfiguration :: Lens' GetBucketNotificationResponse (Maybe TopicConfiguration)
gbnrsTopicConfiguration =
    lens _gbnrsTopicConfiguration
         (\s a -> s { _gbnrsTopicConfiguration = a })
{-# INLINE gbnrsTopicConfiguration #-}

instance FromXML GetBucketNotificationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationResponse

    request = get
    response _ = xmlResponse
