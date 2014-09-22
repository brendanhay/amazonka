{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return the notification configuration of a bucket.
module Network.AWS.S3.GetBucketNotification
    (
    -- * Request
      GetBucketNotification
    -- ** Request constructor
    , getBucketNotification
    -- ** Request lenses
    , gbnBucket

    -- * Response
    , GetBucketNotificationResponse
    -- ** Response constructor
    , getBucketNotificationResponse
    -- ** Response lenses
    , gbnrTopicConfiguration
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype GetBucketNotification = GetBucketNotification
    { _gbnBucket :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketNotification' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Text@
--
getBucketNotification :: Text -- ^ 'gbnBucket'
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { _gbnBucket = p1
    }

gbnBucket :: Lens' GetBucketNotification Text
gbnBucket = lens _gbnBucket (\s a -> s { _gbnBucket = a })

instance ToPath GetBucketNotification

instance ToQuery GetBucketNotification

instance ToHeaders GetBucketNotification

instance ToBody GetBucketNotification

newtype GetBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnrTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetBucketNotificationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicConfiguration ::@ @Maybe TopicConfiguration@
--
getBucketNotificationResponse :: GetBucketNotificationResponse
getBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnrTopicConfiguration = Nothing
    }

gbnrTopicConfiguration :: Lens' GetBucketNotificationResponse (Maybe TopicConfiguration)
gbnrTopicConfiguration =
    lens _gbnrTopicConfiguration (\s a -> s { _gbnrTopicConfiguration = a })

instance FromXML GetBucketNotificationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationResponse

    request = get
    response _ = xmlResponse
