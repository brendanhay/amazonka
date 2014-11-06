{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , gbnrBucket

    -- * Response
    , GetBucketNotificationOutput
    -- ** Response constructor
    , getBucketNotificationOutput
    -- ** Response lenses
    , gbnoTopicConfiguration
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketNotification = GetBucketNotification
    { _gbnrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketNotification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbnrBucket' @::@ 'BucketName'
--
getBucketNotification :: BucketName -- ^ 'gbnrBucket'
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { _gbnrBucket = p1
    }

gbnrBucket :: Lens' GetBucketNotification BucketName
gbnrBucket = lens _gbnrBucket (\s a -> s { _gbnrBucket = a })

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toText _gbnrBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery = const "notification"

instance ToHeaders GetBucketNotification

newtype GetBucketNotificationOutput = GetBucketNotificationOutput
    { _gbnoTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "TopicConfiguration"
