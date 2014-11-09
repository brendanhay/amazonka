{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    { _gbnBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetBucketNotification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbnBucket' @::@ 'Text'
--
getBucketNotification :: Text -- ^ 'gbnBucket'
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { _gbnBucket = p1
    }

gbnBucket :: Lens' GetBucketNotification Text
gbnBucket = lens _gbnBucket (\s a -> s { _gbnBucket = a })

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toText _gbnBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery = const "notification"

instance ToHeaders GetBucketNotification

newtype GetBucketNotificationOutput = GetBucketNotificationOutput
    { _gbnoTopicConfiguration :: Maybe TopicConfiguration
    } deriving (Eq, Show, Generic)

-- | 'GetBucketNotificationOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbnoTopicConfiguration' @::@ 'Maybe' 'TopicConfiguration'
--
getBucketNotificationOutput :: GetBucketNotificationOutput
getBucketNotificationOutput = GetBucketNotificationOutput
    { _gbnoTopicConfiguration = Nothing
    }

gbnoTopicConfiguration :: Lens' GetBucketNotificationOutput (Maybe TopicConfiguration)
gbnoTopicConfiguration =
    lens _gbnoTopicConfiguration (\s a -> s { _gbnoTopicConfiguration = a })

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationOutput

    request  = get
    response = const . xmlResponse $ \h x -> GetBucketNotificationOutput
        <$> x %| "TopicConfiguration"
