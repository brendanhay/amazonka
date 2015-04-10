{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketNotificationConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables notifications of specified events for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketNotificationConfiguration.html>
module Network.AWS.S3.PutBucketNotificationConfiguration
    (
    -- * Request
      PutBucketNotificationConfiguration
    -- ** Request constructor
    , putBucketNotificationConfiguration
    -- ** Request lenses
    , pbncBucket
    , pbncNotificationConfiguration

    -- * Response
    , PutBucketNotificationConfigurationResponse
    -- ** Response constructor
    , putBucketNotificationConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketNotificationConfiguration = PutBucketNotificationConfiguration
    { _pbncBucket                    :: Text
    , _pbncNotificationConfiguration :: NotificationConfiguration
    } deriving (Eq, Read, Show)

-- | 'PutBucketNotificationConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbncBucket' @::@ 'Text'
--
-- * 'pbncNotificationConfiguration' @::@ 'NotificationConfiguration'
--
putBucketNotificationConfiguration :: Text -- ^ 'pbncBucket'
                                   -> NotificationConfiguration -- ^ 'pbncNotificationConfiguration'
                                   -> PutBucketNotificationConfiguration
putBucketNotificationConfiguration p1 p2 = PutBucketNotificationConfiguration
    { _pbncBucket                    = p1
    , _pbncNotificationConfiguration = p2
    }

pbncBucket :: Lens' PutBucketNotificationConfiguration Text
pbncBucket = lens _pbncBucket (\s a -> s { _pbncBucket = a })

pbncNotificationConfiguration :: Lens' PutBucketNotificationConfiguration NotificationConfiguration
pbncNotificationConfiguration =
    lens _pbncNotificationConfiguration
        (\s a -> s { _pbncNotificationConfiguration = a })

data PutBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PutBucketNotificationConfigurationResponse' constructor.
putBucketNotificationConfigurationResponse :: PutBucketNotificationConfigurationResponse
putBucketNotificationConfigurationResponse = PutBucketNotificationConfigurationResponse

instance ToPath PutBucketNotificationConfiguration where
    toPath PutBucketNotificationConfiguration{..} = mconcat
        [ "/"
        , toText _pbncBucket
        ]

instance ToQuery PutBucketNotificationConfiguration where
    toQuery = const "notification"

instance ToHeaders PutBucketNotificationConfiguration

instance ToXMLRoot PutBucketNotificationConfiguration where
    toXMLRoot PutBucketNotificationConfiguration{..} = namespaced ns "PutBucketNotificationConfiguration"
        [ "NotificationConfiguration" =@ _pbncNotificationConfiguration
        ]

instance ToXML PutBucketNotificationConfiguration

instance AWSRequest PutBucketNotificationConfiguration where
    type Sv PutBucketNotificationConfiguration = S3
    type Rs PutBucketNotificationConfiguration = PutBucketNotificationConfigurationResponse

    request  = put
    response = nullResponse PutBucketNotificationConfigurationResponse
