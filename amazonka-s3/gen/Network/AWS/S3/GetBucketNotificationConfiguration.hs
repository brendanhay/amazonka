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

-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
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

-- | Returns the notification configuration of a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketNotificationConfiguration.html>
module Network.AWS.S3.GetBucketNotificationConfiguration
    (
    -- * Request
      GetBucketNotificationConfiguration
    -- ** Request constructor
    , getBucketNotificationConfiguration
    -- ** Request lenses
    , gbnc1Bucket

    -- * Response
    , GetBucketNotificationConfigurationResponse
    -- ** Response constructor
    , getBucketNotificationConfigurationResponse
    -- ** Response lenses
    , gbncrLambdaFunctionConfigurations
    , gbncrQueueConfigurations
    , gbncrTopicConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketNotificationConfiguration = GetBucketNotificationConfiguration
    { _gbnc1Bucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetBucketNotificationConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbnc1Bucket' @::@ 'Text'
--
getBucketNotificationConfiguration :: Text -- ^ 'gbnc1Bucket'
                                   -> GetBucketNotificationConfiguration
getBucketNotificationConfiguration p1 = GetBucketNotificationConfiguration
    { _gbnc1Bucket = p1
    }

-- | Name of the buket to get the notification configuration for.
gbnc1Bucket :: Lens' GetBucketNotificationConfiguration Text
gbnc1Bucket = lens _gbnc1Bucket (\s a -> s { _gbnc1Bucket = a })

data GetBucketNotificationConfigurationResponse = GetBucketNotificationConfigurationResponse
    { _gbncrLambdaFunctionConfigurations :: List "CloudFunctionConfiguration" LambdaFunctionConfiguration
    , _gbncrQueueConfigurations          :: List "QueueConfiguration" QueueConfiguration
    , _gbncrTopicConfigurations          :: List "TopicConfiguration" TopicConfiguration
    } deriving (Eq, Read, Show)

-- | 'GetBucketNotificationConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbncrLambdaFunctionConfigurations' @::@ ['LambdaFunctionConfiguration']
--
-- * 'gbncrQueueConfigurations' @::@ ['QueueConfiguration']
--
-- * 'gbncrTopicConfigurations' @::@ ['TopicConfiguration']
--
getBucketNotificationConfigurationResponse :: GetBucketNotificationConfigurationResponse
getBucketNotificationConfigurationResponse = GetBucketNotificationConfigurationResponse
    { _gbncrTopicConfigurations          = mempty
    , _gbncrQueueConfigurations          = mempty
    , _gbncrLambdaFunctionConfigurations = mempty
    }

gbncrLambdaFunctionConfigurations :: Lens' GetBucketNotificationConfigurationResponse [LambdaFunctionConfiguration]
gbncrLambdaFunctionConfigurations =
    lens _gbncrLambdaFunctionConfigurations
        (\s a -> s { _gbncrLambdaFunctionConfigurations = a })
            . _List

gbncrQueueConfigurations :: Lens' GetBucketNotificationConfigurationResponse [QueueConfiguration]
gbncrQueueConfigurations =
    lens _gbncrQueueConfigurations
        (\s a -> s { _gbncrQueueConfigurations = a })
            . _List

gbncrTopicConfigurations :: Lens' GetBucketNotificationConfigurationResponse [TopicConfiguration]
gbncrTopicConfigurations =
    lens _gbncrTopicConfigurations
        (\s a -> s { _gbncrTopicConfigurations = a })
            . _List

instance ToPath GetBucketNotificationConfiguration where
    toPath GetBucketNotificationConfiguration{..} = mconcat
        [ "/"
        , toText _gbnc1Bucket
        ]

instance ToQuery GetBucketNotificationConfiguration where
    toQuery = const "notification"

instance ToHeaders GetBucketNotificationConfiguration

instance ToXMLRoot GetBucketNotificationConfiguration where
    toXMLRoot = const (namespaced ns "GetBucketNotificationConfiguration" [])

instance ToXML GetBucketNotificationConfiguration

instance AWSRequest GetBucketNotificationConfiguration where
    type Sv GetBucketNotificationConfiguration = S3
    type Rs GetBucketNotificationConfiguration = GetBucketNotificationConfigurationResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketNotificationConfigurationResponse where
    parseXML x = GetBucketNotificationConfigurationResponse
        <$> parseXML x
        <*> parseXML x
        <*> parseXML x
