{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.UpdateTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | From the command line, use update-subscription. Updates the settings that
-- specify delivery of log files. Changes to a trail do not require stopping
-- the CloudTrail service. Use this action to designate an existing bucket for
-- log delivery. If the existing bucket has previously been a target for
-- CloudTrail log files, an IAM policy exists for the bucket.
module Network.AWS.CloudTrail.V2013_11_01.UpdateTrail
    (
    -- * Request
      UpdateTrail
    -- ** Request constructor
    , mkUpdateTrail
    -- ** Request lenses
    , utName
    , utS3BucketName
    , utS3KeyPrefix
    , utSnsTopicName
    , utIncludeGlobalServiceEvents

    -- * Response
    , UpdateTrailResponse
    -- ** Response constructor
    , mkUpdateTrailResponse
    -- ** Response lenses
    , utrName
    , utrS3BucketName
    , utrS3KeyPrefix
    , utrSnsTopicName
    , utrIncludeGlobalServiceEvents
    ) where

import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Specifies settings to update for the trail.
data UpdateTrail = UpdateTrail
    { _utName :: Text
    , _utS3BucketName :: Maybe Text
    , _utS3KeyPrefix :: Maybe Text
    , _utSnsTopicName :: Maybe Text
    , _utIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateTrail' request.
mkUpdateTrail :: Text -- ^ 'utName'
              -> UpdateTrail
mkUpdateTrail p1 = UpdateTrail
    { _utName = p1
    , _utS3BucketName = Nothing
    , _utS3KeyPrefix = Nothing
    , _utSnsTopicName = Nothing
    , _utIncludeGlobalServiceEvents = Nothing
    }

-- | Specifies the name of the trail.
utName :: Lens' UpdateTrail Text
utName = lens _utName (\s a -> s { _utName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utS3BucketName :: Lens' UpdateTrail (Maybe Text)
utS3BucketName = lens _utS3BucketName (\s a -> s { _utS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utS3KeyPrefix = lens _utS3KeyPrefix (\s a -> s { _utS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utSnsTopicName :: Lens' UpdateTrail (Maybe Text)
utSnsTopicName = lens _utSnsTopicName (\s a -> s { _utSnsTopicName = a })

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utIncludeGlobalServiceEvents =
    lens _utIncludeGlobalServiceEvents
         (\s a -> s { _utIncludeGlobalServiceEvents = a })

instance ToPath UpdateTrail

instance ToQuery UpdateTrail

instance ToHeaders UpdateTrail

instance ToJSON UpdateTrail

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data UpdateTrailResponse = UpdateTrailResponse
    { _utrName :: Maybe Text
    , _utrS3BucketName :: Maybe Text
    , _utrS3KeyPrefix :: Maybe Text
    , _utrSnsTopicName :: Maybe Text
    , _utrIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateTrailResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateTrailResponse :: UpdateTrailResponse
mkUpdateTrailResponse = UpdateTrailResponse
    { _utrName = Nothing
    , _utrS3BucketName = Nothing
    , _utrS3KeyPrefix = Nothing
    , _utrSnsTopicName = Nothing
    , _utrIncludeGlobalServiceEvents = Nothing
    }

-- | Specifies the name of the trail.
utrName :: Lens' UpdateTrailResponse (Maybe Text)
utrName = lens _utrName (\s a -> s { _utrName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utrS3BucketName = lens _utrS3BucketName (\s a -> s { _utrS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utrS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utrS3KeyPrefix = lens _utrS3KeyPrefix (\s a -> s { _utrS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utrSnsTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utrSnsTopicName = lens _utrSnsTopicName (\s a -> s { _utrSnsTopicName = a })

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utrIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utrIncludeGlobalServiceEvents =
    lens _utrIncludeGlobalServiceEvents
         (\s a -> s { _utrIncludeGlobalServiceEvents = a })

instance FromJSON UpdateTrailResponse

instance AWSRequest UpdateTrail where
    type Sv UpdateTrail = CloudTrail
    type Rs UpdateTrail = UpdateTrailResponse

    request = get
    response _ = jsonResponse
