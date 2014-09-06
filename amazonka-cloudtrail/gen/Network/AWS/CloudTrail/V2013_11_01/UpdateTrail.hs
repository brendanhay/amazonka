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
    -- ** Response lenses
    , utrsName
    , utrsS3BucketName
    , utrsS3KeyPrefix
    , utrsSnsTopicName
    , utrsIncludeGlobalServiceEvents
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

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
{-# INLINE mkUpdateTrail #-}

-- | Specifies the name of the trail.
utName :: Lens' UpdateTrail Text
utName = lens _utName (\s a -> s { _utName = a })
{-# INLINE utName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utS3BucketName :: Lens' UpdateTrail (Maybe Text)
utS3BucketName = lens _utS3BucketName (\s a -> s { _utS3BucketName = a })
{-# INLINE utS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utS3KeyPrefix = lens _utS3KeyPrefix (\s a -> s { _utS3KeyPrefix = a })
{-# INLINE utS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utSnsTopicName :: Lens' UpdateTrail (Maybe Text)
utSnsTopicName = lens _utSnsTopicName (\s a -> s { _utSnsTopicName = a })
{-# INLINE utSnsTopicName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utIncludeGlobalServiceEvents =
    lens _utIncludeGlobalServiceEvents
         (\s a -> s { _utIncludeGlobalServiceEvents = a })
{-# INLINE utIncludeGlobalServiceEvents #-}

instance ToPath UpdateTrail

instance ToQuery UpdateTrail

instance ToHeaders UpdateTrail

instance ToJSON UpdateTrail

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data UpdateTrailResponse = UpdateTrailResponse
    { _utrsName :: Maybe Text
    , _utrsS3BucketName :: Maybe Text
    , _utrsS3KeyPrefix :: Maybe Text
    , _utrsSnsTopicName :: Maybe Text
    , _utrsIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
utrsName :: Lens' UpdateTrailResponse (Maybe Text)
utrsName = lens _utrsName (\s a -> s { _utrsName = a })
{-# INLINE utrsName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrsS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utrsS3BucketName =
    lens _utrsS3BucketName (\s a -> s { _utrsS3BucketName = a })
{-# INLINE utrsS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utrsS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utrsS3KeyPrefix = lens _utrsS3KeyPrefix (\s a -> s { _utrsS3KeyPrefix = a })
{-# INLINE utrsS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utrsSnsTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utrsSnsTopicName =
    lens _utrsSnsTopicName (\s a -> s { _utrsSnsTopicName = a })
{-# INLINE utrsSnsTopicName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utrsIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utrsIncludeGlobalServiceEvents =
    lens _utrsIncludeGlobalServiceEvents
         (\s a -> s { _utrsIncludeGlobalServiceEvents = a })
{-# INLINE utrsIncludeGlobalServiceEvents #-}

instance FromJSON UpdateTrailResponse

instance AWSRequest UpdateTrail where
    type Sv UpdateTrail = CloudTrail
    type Rs UpdateTrail = UpdateTrailResponse

    request = get
    response _ = jsonResponse
