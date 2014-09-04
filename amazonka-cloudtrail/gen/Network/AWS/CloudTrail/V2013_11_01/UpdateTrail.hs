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
    , mkUpdateTrailRequest
    -- ** Request lenses
    , utrName
    , utrS3BucketName
    , utrS3KeyPrefix
    , utrSnsTopicName
    , utrIncludeGlobalServiceEvents

    -- * Response
    , UpdateTrailResponse
    -- ** Response lenses
    , utsName
    , utsS3BucketName
    , utsS3KeyPrefix
    , utsSnsTopicName
    , utsIncludeGlobalServiceEvents
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateTrail' request.
mkUpdateTrailRequest :: Text -- ^ 'utrName'
                     -> UpdateTrail
mkUpdateTrailRequest p1 = UpdateTrail
    { _utrName = p1
    , _utrS3BucketName = Nothing
    , _utrS3KeyPrefix = Nothing
    , _utrSnsTopicName = Nothing
    , _utrIncludeGlobalServiceEvents = Nothing
    }
{-# INLINE mkUpdateTrailRequest #-}

data UpdateTrail = UpdateTrail
    { _utrName :: Text
      -- ^ Specifies the name of the trail.
    , _utrS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _utrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _utrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _utrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
utrName :: Lens' UpdateTrail (Text)
utrName = lens _utrName (\s a -> s { _utrName = a })
{-# INLINE utrName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrS3BucketName :: Lens' UpdateTrail (Maybe Text)
utrS3BucketName = lens _utrS3BucketName (\s a -> s { _utrS3BucketName = a })
{-# INLINE utrS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utrS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utrS3KeyPrefix = lens _utrS3KeyPrefix (\s a -> s { _utrS3KeyPrefix = a })
{-# INLINE utrS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utrSnsTopicName :: Lens' UpdateTrail (Maybe Text)
utrSnsTopicName = lens _utrSnsTopicName (\s a -> s { _utrSnsTopicName = a })
{-# INLINE utrSnsTopicName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utrIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utrIncludeGlobalServiceEvents = lens _utrIncludeGlobalServiceEvents (\s a -> s { _utrIncludeGlobalServiceEvents = a })
{-# INLINE utrIncludeGlobalServiceEvents #-}

instance ToPath UpdateTrail

instance ToQuery UpdateTrail

instance ToHeaders UpdateTrail

instance ToJSON UpdateTrail

data UpdateTrailResponse = UpdateTrailResponse
    { _utsName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , _utsS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _utsS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _utsSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _utsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
utsName :: Lens' UpdateTrailResponse (Maybe Text)
utsName = lens _utsName (\s a -> s { _utsName = a })
{-# INLINE utsName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utsS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utsS3BucketName = lens _utsS3BucketName (\s a -> s { _utsS3BucketName = a })
{-# INLINE utsS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utsS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utsS3KeyPrefix = lens _utsS3KeyPrefix (\s a -> s { _utsS3KeyPrefix = a })
{-# INLINE utsS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utsSnsTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utsSnsTopicName = lens _utsSnsTopicName (\s a -> s { _utsSnsTopicName = a })
{-# INLINE utsSnsTopicName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utsIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utsIncludeGlobalServiceEvents = lens _utsIncludeGlobalServiceEvents (\s a -> s { _utsIncludeGlobalServiceEvents = a })
{-# INLINE utsIncludeGlobalServiceEvents #-}

instance FromJSON UpdateTrailResponse

instance AWSRequest UpdateTrail where
    type Sv UpdateTrail = CloudTrail
    type Rs UpdateTrail = UpdateTrailResponse

    request = get
    response _ = jsonResponse
