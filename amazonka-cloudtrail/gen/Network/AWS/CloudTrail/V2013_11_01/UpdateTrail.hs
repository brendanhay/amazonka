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
    , updateTrail
    -- ** Request lenses
    , utrName
    , utrIncludeGlobalServiceEvents
    , utrS3BucketName
    , utrS3KeyPrefix
    , utrSnsTopicName

    -- * Response
    , UpdateTrailResponse
    -- ** Response lenses
    , utsIncludeGlobalServiceEvents
    , utsName
    , utsS3BucketName
    , utsS3KeyPrefix
    , utsSnsTopicName
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateTrail' request.
updateTrail :: Text -- ^ 'utrName'
            -> UpdateTrail
updateTrail p1 = UpdateTrail
    { _utrName = p1
    , _utrIncludeGlobalServiceEvents = Nothing
    , _utrS3BucketName = Nothing
    , _utrS3KeyPrefix = Nothing
    , _utrSnsTopicName = Nothing
    }
{-# INLINE updateTrail #-}

data UpdateTrail = UpdateTrail
    { _utrName :: Text
      -- ^ Specifies the name of the trail.
    , _utrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _utrS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _utrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _utrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
utrName :: Lens' UpdateTrail (Text)
utrName f x =
    f (_utrName x)
        <&> \y -> x { _utrName = y }
{-# INLINE utrName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utrIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utrIncludeGlobalServiceEvents f x =
    f (_utrIncludeGlobalServiceEvents x)
        <&> \y -> x { _utrIncludeGlobalServiceEvents = y }
{-# INLINE utrIncludeGlobalServiceEvents #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrS3BucketName :: Lens' UpdateTrail (Maybe Text)
utrS3BucketName f x =
    f (_utrS3BucketName x)
        <&> \y -> x { _utrS3BucketName = y }
{-# INLINE utrS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utrS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utrS3KeyPrefix f x =
    f (_utrS3KeyPrefix x)
        <&> \y -> x { _utrS3KeyPrefix = y }
{-# INLINE utrS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utrSnsTopicName :: Lens' UpdateTrail (Maybe Text)
utrSnsTopicName f x =
    f (_utrSnsTopicName x)
        <&> \y -> x { _utrSnsTopicName = y }
{-# INLINE utrSnsTopicName #-}

instance ToPath UpdateTrail

instance ToQuery UpdateTrail

instance ToHeaders UpdateTrail

instance ToJSON UpdateTrail

data UpdateTrailResponse = UpdateTrailResponse
    { _utsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _utsName :: Maybe Text
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
    } deriving (Show, Generic)

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
utsIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utsIncludeGlobalServiceEvents f x =
    f (_utsIncludeGlobalServiceEvents x)
        <&> \y -> x { _utsIncludeGlobalServiceEvents = y }
{-# INLINE utsIncludeGlobalServiceEvents #-}

-- | Specifies the name of the trail.
utsName :: Lens' UpdateTrailResponse (Maybe Text)
utsName f x =
    f (_utsName x)
        <&> \y -> x { _utsName = y }
{-# INLINE utsName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utsS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utsS3BucketName f x =
    f (_utsS3BucketName x)
        <&> \y -> x { _utsS3BucketName = y }
{-# INLINE utsS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
utsS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utsS3KeyPrefix f x =
    f (_utsS3KeyPrefix x)
        <&> \y -> x { _utsS3KeyPrefix = y }
{-# INLINE utsS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
utsSnsTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utsSnsTopicName f x =
    f (_utsSnsTopicName x)
        <&> \y -> x { _utsSnsTopicName = y }
{-# INLINE utsSnsTopicName #-}

instance FromJSON UpdateTrailResponse

instance AWSRequest UpdateTrail where
    type Sv UpdateTrail = CloudTrail
    type Rs UpdateTrail = UpdateTrailResponse

    request = get
    response _ = jsonResponse
