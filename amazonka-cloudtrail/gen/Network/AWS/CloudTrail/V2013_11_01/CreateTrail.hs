{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.CreateTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | From the command line, use create-subscription. Creates a trail that
-- specifies the settings for delivery of log data to an Amazon S3 bucket.
module Network.AWS.CloudTrail.V2013_11_01.CreateTrail
    (
    -- * Request
      CreateTrail
    -- ** Request constructor
    , mkCreateTrailRequest
    -- ** Request lenses
    , ctrName
    , ctrS3BucketName
    , ctrS3KeyPrefix
    , ctrSnsTopicName
    , ctrIncludeGlobalServiceEvents

    -- * Response
    , CreateTrailResponse
    -- ** Response lenses
    , ctsName
    , ctsS3BucketName
    , ctsS3KeyPrefix
    , ctsSnsTopicName
    , ctsIncludeGlobalServiceEvents
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTrail' request.
mkCreateTrailRequest :: Text -- ^ 'ctrName'
                     -> Text -- ^ 'ctrS3BucketName'
                     -> CreateTrail
mkCreateTrailRequest p1 p2 = CreateTrail
    { _ctrName = p1
    , _ctrS3BucketName = p2
    , _ctrS3KeyPrefix = Nothing
    , _ctrSnsTopicName = Nothing
    , _ctrIncludeGlobalServiceEvents = Nothing
    }
{-# INLINE mkCreateTrailRequest #-}

data CreateTrail = CreateTrail
    { _ctrName :: Text
      -- ^ Specifies the name of the trail.
    , _ctrS3BucketName :: Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _ctrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _ctrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _ctrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
ctrName :: Lens' CreateTrail (Text)
ctrName = lens _ctrName (\s a -> s { _ctrName = a })
{-# INLINE ctrName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrS3BucketName :: Lens' CreateTrail (Text)
ctrS3BucketName = lens _ctrS3BucketName (\s a -> s { _ctrS3BucketName = a })
{-# INLINE ctrS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctrS3KeyPrefix :: Lens' CreateTrail (Maybe Text)
ctrS3KeyPrefix = lens _ctrS3KeyPrefix (\s a -> s { _ctrS3KeyPrefix = a })
{-# INLINE ctrS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctrSnsTopicName :: Lens' CreateTrail (Maybe Text)
ctrSnsTopicName = lens _ctrSnsTopicName (\s a -> s { _ctrSnsTopicName = a })
{-# INLINE ctrSnsTopicName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctrIncludeGlobalServiceEvents :: Lens' CreateTrail (Maybe Bool)
ctrIncludeGlobalServiceEvents = lens _ctrIncludeGlobalServiceEvents (\s a -> s { _ctrIncludeGlobalServiceEvents = a })
{-# INLINE ctrIncludeGlobalServiceEvents #-}

instance ToPath CreateTrail

instance ToQuery CreateTrail

instance ToHeaders CreateTrail

instance ToJSON CreateTrail

data CreateTrailResponse = CreateTrailResponse
    { _ctsName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , _ctsS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _ctsS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _ctsSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _ctsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
ctsName :: Lens' CreateTrailResponse (Maybe Text)
ctsName = lens _ctsName (\s a -> s { _ctsName = a })
{-# INLINE ctsName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctsS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctsS3BucketName = lens _ctsS3BucketName (\s a -> s { _ctsS3BucketName = a })
{-# INLINE ctsS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctsS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctsS3KeyPrefix = lens _ctsS3KeyPrefix (\s a -> s { _ctsS3KeyPrefix = a })
{-# INLINE ctsS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctsSnsTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctsSnsTopicName = lens _ctsSnsTopicName (\s a -> s { _ctsSnsTopicName = a })
{-# INLINE ctsSnsTopicName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctsIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctsIncludeGlobalServiceEvents = lens _ctsIncludeGlobalServiceEvents (\s a -> s { _ctsIncludeGlobalServiceEvents = a })
{-# INLINE ctsIncludeGlobalServiceEvents #-}

instance FromJSON CreateTrailResponse

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request = get
    response _ = jsonResponse
