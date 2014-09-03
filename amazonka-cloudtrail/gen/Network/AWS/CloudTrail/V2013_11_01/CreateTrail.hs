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
    , createTrail
    -- ** Request lenses
    , ctrName
    , ctrS3BucketName
    , ctrIncludeGlobalServiceEvents
    , ctrS3KeyPrefix
    , ctrSnsTopicName

    -- * Response
    , CreateTrailResponse
    -- ** Response lenses
    , ctsIncludeGlobalServiceEvents
    , ctsName
    , ctsS3BucketName
    , ctsS3KeyPrefix
    , ctsSnsTopicName
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateTrail' request.
createTrail :: Text -- ^ 'ctrName'
            -> Text -- ^ 'ctrS3BucketName'
            -> CreateTrail
createTrail p1 p2 = CreateTrail
    { _ctrName = p1
    , _ctrS3BucketName = p2
    , _ctrIncludeGlobalServiceEvents = Nothing
    , _ctrS3KeyPrefix = Nothing
    , _ctrSnsTopicName = Nothing
    }

data CreateTrail = CreateTrail
    { _ctrName :: Text
      -- ^ Specifies the name of the trail.
    , _ctrS3BucketName :: Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _ctrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _ctrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _ctrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
ctrName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateTrail
    -> f CreateTrail
ctrName f x =
    (\y -> x { _ctrName = y })
       <$> f (_ctrName x)
{-# INLINE ctrName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrS3BucketName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateTrail
    -> f CreateTrail
ctrS3BucketName f x =
    (\y -> x { _ctrS3BucketName = y })
       <$> f (_ctrS3BucketName x)
{-# INLINE ctrS3BucketName #-}

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctrIncludeGlobalServiceEvents
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateTrail
    -> f CreateTrail
ctrIncludeGlobalServiceEvents f x =
    (\y -> x { _ctrIncludeGlobalServiceEvents = y })
       <$> f (_ctrIncludeGlobalServiceEvents x)
{-# INLINE ctrIncludeGlobalServiceEvents #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctrS3KeyPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateTrail
    -> f CreateTrail
ctrS3KeyPrefix f x =
    (\y -> x { _ctrS3KeyPrefix = y })
       <$> f (_ctrS3KeyPrefix x)
{-# INLINE ctrS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctrSnsTopicName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateTrail
    -> f CreateTrail
ctrSnsTopicName f x =
    (\y -> x { _ctrSnsTopicName = y })
       <$> f (_ctrSnsTopicName x)
{-# INLINE ctrSnsTopicName #-}

instance ToPath CreateTrail

instance ToQuery CreateTrail

instance ToHeaders CreateTrail

instance ToJSON CreateTrail

data CreateTrailResponse = CreateTrailResponse
    { _ctsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _ctsName :: Maybe Text
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
    } deriving (Show, Generic)

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctsIncludeGlobalServiceEvents
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateTrailResponse
    -> f CreateTrailResponse
ctsIncludeGlobalServiceEvents f x =
    (\y -> x { _ctsIncludeGlobalServiceEvents = y })
       <$> f (_ctsIncludeGlobalServiceEvents x)
{-# INLINE ctsIncludeGlobalServiceEvents #-}

-- | Specifies the name of the trail.
ctsName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateTrailResponse
    -> f CreateTrailResponse
ctsName f x =
    (\y -> x { _ctsName = y })
       <$> f (_ctsName x)
{-# INLINE ctsName #-}

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctsS3BucketName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateTrailResponse
    -> f CreateTrailResponse
ctsS3BucketName f x =
    (\y -> x { _ctsS3BucketName = y })
       <$> f (_ctsS3BucketName x)
{-# INLINE ctsS3BucketName #-}

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctsS3KeyPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateTrailResponse
    -> f CreateTrailResponse
ctsS3KeyPrefix f x =
    (\y -> x { _ctsS3KeyPrefix = y })
       <$> f (_ctsS3KeyPrefix x)
{-# INLINE ctsS3KeyPrefix #-}

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctsSnsTopicName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateTrailResponse
    -> f CreateTrailResponse
ctsSnsTopicName f x =
    (\y -> x { _ctsSnsTopicName = y })
       <$> f (_ctsSnsTopicName x)
{-# INLINE ctsSnsTopicName #-}

instance FromJSON CreateTrailResponse

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request = get
    response _ = jsonResponse
