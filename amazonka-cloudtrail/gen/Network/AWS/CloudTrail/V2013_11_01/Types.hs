{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | CloudTrail is a web service that records AWS API calls for your AWS account
-- and delivers log files to an Amazon S3 bucket. The recorded information
-- includes the identity of the user, the start time of the AWS API call, the
-- source IP address, the request parameters, and the response elements
-- returned by the service.
module Network.AWS.CloudTrail.V2013_11_01.Types
    (
    -- * Service
      CloudTrail
    -- ** Errors
    , Er (..)
    -- * Trail
    , Trail (..)
    , tmName
    , tmS3BucketName
    , tmS3KeyPrefix
    , tmSnsTopicName
    , tmIncludeGlobalServiceEvents

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-11-01@) of the
-- @AWS CloudTrail@ service.
data CloudTrail deriving (Typeable)

instance AWSService CloudTrail where
    type Sg CloudTrail = V4
    data Er CloudTrail
        = CloudTrailClient HttpException
        | CloudTrailSerializer String
        | CloudTrailService String
        | InsufficientS3BucketPolicyException
        | InsufficientSnsTopicPolicyException
        | InvalidS3BucketNameException
        | InvalidS3PrefixException
        | InvalidSnsTopicNameException
        | InvalidTrailNameException
        | MaximumNumberOfTrailsExceededException
        | S3BucketDoesNotExistException
        | TrailAlreadyExistsException
        | TrailNotFoundException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudtrail"
        , _svcVersion  = "2013-11-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudTrail)
deriving instance Generic (Er CloudTrail)

instance AWSError (Er CloudTrail) where
    awsError = const "CloudTrailError"

instance AWSServiceError (Er CloudTrail) where
    serviceError    = CloudTrailService
    clientError     = CloudTrailClient
    serializerError = CloudTrailSerializer

instance Exception (Er CloudTrail)

-- | The settings for a trail.
data Trail = Trail
    { _tmName :: Maybe Text
      -- ^ Name of the trail set by calling CreateTrail.
    , _tmS3BucketName :: Maybe Text
      -- ^ Name of the Amazon S3 bucket into which CloudTrail delivers your
      -- trail files.
    , _tmS3KeyPrefix :: Maybe Text
      -- ^ Value of the Amazon S3 prefix.
    , _tmSnsTopicName :: Maybe Text
      -- ^ Name of the existing Amazon SNS topic that CloudTrail uses to
      -- notify the account owner when new CloudTrail log files have been
      -- delivered.
    , _tmIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Set to True to include AWS API calls from AWS global services
      -- such as IAM. Otherwise, False.
    } deriving (Show, Generic)

-- | Name of the trail set by calling CreateTrail.
tmName :: Lens' Trail (Maybe Text)
tmName f x =
    f (_tmName x)
        <&> \y -> x { _tmName = y }
{-# INLINE tmName #-}

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files.
tmS3BucketName :: Lens' Trail (Maybe Text)
tmS3BucketName f x =
    f (_tmS3BucketName x)
        <&> \y -> x { _tmS3BucketName = y }
{-# INLINE tmS3BucketName #-}

-- | Value of the Amazon S3 prefix.
tmS3KeyPrefix :: Lens' Trail (Maybe Text)
tmS3KeyPrefix f x =
    f (_tmS3KeyPrefix x)
        <&> \y -> x { _tmS3KeyPrefix = y }
{-# INLINE tmS3KeyPrefix #-}

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered.
tmSnsTopicName :: Lens' Trail (Maybe Text)
tmSnsTopicName f x =
    f (_tmSnsTopicName x)
        <&> \y -> x { _tmSnsTopicName = y }
{-# INLINE tmSnsTopicName #-}

-- | Set to True to include AWS API calls from AWS global services such as IAM.
-- Otherwise, False.
tmIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tmIncludeGlobalServiceEvents f x =
    f (_tmIncludeGlobalServiceEvents x)
        <&> \y -> x { _tmIncludeGlobalServiceEvents = y }
{-# INLINE tmIncludeGlobalServiceEvents #-}

instance FromJSON Trail
