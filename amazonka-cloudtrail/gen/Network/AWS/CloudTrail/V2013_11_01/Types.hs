{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    -- * Trail
    , Trail
    , mkTrail
    , tName
    , tS3BucketName
    , tS3KeyPrefix
    , tSnsTopicName
    , tIncludeGlobalServiceEvents
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
    { _tName :: Maybe Text
    , _tS3BucketName :: Maybe Text
    , _tS3KeyPrefix :: Maybe Text
    , _tSnsTopicName :: Maybe Text
    , _tIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Trail' data type.
--
-- 'Trail' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
mkTrail :: Trail
mkTrail = Trail
    { _tName = Nothing
    , _tS3BucketName = Nothing
    , _tS3KeyPrefix = Nothing
    , _tSnsTopicName = Nothing
    , _tIncludeGlobalServiceEvents = Nothing
    }

-- | Name of the trail set by calling CreateTrail.
tName :: Lens' Trail (Maybe Text)
tName = lens _tName (\s a -> s { _tName = a })

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail
-- files.
tS3BucketName :: Lens' Trail (Maybe Text)
tS3BucketName = lens _tS3BucketName (\s a -> s { _tS3BucketName = a })

-- | Value of the Amazon S3 prefix.
tS3KeyPrefix :: Lens' Trail (Maybe Text)
tS3KeyPrefix = lens _tS3KeyPrefix (\s a -> s { _tS3KeyPrefix = a })

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered.
tSnsTopicName :: Lens' Trail (Maybe Text)
tSnsTopicName = lens _tSnsTopicName (\s a -> s { _tSnsTopicName = a })

-- | Set to True to include AWS API calls from AWS global services such as IAM.
-- Otherwise, False.
tIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tIncludeGlobalServiceEvents =
    lens _tIncludeGlobalServiceEvents
         (\s a -> s { _tIncludeGlobalServiceEvents = a })

instance FromJSON Trail
