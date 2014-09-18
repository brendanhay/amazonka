{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.Types
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
module Network.AWS.CloudTrail.Types
    (
    -- * Service
      CloudTrail
    -- ** Errors
    , CloudTrailError (..)
    , _CloudTrailClient
    , _CloudTrailSerializer
    , _CloudTrailService
    , _InsufficientS3BucketPolicyException
    , _InsufficientSnsTopicPolicyException
    , _InvalidS3BucketNameException
    , _InvalidS3PrefixException
    , _InvalidSnsTopicNameException
    , _InvalidTrailNameException
    , _MaximumNumberOfTrailsExceededException
    , _S3BucketDoesNotExistException
    , _TrailAlreadyExistsException
    , _TrailNotFoundException
    -- * Trail
    , Trail
    , trail
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
    type Er CloudTrail = CloudTrailError

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "cloudtrail"
        , _svcVersion  = "2013-11-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'CloudTrail' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data CloudTrailError
    = CloudTrailClient HttpException
    | CloudTrailSerializer String
    | CloudTrailService String
      -- | This exception is thrown when the policy on the S3 bucket is not
      -- sufficient.
    | InsufficientS3BucketPolicyException
      -- | This exception is thrown when the policy on the SNS topic is not
      -- sufficient.
    | InsufficientSnsTopicPolicyException
      -- | This exception is thrown when the provided S3 bucket name is not
      -- valid.
    | InvalidS3BucketNameException
      -- | This exception is thrown when the provided S3 prefix is not
      -- valid.
    | InvalidS3PrefixException
      -- | This exception is thrown when the provided SNS topic name is not
      -- valid.
    | InvalidSnsTopicNameException
      -- | This exception is thrown when the provided trail name is not
      -- valid.
    | InvalidTrailNameException
      -- | This exception is thrown when the maximum number of trails is
      -- reached.
    | MaximumNumberOfTrailsExceededException
      -- | This exception is thrown when the specified S3 bucket does not
      -- exist.
    | S3BucketDoesNotExistException
      -- | This exception is thrown when the specified trail already exists.
    | TrailAlreadyExistsException
      -- | This exception is thrown when the trail with the given name is
      -- not found.
    | TrailNotFoundException
      deriving (Show, Typeable, Generic)

instance AWSError CloudTrailError where
    awsError = const "CloudTrailError"

instance AWSServiceError CloudTrailError where
    serviceError    = CloudTrailService
    clientError     = CloudTrailClient
    serializerError = CloudTrailSerializer

instance Exception CloudTrailError

-- | See: 'CloudTrailClient'
_CloudTrailClient :: Prism' CloudTrailError HttpException
_CloudTrailClient = prism
    CloudTrailClient
    (\case
        CloudTrailClient p1 -> Right p1
        x -> Left x)

-- | See: 'CloudTrailSerializer'
_CloudTrailSerializer :: Prism' CloudTrailError String
_CloudTrailSerializer = prism
    CloudTrailSerializer
    (\case
        CloudTrailSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'CloudTrailService'
_CloudTrailService :: Prism' CloudTrailError String
_CloudTrailService = prism
    CloudTrailService
    (\case
        CloudTrailService p1 -> Right p1
        x -> Left x)

-- | This exception is thrown when the policy on the S3 bucket is not
-- sufficient.
--
-- See: 'InsufficientS3BucketPolicyException'
_InsufficientS3BucketPolicyException :: Prism' CloudTrailError ()
_InsufficientS3BucketPolicyException = prism
    (const InsufficientS3BucketPolicyException)
    (\case
        InsufficientS3BucketPolicyException -> Right ()
        x -> Left x)

-- | This exception is thrown when the policy on the SNS topic is not
-- sufficient.
--
-- See: 'InsufficientSnsTopicPolicyException'
_InsufficientSnsTopicPolicyException :: Prism' CloudTrailError ()
_InsufficientSnsTopicPolicyException = prism
    (const InsufficientSnsTopicPolicyException)
    (\case
        InsufficientSnsTopicPolicyException -> Right ()
        x -> Left x)

-- | This exception is thrown when the provided S3 bucket name is not valid.
--
-- See: 'InvalidS3BucketNameException'
_InvalidS3BucketNameException :: Prism' CloudTrailError ()
_InvalidS3BucketNameException = prism
    (const InvalidS3BucketNameException)
    (\case
        InvalidS3BucketNameException -> Right ()
        x -> Left x)

-- | This exception is thrown when the provided S3 prefix is not valid.
--
-- See: 'InvalidS3PrefixException'
_InvalidS3PrefixException :: Prism' CloudTrailError ()
_InvalidS3PrefixException = prism
    (const InvalidS3PrefixException)
    (\case
        InvalidS3PrefixException -> Right ()
        x -> Left x)

-- | This exception is thrown when the provided SNS topic name is not valid.
--
-- See: 'InvalidSnsTopicNameException'
_InvalidSnsTopicNameException :: Prism' CloudTrailError ()
_InvalidSnsTopicNameException = prism
    (const InvalidSnsTopicNameException)
    (\case
        InvalidSnsTopicNameException -> Right ()
        x -> Left x)

-- | This exception is thrown when the provided trail name is not valid.
--
-- See: 'InvalidTrailNameException'
_InvalidTrailNameException :: Prism' CloudTrailError ()
_InvalidTrailNameException = prism
    (const InvalidTrailNameException)
    (\case
        InvalidTrailNameException -> Right ()
        x -> Left x)

-- | This exception is thrown when the maximum number of trails is reached.
--
-- See: 'MaximumNumberOfTrailsExceededException'
_MaximumNumberOfTrailsExceededException :: Prism' CloudTrailError ()
_MaximumNumberOfTrailsExceededException = prism
    (const MaximumNumberOfTrailsExceededException)
    (\case
        MaximumNumberOfTrailsExceededException -> Right ()
        x -> Left x)

-- | This exception is thrown when the specified S3 bucket does not exist.
--
-- See: 'S3BucketDoesNotExistException'
_S3BucketDoesNotExistException :: Prism' CloudTrailError ()
_S3BucketDoesNotExistException = prism
    (const S3BucketDoesNotExistException)
    (\case
        S3BucketDoesNotExistException -> Right ()
        x -> Left x)

-- | This exception is thrown when the specified trail already exists.
--
-- See: 'TrailAlreadyExistsException'
_TrailAlreadyExistsException :: Prism' CloudTrailError ()
_TrailAlreadyExistsException = prism
    (const TrailAlreadyExistsException)
    (\case
        TrailAlreadyExistsException -> Right ()
        x -> Left x)

-- | This exception is thrown when the trail with the given name is not found.
--
-- See: 'TrailNotFoundException'
_TrailNotFoundException :: Prism' CloudTrailError ()
_TrailNotFoundException = prism
    (const TrailNotFoundException)
    (\case
        TrailNotFoundException -> Right ()
        x -> Left x)

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
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @S3BucketName ::@ @Maybe Text@
--
-- * @S3KeyPrefix ::@ @Maybe Text@
--
-- * @SnsTopicName ::@ @Maybe Text@
--
-- * @IncludeGlobalServiceEvents ::@ @Maybe Bool@
--
trail :: Trail
trail = Trail
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
