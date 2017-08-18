{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types
    (
    -- * Service Configuration
      batch

    -- * Errors
    , _ServerException
    , _ClientException

    -- * CEState
    , CEState (..)

    -- * CEStatus
    , CEStatus (..)

    -- * CEType
    , CEType (..)

    -- * CRType
    , CRType (..)

    -- * JQState
    , JQState (..)

    -- * JQStatus
    , JQStatus (..)

    -- * JobDefinitionType
    , JobDefinitionType (..)

    -- * JobStatus
    , JobStatus (..)

    -- * AttemptContainerDetail
    , AttemptContainerDetail
    , attemptContainerDetail
    , acdTaskARN
    , acdContainerInstanceARN
    , acdReason
    , acdLogStreamName
    , acdExitCode

    -- * AttemptDetail
    , AttemptDetail
    , attemptDetail
    , adStoppedAt
    , adStartedAt
    , adContainer
    , adStatusReason

    -- * ComputeEnvironmentDetail
    , ComputeEnvironmentDetail
    , computeEnvironmentDetail
    , cedStatus
    , cedState
    , cedComputeResources
    , cedStatusReason
    , cedType
    , cedServiceRole
    , cedComputeEnvironmentName
    , cedComputeEnvironmentARN
    , cedEcsClusterARN

    -- * ComputeEnvironmentOrder
    , ComputeEnvironmentOrder
    , computeEnvironmentOrder
    , ceoOrder
    , ceoComputeEnvironment

    -- * ComputeResource
    , ComputeResource
    , computeResource
    , crEc2KeyPair
    , crBidPercentage
    , crSpotIAMFleetRole
    , crImageId
    , crDesiredvCPUs
    , crTags
    , crType
    , crMinvCPUs
    , crMaxvCPUs
    , crInstanceTypes
    , crSubnets
    , crSecurityGroupIds
    , crInstanceRole

    -- * ComputeResourceUpdate
    , ComputeResourceUpdate
    , computeResourceUpdate
    , cruMinvCPUs
    , cruMaxvCPUs
    , cruDesiredvCPUs

    -- * ContainerDetail
    , ContainerDetail
    , containerDetail
    , cdImage
    , cdCommand
    , cdEnvironment
    , cdTaskARN
    , cdUlimits
    , cdContainerInstanceARN
    , cdPrivileged
    , cdJobRoleARN
    , cdMemory
    , cdUser
    , cdReason
    , cdLogStreamName
    , cdMountPoints
    , cdExitCode
    , cdVcpus
    , cdReadonlyRootFilesystem
    , cdVolumes

    -- * ContainerOverrides
    , ContainerOverrides
    , containerOverrides
    , coCommand
    , coEnvironment
    , coMemory
    , coVcpus

    -- * ContainerProperties
    , ContainerProperties
    , containerProperties
    , cpCommand
    , cpEnvironment
    , cpUlimits
    , cpPrivileged
    , cpJobRoleARN
    , cpUser
    , cpMountPoints
    , cpReadonlyRootFilesystem
    , cpVolumes
    , cpImage
    , cpVcpus
    , cpMemory

    -- * Host
    , Host
    , host
    , hSourcePath

    -- * JobDefinition
    , JobDefinition
    , jobDefinition
    , jStatus
    , jRetryStrategy
    , jParameters
    , jContainerProperties
    , jJobDefinitionName
    , jJobDefinitionARN
    , jRevision
    , jType

    -- * JobDependency
    , JobDependency
    , jobDependency
    , jJobId

    -- * JobDetail
    , JobDetail
    , jobDetail
    , jdStoppedAt
    , jdCreatedAt
    , jdRetryStrategy
    , jdAttempts
    , jdDependsOn
    , jdContainer
    , jdParameters
    , jdStatusReason
    , jdJobName
    , jdJobId
    , jdJobQueue
    , jdStatus
    , jdStartedAt
    , jdJobDefinition

    -- * JobQueueDetail
    , JobQueueDetail
    , jobQueueDetail
    , jqdStatus
    , jqdStatusReason
    , jqdJobQueueName
    , jqdJobQueueARN
    , jqdState
    , jqdPriority
    , jqdComputeEnvironmentOrder

    -- * JobSummary
    , JobSummary
    , jobSummary
    , jsJobId
    , jsJobName

    -- * KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpValue
    , kvpName

    -- * MountPoint
    , MountPoint
    , mountPoint
    , mpContainerPath
    , mpSourceVolume
    , mpReadOnly

    -- * RetryStrategy
    , RetryStrategy
    , retryStrategy
    , rsAttempts

    -- * Ulimit
    , Ulimit
    , ulimit
    , uHardLimit
    , uName
    , uSoftLimit

    -- * Volume
    , Volume
    , volume
    , vName
    , vHost
    ) where

import           Network.AWS.Batch.Types.Product
import           Network.AWS.Batch.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2016-08-10@ of the Amazon Batch SDK configuration.
batch :: Service
batch =
    Service
    { _svcAbbrev = "Batch"
    , _svcSigner = v4
    , _svcPrefix = "batch"
    , _svcVersion = "2016-08-10"
    , _svcEndpoint = defaultEndpoint batch
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Batch"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
          Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | These errors are usually caused by a server issue.
--
--
_ServerException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerException = _MatchServiceError batch "ServerException" . hasStatus 500

-- | These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permission to use the action or resource, or specifying an identifier that is not valid.
--
--
_ClientException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientException = _MatchServiceError batch "ClientException" . hasStatus 400
