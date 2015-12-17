{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types
    (
    -- * Service Configuration
      elasticBeanstalk

    -- * Errors
    , _InvalidRequestException
    , _TooManyBucketsException
    , _S3SubscriptionRequiredException
    , _OperationInProgressException
    , _TooManyApplicationVersionsException
    , _TooManyConfigurationTemplatesException
    , _InsufficientPrivilegesException
    , _ElasticBeanstalkServiceException
    , _TooManyApplicationsException
    , _SourceBundleDeletionException
    , _S3LocationNotInServiceRegionException
    , _TooManyEnvironmentsException

    -- * ConfigurationDeploymentStatus
    , ConfigurationDeploymentStatus (..)

    -- * ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)

    -- * EnvironmentHealth
    , EnvironmentHealth (..)

    -- * EnvironmentHealthAttribute
    , EnvironmentHealthAttribute (..)

    -- * EnvironmentHealthStatus
    , EnvironmentHealthStatus (..)

    -- * EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- * EnvironmentStatus
    , EnvironmentStatus (..)

    -- * EventSeverity
    , EventSeverity (..)

    -- * InstancesHealthAttribute
    , InstancesHealthAttribute (..)

    -- * ValidationSeverity
    , ValidationSeverity (..)

    -- * ApplicationDescription
    , ApplicationDescription
    , applicationDescription
    , adVersions
    , adDateUpdated
    , adDateCreated
    , adApplicationName
    , adConfigurationTemplates
    , adDescription

    -- * ApplicationDescriptionMessage
    , ApplicationDescriptionMessage
    , applicationDescriptionMessage
    , admApplication

    -- * ApplicationMetrics
    , ApplicationMetrics
    , applicationMetrics
    , amRequestCount
    , amLatency
    , amStatusCodes
    , amDuration

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , applicationVersionDescription
    , avdSourceBundle
    , avdDateUpdated
    , avdDateCreated
    , avdVersionLabel
    , avdApplicationName
    , avdDescription

    -- * ApplicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage
    , applicationVersionDescriptionMessage
    , avdmApplicationVersion

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgName

    -- * CPUUtilization
    , CPUUtilization
    , cpuUtilization
    , cuSoftIRQ
    , cuIdle
    , cuIRQ
    , cuSystem
    , cuUser
    , cuIOWait
    , cuNice

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription
    , configurationOptionDescription
    , codMaxValue
    , codRegex
    , codMaxLength
    , codUserDefined
    , codNamespace
    , codValueOptions
    , codName
    , codChangeSeverity
    , codDefaultValue
    , codValueType
    , codMinValue

    -- * ConfigurationOptionSetting
    , ConfigurationOptionSetting
    , configurationOptionSetting
    , cosOptionName
    , cosResourceName
    , cosNamespace
    , cosValue

    -- * ConfigurationSettingsDescription
    , ConfigurationSettingsDescription
    , configurationSettingsDescription
    , csdTemplateName
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdEnvironmentName
    , csdApplicationName
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription

    -- * EnvironmentDescription
    , EnvironmentDescription
    , environmentDescription
    , eStatus
    , eCNAME
    , eTemplateName
    , eAbortableOperationInProgress
    , eEndpointURL
    , eResources
    , eDateUpdated
    , eDateCreated
    , eHealth
    , eVersionLabel
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eSolutionStackName
    , eEnvironmentId
    , eHealthStatus
    , eDescription

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription
    , environmentInfoDescription
    , eidSampleTimestamp
    , eidEC2InstanceId
    , eidInfoType
    , eidMessage

    -- * EnvironmentResourceDescription
    , EnvironmentResourceDescription
    , environmentResourceDescription
    , erdQueues
    , erdTriggers
    , erdLoadBalancers
    , erdEnvironmentName
    , erdInstances
    , erdLaunchConfigurations
    , erdAutoScalingGroups

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription
    , environmentResourcesDescription
    , erdLoadBalancer

    -- * EnvironmentTier
    , EnvironmentTier
    , environmentTier
    , etName
    , etVersion
    , etType

    -- * EventDescription
    , EventDescription
    , eventDescription
    , edRequestId
    , edTemplateName
    , edSeverity
    , edVersionLabel
    , edEnvironmentName
    , edApplicationName
    , edEventDate
    , edMessage

    -- * Instance
    , Instance
    , instance'
    , iId

    -- * InstanceHealthSummary
    , InstanceHealthSummary
    , instanceHealthSummary
    , ihsOK
    , ihsPending
    , ihsSevere
    , ihsUnknown
    , ihsNoData
    , ihsWarning
    , ihsDegraded
    , ihsInfo

    -- * Latency
    , Latency
    , latency
    , lP75
    , lP50
    , lP85
    , lP999
    , lP90
    , lP95
    , lP99
    , lP10

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcName

    -- * Listener
    , Listener
    , listener
    , lProtocol
    , lPort

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbName

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , loadBalancerDescription
    , lbdLoadBalancerName
    , lbdDomain
    , lbdListeners

    -- * OptionRestrictionRegex
    , OptionRestrictionRegex
    , optionRestrictionRegex
    , orrPattern
    , orrLabel

    -- * OptionSpecification
    , OptionSpecification
    , optionSpecification
    , osOptionName
    , osResourceName
    , osNamespace

    -- * Queue
    , Queue
    , queue
    , qURL
    , qName

    -- * S3Location
    , S3Location
    , s3Location
    , slS3Key
    , slS3Bucket

    -- * SingleInstanceHealth
    , SingleInstanceHealth
    , singleInstanceHealth
    , sihInstanceId
    , sihCauses
    , sihSystem
    , sihApplicationMetrics
    , sihColor
    , sihHealthStatus
    , sihLaunchedAt

    -- * SolutionStackDescription
    , SolutionStackDescription
    , solutionStackDescription
    , ssdPermittedFileTypes
    , ssdSolutionStackName

    -- * SourceConfiguration
    , SourceConfiguration
    , sourceConfiguration
    , scTemplateName
    , scApplicationName

    -- * StatusCodes
    , StatusCodes
    , statusCodes
    , scStatus2xx
    , scStatus3xx
    , scStatus4xx
    , scStatus5xx

    -- * SystemStatus
    , SystemStatus
    , systemStatus
    , ssCPUUtilization
    , ssLoadAverage

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Trigger
    , Trigger
    , trigger
    , tName

    -- * ValidationMessage
    , ValidationMessage
    , validationMessage
    , vmOptionName
    , vmSeverity
    , vmNamespace
    , vmMessage
    ) where

import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.ElasticBeanstalk.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2010-12-01' of the Amazon Elastic Beanstalk SDK configuration.
elasticBeanstalk :: Service
elasticBeanstalk =
    Service
    { _svcAbbrev = "ElasticBeanstalk"
    , _svcSigner = v4
    , _svcPrefix = "elasticbeanstalk"
    , _svcVersion = "2010-12-01"
    , _svcEndpoint = defaultEndpoint elasticBeanstalk
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The request is invalid, please check parameters and their values
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequestException"

-- | The web service attempted to create a bucket in an Amazon S3 account
-- that already has 100 buckets.
_TooManyBucketsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyBucketsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyBucketsException"

-- | The caller does not have a subscription to Amazon S3.
_S3SubscriptionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_S3SubscriptionRequiredException =
    _ServiceError . hasStatus 400 . hasCode "S3SubscriptionRequiredException"

-- | Unable to perform the specified operation because another operation is
-- already in progress affecting an element in this activity.
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
    _ServiceError . hasStatus 400 . hasCode "OperationInProgressFailure"

-- | The caller has exceeded the limit on the number of application versions
-- associated with their account.
_TooManyApplicationVersionsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyApplicationVersionsException =
    _ServiceError . hasCode "TooManyApplicationVersionsException"

-- | The caller has exceeded the limit on the number of configuration
-- templates associated with their account.
_TooManyConfigurationTemplatesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyConfigurationTemplatesException =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyConfigurationTemplatesException"

-- | Unable to perform the specified operation because the user does not have
-- enough privileges for one of more downstream aws services
_InsufficientPrivilegesException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPrivilegesException =
    _ServiceError . hasStatus 403 . hasCode "InsufficientPrivilegesException"

-- | Prism for ElasticBeanstalkServiceException' errors.
_ElasticBeanstalkServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ElasticBeanstalkServiceException =
    _ServiceError . hasCode "ElasticBeanstalkServiceException"

-- | The caller has exceeded the limit on the number of applications
-- associated with their account.
_TooManyApplicationsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyApplicationsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyApplicationsException"

-- | Unable to delete the Amazon S3 source bundle associated with the
-- application version, although the application version deleted
-- successfully.
_SourceBundleDeletionException :: AsError a => Getting (First ServiceError) a ServiceError
_SourceBundleDeletionException =
    _ServiceError . hasStatus 400 . hasCode "SourceBundleDeletionFailure"

-- | The specified S3 bucket does not belong to the S3 region in which the
-- service is running.
_S3LocationNotInServiceRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_S3LocationNotInServiceRegionException =
    _ServiceError .
    hasStatus 400 . hasCode "S3LocationNotInServiceRegionException"

-- | The caller has exceeded the limit of allowed environments associated
-- with the account.
_TooManyEnvironmentsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyEnvironmentsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyEnvironmentsException"
