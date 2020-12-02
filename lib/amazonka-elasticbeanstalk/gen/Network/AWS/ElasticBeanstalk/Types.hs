{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , _PlatformVersionStillReferencedException
    , _TooManyApplicationVersionsException
    , _TooManyConfigurationTemplatesException
    , _ResourceTypeNotSupportedException
    , _InsufficientPrivilegesException
    , _ElasticBeanstalkServiceException
    , _TooManyTagsException
    , _TooManyApplicationsException
    , _TooManyPlatformsException
    , _ManagedActionInvalidStateException
    , _SourceBundleDeletionException
    , _S3LocationNotInServiceRegionException
    , _CodeBuildNotInServiceRegionException
    , _TooManyEnvironmentsException
    , _ResourceNotFoundException

    -- * ActionHistoryStatus
    , ActionHistoryStatus (..)

    -- * ActionStatus
    , ActionStatus (..)

    -- * ActionType
    , ActionType (..)

    -- * ApplicationVersionStatus
    , ApplicationVersionStatus (..)

    -- * ComputeType
    , ComputeType (..)

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

    -- * FailureType
    , FailureType (..)

    -- * InstancesHealthAttribute
    , InstancesHealthAttribute (..)

    -- * PlatformStatus
    , PlatformStatus (..)

    -- * SourceRepository
    , SourceRepository (..)

    -- * SourceType
    , SourceType (..)

    -- * ValidationSeverity
    , ValidationSeverity (..)

    -- * ApplicationDescription
    , ApplicationDescription
    , applicationDescription
    , adApplicationARN
    , adVersions
    , adDateUpdated
    , adDateCreated
    , adApplicationName
    , adConfigurationTemplates
    , adResourceLifecycleConfig
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

    -- * ApplicationResourceLifecycleConfig
    , ApplicationResourceLifecycleConfig
    , applicationResourceLifecycleConfig
    , arlcVersionLifecycleConfig
    , arlcServiceRole

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , applicationVersionDescription
    , avdStatus
    , avdSourceBundle
    , avdDateUpdated
    , avdDateCreated
    , avdVersionLabel
    , avdSourceBuildInformation
    , avdApplicationName
    , avdApplicationVersionARN
    , avdBuildARN
    , avdDescription

    -- * ApplicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage
    , applicationVersionDescriptionMessage
    , avdmApplicationVersion

    -- * ApplicationVersionLifecycleConfig
    , ApplicationVersionLifecycleConfig
    , applicationVersionLifecycleConfig
    , avlcMaxAgeRule
    , avlcMaxCountRule

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgName

    -- * BuildConfiguration
    , BuildConfiguration
    , buildConfiguration
    , bcArtifactName
    , bcComputeType
    , bcTimeoutInMinutes
    , bcCodeBuildServiceRole
    , bcImage

    -- * Builder
    , Builder
    , builder
    , bARN

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
    , csdPlatformARN
    , csdEnvironmentName
    , csdApplicationName
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription

    -- * CustomAMI
    , CustomAMI
    , customAMI
    , caVirtualizationType
    , caImageId

    -- * Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dStatus
    , dDeploymentTime
    , dVersionLabel

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
    , ePlatformARN
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eEnvironmentARN
    , eSolutionStackName
    , eEnvironmentId
    , eHealthStatus
    , eEnvironmentLinks
    , eDescription

    -- * EnvironmentDescriptionsMessage
    , EnvironmentDescriptionsMessage
    , environmentDescriptionsMessage
    , edmNextToken
    , edmEnvironments

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription
    , environmentInfoDescription
    , eidSampleTimestamp
    , eidEC2InstanceId
    , eidInfoType
    , eidMessage

    -- * EnvironmentLink
    , EnvironmentLink
    , environmentLink
    , elLinkName
    , elEnvironmentName

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
    , edPlatformARN
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

    -- * ManagedAction
    , ManagedAction
    , managedAction
    , maStatus
    , maActionId
    , maWindowStartTime
    , maActionDescription
    , maActionType

    -- * ManagedActionHistoryItem
    , ManagedActionHistoryItem
    , managedActionHistoryItem
    , mahiStatus
    , mahiFailureType
    , mahiActionId
    , mahiFailureDescription
    , mahiFinishedTime
    , mahiActionDescription
    , mahiExecutedTime
    , mahiActionType

    -- * MaxAgeRule
    , MaxAgeRule
    , maxAgeRule
    , marDeleteSourceFromS3
    , marMaxAgeInDays
    , marEnabled

    -- * MaxCountRule
    , MaxCountRule
    , maxCountRule
    , mcrMaxCount
    , mcrDeleteSourceFromS3
    , mcrEnabled

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

    -- * PlatformDescription
    , PlatformDescription
    , platformDescription
    , pdSupportedAddonList
    , pdPlatformCategory
    , pdPlatformVersion
    , pdPlatformStatus
    , pdMaintainer
    , pdPlatformOwner
    , pdDateUpdated
    , pdCustomAMIList
    , pdDateCreated
    , pdOperatingSystemName
    , pdFrameworks
    , pdPlatformARN
    , pdOperatingSystemVersion
    , pdProgrammingLanguages
    , pdSolutionStackName
    , pdPlatformName
    , pdDescription
    , pdSupportedTierList

    -- * PlatformFilter
    , PlatformFilter
    , platformFilter
    , pfValues
    , pfOperator
    , pfType

    -- * PlatformFramework
    , PlatformFramework
    , platformFramework
    , pfName
    , pfVersion

    -- * PlatformProgrammingLanguage
    , PlatformProgrammingLanguage
    , platformProgrammingLanguage
    , pplName
    , pplVersion

    -- * PlatformSummary
    , PlatformSummary
    , platformSummary
    , psSupportedAddonList
    , psPlatformCategory
    , psPlatformStatus
    , psPlatformOwner
    , psOperatingSystemName
    , psPlatformARN
    , psOperatingSystemVersion
    , psSupportedTierList

    -- * Queue
    , Queue
    , queue
    , qURL
    , qName

    -- * ResourceQuota
    , ResourceQuota
    , resourceQuota
    , rqMaximum

    -- * ResourceQuotas
    , ResourceQuotas
    , resourceQuotas
    , rqApplicationQuota
    , rqCustomPlatformQuota
    , rqApplicationVersionQuota
    , rqEnvironmentQuota
    , rqConfigurationTemplateQuota

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
    , sihInstanceType
    , sihAvailabilityZone
    , sihHealthStatus
    , sihDeployment
    , sihLaunchedAt

    -- * SolutionStackDescription
    , SolutionStackDescription
    , solutionStackDescription
    , ssdPermittedFileTypes
    , ssdSolutionStackName

    -- * SourceBuildInformation
    , SourceBuildInformation
    , sourceBuildInformation
    , sbiSourceType
    , sbiSourceRepository
    , sbiSourceLocation

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

import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.ElasticBeanstalk.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2010-12-01@ of the Amazon Elastic Beanstalk SDK configuration.
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
    , _svcError = parseXMLError "ElasticBeanstalk"
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
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | One or more input parameters is not valid. Please correct the input parameters and try the operation again.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError elasticBeanstalk "InvalidRequestException" . hasStatus 400


-- | The specified account has reached its limit of Amazon S3 buckets.
--
--
_TooManyBucketsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyBucketsException =
  _MatchServiceError elasticBeanstalk "TooManyBucketsException" . hasStatus 400


-- | The specified account does not have a subscription to Amazon S3.
--
--
_S3SubscriptionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_S3SubscriptionRequiredException =
  _MatchServiceError elasticBeanstalk "S3SubscriptionRequiredException" .
  hasStatus 400


-- | Unable to perform the specified operation because another operation that effects an element in this activity is already in progress.
--
--
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
  _MatchServiceError elasticBeanstalk "OperationInProgressFailure" .
  hasStatus 400


-- | You cannot delete the platform version because there are still environments running on it.
--
--
_PlatformVersionStillReferencedException :: AsError a => Getting (First ServiceError) a ServiceError
_PlatformVersionStillReferencedException =
  _MatchServiceError elasticBeanstalk "PlatformVersionStillReferencedException" .
  hasStatus 400


-- | The specified account has reached its limit of application versions.
--
--
_TooManyApplicationVersionsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyApplicationVersionsException =
  _MatchServiceError elasticBeanstalk "TooManyApplicationVersionsException"


-- | The specified account has reached its limit of configuration templates.
--
--
_TooManyConfigurationTemplatesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyConfigurationTemplatesException =
  _MatchServiceError elasticBeanstalk "TooManyConfigurationTemplatesException" .
  hasStatus 400


-- | The type of the specified Amazon Resource Name (ARN) isn't supported for this operation.
--
--
_ResourceTypeNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceTypeNotSupportedException =
  _MatchServiceError elasticBeanstalk "ResourceTypeNotSupportedException" .
  hasStatus 400


-- | The specified account does not have sufficient privileges for one or more AWS services.
--
--
_InsufficientPrivilegesException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPrivilegesException =
  _MatchServiceError elasticBeanstalk "InsufficientPrivilegesException" .
  hasStatus 403


-- | A generic service exception has occurred.
--
--
_ElasticBeanstalkServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ElasticBeanstalkServiceException =
  _MatchServiceError elasticBeanstalk "ElasticBeanstalkServiceException"


-- | The number of tags in the resource would exceed the number of tags that each resource can have.
--
--
-- To calculate this, the operation considers both the number of tags the resource already has and the tags this operation would add if it succeeded.
--
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException =
  _MatchServiceError elasticBeanstalk "TooManyTagsException" . hasStatus 400


-- | The specified account has reached its limit of applications.
--
--
_TooManyApplicationsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyApplicationsException =
  _MatchServiceError elasticBeanstalk "TooManyApplicationsException" .
  hasStatus 400


-- | You have exceeded the maximum number of allowed platforms associated with the account.
--
--
_TooManyPlatformsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyPlatformsException =
  _MatchServiceError elasticBeanstalk "TooManyPlatformsException" .
  hasStatus 400


-- | Cannot modify the managed action in its current state.
--
--
_ManagedActionInvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_ManagedActionInvalidStateException =
  _MatchServiceError elasticBeanstalk "ManagedActionInvalidStateException" .
  hasStatus 400


-- | Unable to delete the Amazon S3 source bundle associated with the application version. The application version was deleted successfully.
--
--
_SourceBundleDeletionException :: AsError a => Getting (First ServiceError) a ServiceError
_SourceBundleDeletionException =
  _MatchServiceError elasticBeanstalk "SourceBundleDeletionFailure" .
  hasStatus 400


-- | The specified S3 bucket does not belong to the S3 region in which the service is running. The following regions are supported:
--
--
--     * IAD/us-east-1
--
--     * PDX/us-west-2
--
--     * DUB/eu-west-1
--
--
--
_S3LocationNotInServiceRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_S3LocationNotInServiceRegionException =
  _MatchServiceError elasticBeanstalk "S3LocationNotInServiceRegionException" .
  hasStatus 400


-- | AWS CodeBuild is not available in the specified region.
--
--
_CodeBuildNotInServiceRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeBuildNotInServiceRegionException =
  _MatchServiceError elasticBeanstalk "CodeBuildNotInServiceRegionException" .
  hasStatus 400


-- | The specified account has reached its limit of environments.
--
--
_TooManyEnvironmentsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyEnvironmentsException =
  _MatchServiceError elasticBeanstalk "TooManyEnvironmentsException" .
  hasStatus 400


-- | A resource doesn't exist for the specified Amazon Resource Name (ARN).
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError elasticBeanstalk "ResourceNotFoundException" .
  hasStatus 400

