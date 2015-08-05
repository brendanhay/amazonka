{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types
    (
    -- * Service
      ElasticBeanstalk

    -- * Errors
    , _S3SubscriptionRequiredException
    , _TooManyBucketsException
    , _OperationInProgressException
    , _TooManyConfigurationTemplatesException
    , _TooManyApplicationVersionsException
    , _InsufficientPrivilegesException
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

    -- * EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- * EnvironmentStatus
    , EnvironmentStatus (..)

    -- * EventSeverity
    , EventSeverity (..)

    -- * ValidationSeverity
    , ValidationSeverity (..)

    -- * ApplicationDescription
    , ApplicationDescription
    , applicationDescription
    , adDateUpdated
    , adVersions
    , adDateCreated
    , adApplicationName
    , adConfigurationTemplates
    , adDescription

    -- * ApplicationDescriptionMessage
    , ApplicationDescriptionMessage
    , applicationDescriptionMessage
    , admApplication

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , applicationVersionDescription
    , avdDateUpdated
    , avdSourceBundle
    , avdVersionLabel
    , avdDateCreated
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

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription
    , configurationOptionDescription
    , codMaxValue
    , codRegex
    , codUserDefined
    , codMaxLength
    , codValueOptions
    , codNamespace
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
    , cosValue
    , cosNamespace

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
    , eCNAME
    , eStatus
    , eTemplateName
    , eAbortableOperationInProgress
    , eEndpointURL
    , eDateUpdated
    , eResources
    , eHealth
    , eVersionLabel
    , eDateCreated
    , eTier
    , eEnvironmentName
    , eApplicationName
    , eEnvironmentId
    , eSolutionStackName
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
    , erdInstances
    , erdEnvironmentName
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
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2010-12-01@ of the Amazon Elastic Beanstalk SDK.
data ElasticBeanstalk

instance AWSService ElasticBeanstalk where
    type Sg ElasticBeanstalk = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "ElasticBeanstalk"
            , _svcPrefix = "elasticbeanstalk"
            , _svcVersion = "2010-12-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
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

-- | The caller does not have a subscription to Amazon S3.
_S3SubscriptionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_S3SubscriptionRequiredException =
    _ServiceError . hasStatus 400 . hasCode "S3SubscriptionRequiredException"

-- | The web service attempted to create a bucket in an Amazon S3 account
-- that already has 100 buckets.
_TooManyBucketsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyBucketsException =
    _ServiceError . hasStatus 400 . hasCode "TooManyBucketsException"

-- | Unable to perform the specified operation because another operation is
-- already in progress affecting an an element in this activity.
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
    _ServiceError . hasStatus 400 . hasCode "OperationInProgressFailure"

-- | The caller has exceeded the limit on the number of configuration
-- templates associated with their account.
_TooManyConfigurationTemplatesException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyConfigurationTemplatesException =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyConfigurationTemplatesException"

-- | The caller has exceeded the limit on the number of application versions
-- associated with their account.
_TooManyApplicationVersionsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyApplicationVersionsException =
    _ServiceError . hasCode "TooManyApplicationVersionsException"

-- | Unable to perform the specified operation because the user does not have
-- enough privileges for one of more downstream aws services
_InsufficientPrivilegesException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPrivilegesException =
    _ServiceError . hasStatus 403 . hasCode "InsufficientPrivilegesException"

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
