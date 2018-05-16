{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types
    (
    -- * Service Configuration
      greengrass

    -- * Errors
    , _InternalServerErrorException
    , _BadRequestException

    -- * DeploymentType
    , DeploymentType (..)

    -- * EncodingType
    , EncodingType (..)

    -- * LoggerComponent
    , LoggerComponent (..)

    -- * LoggerLevel
    , LoggerLevel (..)

    -- * LoggerType
    , LoggerType (..)

    -- * Permission
    , Permission (..)

    -- * SoftwareToUpdate
    , SoftwareToUpdate (..)

    -- * UpdateAgentLogLevel
    , UpdateAgentLogLevel (..)

    -- * UpdateTargetsArchitecture
    , UpdateTargetsArchitecture (..)

    -- * UpdateTargetsOperatingSystem
    , UpdateTargetsOperatingSystem (..)

    -- * ConnectivityInfo
    , ConnectivityInfo
    , connectivityInfo
    , ciPortNumber
    , ciId
    , ciMetadata
    , ciHostAddress

    -- * Core
    , Core
    , core
    , cCertificateARN
    , cThingARN
    , cSyncShadow
    , cId

    -- * CoreDefinitionVersion
    , CoreDefinitionVersion
    , coreDefinitionVersion
    , cdvCores

    -- * DefinitionInformation
    , DefinitionInformation
    , definitionInformation
    , diLatestVersionARN
    , diARN
    , diName
    , diCreationTimestamp
    , diId
    , diLatestVersion
    , diLastUpdatedTimestamp

    -- * Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dDeploymentARN
    , dCreatedAt
    , dDeploymentType
    , dGroupARN

    -- * Device
    , Device
    , device
    , dCertificateARN
    , dThingARN
    , dSyncShadow
    , dId

    -- * DeviceDefinitionVersion
    , DeviceDefinitionVersion
    , deviceDefinitionVersion
    , ddvDevices

    -- * ErrorDetail
    , ErrorDetail
    , errorDetail
    , edDetailedErrorCode
    , edDetailedErrorMessage

    -- * Function
    , Function
    , function
    , fFunctionARN
    , fFunctionConfiguration
    , fId

    -- * FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcMemorySize
    , fcExecArgs
    , fcEnvironment
    , fcExecutable
    , fcPinned
    , fcEncodingType
    , fcTimeout

    -- * FunctionConfigurationEnvironment
    , FunctionConfigurationEnvironment
    , functionConfigurationEnvironment
    , fceVariables
    , fceResourceAccessPolicies
    , fceAccessSysfs

    -- * FunctionDefinitionVersion
    , FunctionDefinitionVersion
    , functionDefinitionVersion
    , fdvFunctions

    -- * GreengrassLogger
    , GreengrassLogger
    , greengrassLogger
    , glSpace
    , glComponent
    , glId
    , glType
    , glLevel

    -- * GroupCertificateAuthorityProperties
    , GroupCertificateAuthorityProperties
    , groupCertificateAuthorityProperties
    , gcapGroupCertificateAuthorityARN
    , gcapGroupCertificateAuthorityId

    -- * GroupInformation
    , GroupInformation
    , groupInformation
    , giLatestVersionARN
    , giARN
    , giName
    , giCreationTimestamp
    , giId
    , giLatestVersion
    , giLastUpdatedTimestamp

    -- * GroupOwnerSetting
    , GroupOwnerSetting
    , groupOwnerSetting
    , gosAutoAddGroupOwner
    , gosGroupOwner

    -- * GroupVersion
    , GroupVersion
    , groupVersion
    , gvResourceDefinitionVersionARN
    , gvSubscriptionDefinitionVersionARN
    , gvCoreDefinitionVersionARN
    , gvDeviceDefinitionVersionARN
    , gvFunctionDefinitionVersionARN
    , gvLoggerDefinitionVersionARN

    -- * LocalDeviceResourceData
    , LocalDeviceResourceData
    , localDeviceResourceData
    , ldrdGroupOwnerSetting
    , ldrdSourcePath

    -- * LocalVolumeResourceData
    , LocalVolumeResourceData
    , localVolumeResourceData
    , lvrdGroupOwnerSetting
    , lvrdDestinationPath
    , lvrdSourcePath

    -- * LoggerDefinitionVersion
    , LoggerDefinitionVersion
    , loggerDefinitionVersion
    , ldvLoggers

    -- * Resource
    , Resource
    , resource
    , rResourceDataContainer
    , rName
    , rId

    -- * ResourceAccessPolicy
    , ResourceAccessPolicy
    , resourceAccessPolicy
    , rapResourceId
    , rapPermission

    -- * ResourceDataContainer
    , ResourceDataContainer
    , resourceDataContainer
    , rdcS3MachineLearningModelResourceData
    , rdcSageMakerMachineLearningModelResourceData
    , rdcLocalVolumeResourceData
    , rdcLocalDeviceResourceData

    -- * ResourceDefinitionVersion
    , ResourceDefinitionVersion
    , resourceDefinitionVersion
    , rdvResources

    -- * S3MachineLearningModelResourceData
    , S3MachineLearningModelResourceData
    , s3MachineLearningModelResourceData
    , smlmrdDestinationPath
    , smlmrdS3URI

    -- * SageMakerMachineLearningModelResourceData
    , SageMakerMachineLearningModelResourceData
    , sageMakerMachineLearningModelResourceData
    , smmlmrdSageMakerJobARN
    , smmlmrdDestinationPath

    -- * Subscription
    , Subscription
    , subscription
    , sSubject
    , sSource
    , sId
    , sTarget

    -- * SubscriptionDefinitionVersion
    , SubscriptionDefinitionVersion
    , subscriptionDefinitionVersion
    , sdvSubscriptions

    -- * VersionInformation
    , VersionInformation
    , versionInformation
    , viARN
    , viCreationTimestamp
    , viVersion
    , viId
    ) where

import Network.AWS.Greengrass.Types.Product
import Network.AWS.Greengrass.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-06-07@ of the Amazon Greengrass SDK configuration.
greengrass :: Service
greengrass =
  Service
    { _svcAbbrev = "Greengrass"
    , _svcSigner = v4
    , _svcPrefix = "greengrass"
    , _svcVersion = "2017-06-07"
    , _svcEndpoint = defaultEndpoint greengrass
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Greengrass"
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


-- | General error information.
_InternalServerErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerErrorException =
  _MatchServiceError greengrass "InternalServerErrorException" . hasStatus 500


-- | General error information.
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError greengrass "BadRequestException" . hasStatus 400

