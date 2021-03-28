-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _KMSInvalidStateException
    , _EC2ThrottledException
    , _EFSMountConnectivityException
    , _InvalidRuntimeException
    , _EFSMountFailureException
    , _PolicyLengthExceededException
    , _PreconditionFailedException
    , _EC2AccessDeniedException
    , _InvalidSubnetIDException
    , _CodeVerificationFailedException
    , _UnsupportedMediaTypeException
    , _InvalidRequestContentException
    , _KMSNotFoundException
    , _ENILimitReachedException
    , _InvalidParameterValueException
    , _RequestTooLargeException
    , _InvalidCodeSignatureException
    , _TooManyRequestsException
    , _InvalidSecurityGroupIDException
    , _KMSDisabledException
    , _SubnetIPAddressLimitReachedException
    , _ServiceException
    , _CodeStorageExceededException
    , _CodeSigningConfigNotFoundException
    , _InvalidZipFileException
    , _ProvisionedConcurrencyConfigNotFoundException
    , _ResourceConflictException
    , _ResourceNotReadyException
    , _EC2UnexpectedException
    , _ResourceNotFoundException
    , _EFSIOException
    , _EFSMountTimeoutException
    , _KMSAccessDeniedException
    , _ResourceInUseException

    -- * LayerName
    , LayerName (..)

    -- * MasterRegion
    , MasterRegion (..)

    -- * AccountLimit
    , AccountLimit (..)
    , mkAccountLimit
    , alCodeSizeUnzipped
    , alCodeSizeZipped
    , alConcurrentExecutions
    , alTotalCodeSize
    , alUnreservedConcurrentExecutions

    -- * SourceOwner
    , SourceOwner (..)

    -- * CodeSigningConfig
    , CodeSigningConfig (..)
    , mkCodeSigningConfig
    , cscCodeSigningConfigId
    , cscCodeSigningConfigArn
    , cscAllowedPublishers
    , cscCodeSigningPolicies
    , cscLastModified
    , cscDescription

    -- * GetLayerVersionResponse
    , GetLayerVersionResponse (..)
    , mkGetLayerVersionResponse
    , glvrCompatibleRuntimes
    , glvrContent
    , glvrCreatedDate
    , glvrDescription
    , glvrLayerArn
    , glvrLayerVersionArn
    , glvrLicenseInfo
    , glvrVersion

    -- * EnvironmentVariableName
    , EnvironmentVariableName (..)

    -- * LayerVersionArn
    , LayerVersionArn (..)

    -- * Runtime
    , Runtime (..)

    -- * AccountUsage
    , AccountUsage (..)
    , mkAccountUsage
    , auFunctionCount
    , auTotalCodeSize

    -- * FunctionEventInvokeConfig
    , FunctionEventInvokeConfig (..)
    , mkFunctionEventInvokeConfig
    , feicDestinationConfig
    , feicFunctionArn
    , feicLastModified
    , feicMaximumEventAgeInSeconds
    , feicMaximumRetryAttempts

    -- * State
    , State (..)

    -- * NamespacedStatementId
    , NamespacedStatementId (..)

    -- * TracingMode
    , TracingMode (..)

    -- * LastUpdateStatus
    , LastUpdateStatus (..)

    -- * S3ObjectVersion
    , S3ObjectVersion (..)

    -- * LayersListItem
    , LayersListItem (..)
    , mkLayersListItem
    , lliLatestMatchingVersion
    , lliLayerArn
    , lliLayerName

    -- * S3Key
    , S3Key (..)

    -- * FunctionArn
    , FunctionArn (..)

    -- * KMSKeyArn
    , KMSKeyArn (..)

    -- * EventSourceToken
    , EventSourceToken (..)

    -- * OnSuccess
    , OnSuccess (..)
    , mkOnSuccess
    , osDestination

    -- * LayerPermissionAllowedPrincipal
    , LayerPermissionAllowedPrincipal (..)

    -- * ProvisionedConcurrencyStatusEnum
    , ProvisionedConcurrencyStatusEnum (..)

    -- * TracingConfigResponse
    , TracingConfigResponse (..)
    , mkTracingConfigResponse
    , tcrMode

    -- * Environment
    , Environment (..)
    , mkEnvironment
    , eVariables

    -- * Arn
    , Arn (..)

    -- * AllowedPublishers
    , AllowedPublishers (..)
    , mkAllowedPublishers
    , apSigningProfileVersionArns

    -- * LocalMountPath
    , LocalMountPath (..)

    -- * LayerVersionsListItem
    , LayerVersionsListItem (..)
    , mkLayerVersionsListItem
    , lvliCompatibleRuntimes
    , lvliCreatedDate
    , lvliDescription
    , lvliLayerVersionArn
    , lvliLicenseInfo
    , lvliVersion

    -- * EnvironmentVariableValue
    , EnvironmentVariableValue (..)

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * VpcId
    , VpcId (..)

    -- * ProvisionedConcurrencyConfigListItem
    , ProvisionedConcurrencyConfigListItem (..)
    , mkProvisionedConcurrencyConfigListItem
    , pccliAllocatedProvisionedConcurrentExecutions
    , pccliAvailableProvisionedConcurrentExecutions
    , pccliFunctionArn
    , pccliLastModified
    , pccliRequestedProvisionedConcurrentExecutions
    , pccliStatus
    , pccliStatusReason

    -- * CodeSigningPolicies
    , CodeSigningPolicies (..)
    , mkCodeSigningPolicies
    , cspUntrustedArtifactOnDeployment

    -- * InvocationType
    , InvocationType (..)

    -- * DeadLetterConfig
    , DeadLetterConfig (..)
    , mkDeadLetterConfig
    , dlcTargetArn

    -- * AdditionalVersion
    , AdditionalVersion (..)

    -- * Action
    , Action (..)

    -- * Alias
    , Alias (..)

    -- * LayerVersionContentInput
    , LayerVersionContentInput (..)
    , mkLayerVersionContentInput
    , lvciS3Bucket
    , lvciS3Key
    , lvciS3ObjectVersion
    , lvciZipFile

    -- * FileSystemArn
    , FileSystemArn (..)

    -- * CodeSigningPolicy
    , CodeSigningPolicy (..)

    -- * SubnetId
    , SubnetId (..)

    -- * SourceAccessType
    , SourceAccessType (..)

    -- * TagValue
    , TagValue (..)

    -- * CodeSigningConfigArn
    , CodeSigningConfigArn (..)

    -- * Queue
    , Queue (..)

    -- * SensitiveString
    , SensitiveString (..)

    -- * Topic
    , Topic (..)

    -- * DestinationArn
    , DestinationArn (..)

    -- * FileSystemConfig
    , FileSystemConfig (..)
    , mkFileSystemConfig
    , fscArn
    , fscLocalMountPath

    -- * SecurityGroupId
    , SecurityGroupId (..)

    -- * LogType
    , LogType (..)

    -- * VpcConfigResponse
    , VpcConfigResponse (..)
    , mkVpcConfigResponse
    , vcrSecurityGroupIds
    , vcrSubnetIds
    , vcrVpcId

    -- * VpcConfig
    , VpcConfig (..)
    , mkVpcConfig
    , vcSecurityGroupIds
    , vcSubnetIds

    -- * FunctionCode
    , FunctionCode (..)
    , mkFunctionCode
    , fcS3Bucket
    , fcS3Key
    , fcS3ObjectVersion
    , fcZipFile

    -- * LayerVersionContentOutput
    , LayerVersionContentOutput (..)
    , mkLayerVersionContentOutput
    , lvcoCodeSha256
    , lvcoCodeSize
    , lvcoLocation
    , lvcoSigningJobArn
    , lvcoSigningProfileVersionArn

    -- * Principal
    , Principal (..)

    -- * FunctionCodeLocation
    , FunctionCodeLocation (..)
    , mkFunctionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- * Version
    , Version (..)

    -- * FunctionConfiguration
    , FunctionConfiguration (..)
    , mkFunctionConfiguration
    , fcCodeSha256
    , fcCodeSize
    , fcDeadLetterConfig
    , fcDescription
    , fcEnvironment
    , fcFileSystemConfigs
    , fcFunctionArn
    , fcFunctionName
    , fcHandler
    , fcKMSKeyArn
    , fcLastModified
    , fcLastUpdateStatus
    , fcLastUpdateStatusReason
    , fcLastUpdateStatusReasonCode
    , fcLayers
    , fcMasterArn
    , fcMemorySize
    , fcRevisionId
    , fcRole
    , fcRuntime
    , fcSigningJobArn
    , fcSigningProfileVersionArn
    , fcState
    , fcStateReason
    , fcStateReasonCode
    , fcTimeout
    , fcTracingConfig
    , fcVersion
    , fcVpcConfig

    -- * AliasRoutingConfiguration
    , AliasRoutingConfiguration (..)
    , mkAliasRoutingConfiguration
    , arcAdditionalVersionWeights

    -- * NamespacedFunctionName
    , NamespacedFunctionName (..)

    -- * FunctionName
    , FunctionName (..)

    -- * EnvironmentError
    , EnvironmentError (..)
    , mkEnvironmentError
    , eeErrorCode
    , eeMessage

    -- * EventSourceMappingConfiguration
    , EventSourceMappingConfiguration (..)
    , mkEventSourceMappingConfiguration
    , esmcBatchSize
    , esmcBisectBatchOnFunctionError
    , esmcDestinationConfig
    , esmcEventSourceArn
    , esmcFunctionArn
    , esmcLastModified
    , esmcLastProcessingResult
    , esmcMaximumBatchingWindowInSeconds
    , esmcMaximumRecordAgeInSeconds
    , esmcMaximumRetryAttempts
    , esmcParallelizationFactor
    , esmcQueues
    , esmcSourceAccessConfigurations
    , esmcStartingPosition
    , esmcStartingPositionTimestamp
    , esmcState
    , esmcStateTransitionReason
    , esmcTopics
    , esmcUUID

    -- * Concurrency
    , Concurrency (..)
    , mkConcurrency
    , cReservedConcurrentExecutions

    -- * LicenseInfo
    , LicenseInfo (..)

    -- * TagKey
    , TagKey (..)

    -- * Qualifier
    , Qualifier (..)

    -- * FunctionVersion
    , FunctionVersion (..)

    -- * Handler
    , Handler (..)

    -- * StatementId
    , StatementId (..)

    -- * LastUpdateStatusReason
    , LastUpdateStatusReason (..)

    -- * StateReason
    , StateReason (..)

    -- * Layer
    , Layer (..)
    , mkLayer
    , lArn
    , lCodeSize
    , lSigningJobArn
    , lSigningProfileVersionArn

    -- * LayerArn
    , LayerArn (..)

    -- * TracingConfig
    , TracingConfig (..)
    , mkTracingConfig
    , tcMode

    -- * StateReasonCode
    , StateReasonCode (..)

    -- * Description
    , Description (..)

    -- * LayerPermissionAllowedAction
    , LayerPermissionAllowedAction (..)

    -- * S3Bucket
    , S3Bucket (..)

    -- * LastUpdateStatusReasonCode
    , LastUpdateStatusReasonCode (..)

    -- * AliasConfiguration
    , AliasConfiguration (..)
    , mkAliasConfiguration
    , acAliasArn
    , acDescription
    , acFunctionVersion
    , acName
    , acRevisionId
    , acRoutingConfig

    -- * DestinationConfig
    , DestinationConfig (..)
    , mkDestinationConfig
    , dcOnFailure
    , dcOnSuccess

    -- * EnvironmentResponse
    , EnvironmentResponse (..)
    , mkEnvironmentResponse
    , erError
    , erVariables

    -- * OrganizationId
    , OrganizationId (..)

    -- * OnFailure
    , OnFailure (..)
    , mkOnFailure
    , ofDestination

    -- * SourceAccessConfiguration
    , SourceAccessConfiguration (..)
    , mkSourceAccessConfiguration
    , sacType
    , sacURI

    -- * CodeSigningConfigId
    , CodeSigningConfigId (..)

    -- * LastModified
    , LastModified (..)

    -- * CreatedDate
    , CreatedDate (..)

    -- * Role
    , Role (..)

    -- * Destination
    , Destination (..)

    -- * EventSourceArn
    , EventSourceArn (..)

    -- * TargetArn
    , TargetArn (..)

    -- * Name
    , Name (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Lambda.Types.LayerName
  
import Network.AWS.Lambda.Types.MasterRegion
  
import Network.AWS.Lambda.Types.AccountLimit
  
import Network.AWS.Lambda.Types.SourceOwner
  
import Network.AWS.Lambda.Types.CodeSigningConfig
  
  
import Network.AWS.Lambda.Types.GetLayerVersionResponse
  
import Network.AWS.Lambda.Types.EnvironmentVariableName
  
import Network.AWS.Lambda.Types.LayerVersionArn
  
import Network.AWS.Lambda.Types.Runtime
  
import Network.AWS.Lambda.Types.AccountUsage
  
import Network.AWS.Lambda.Types.FunctionEventInvokeConfig
  
import Network.AWS.Lambda.Types.State
  
import Network.AWS.Lambda.Types.NamespacedStatementId
  
  
import Network.AWS.Lambda.Types.TracingMode
  
import Network.AWS.Lambda.Types.LastUpdateStatus
  
import Network.AWS.Lambda.Types.S3ObjectVersion
  
import Network.AWS.Lambda.Types.LayersListItem
  
import Network.AWS.Lambda.Types.S3Key
  
import Network.AWS.Lambda.Types.FunctionArn
  
  
import Network.AWS.Lambda.Types.KMSKeyArn
  
import Network.AWS.Lambda.Types.EventSourceToken
  
import Network.AWS.Lambda.Types.OnSuccess
  
import Network.AWS.Lambda.Types.LayerPermissionAllowedPrincipal
  
  
import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
  
  
import Network.AWS.Lambda.Types.TracingConfigResponse
  
import Network.AWS.Lambda.Types.Environment
  
import Network.AWS.Lambda.Types.Arn
  
import Network.AWS.Lambda.Types.AllowedPublishers
  
import Network.AWS.Lambda.Types.LocalMountPath
  
import Network.AWS.Lambda.Types.LayerVersionsListItem
  
import Network.AWS.Lambda.Types.EnvironmentVariableValue
  
  
import Network.AWS.Lambda.Types.EventSourcePosition
  
import Network.AWS.Lambda.Types.VpcId
  
import Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
  
  
import Network.AWS.Lambda.Types.CodeSigningPolicies
  
  
import Network.AWS.Lambda.Types.InvocationType
  
import Network.AWS.Lambda.Types.DeadLetterConfig
  
  
  
import Network.AWS.Lambda.Types.AdditionalVersion
  
import Network.AWS.Lambda.Types.Action
  
import Network.AWS.Lambda.Types.Alias
  
import Network.AWS.Lambda.Types.LayerVersionContentInput
  
  
import Network.AWS.Lambda.Types.FileSystemArn
  
  
import Network.AWS.Lambda.Types.CodeSigningPolicy
  
import Network.AWS.Lambda.Types.SubnetId
  
import Network.AWS.Lambda.Types.SourceAccessType
  
import Network.AWS.Lambda.Types.TagValue
  
import Network.AWS.Lambda.Types.CodeSigningConfigArn
  
import Network.AWS.Lambda.Types.Queue
  
  
import Network.AWS.Lambda.Types.SensitiveString
  
import Network.AWS.Lambda.Types.Topic
  
import Network.AWS.Lambda.Types.DestinationArn
  
import Network.AWS.Lambda.Types.FileSystemConfig
  
  
import Network.AWS.Lambda.Types.SecurityGroupId
  
import Network.AWS.Lambda.Types.LogType
  
  
import Network.AWS.Lambda.Types.VpcConfigResponse
  
import Network.AWS.Lambda.Types.VpcConfig
  
import Network.AWS.Lambda.Types.FunctionCode
  
import Network.AWS.Lambda.Types.LayerVersionContentOutput
  
  
  
import Network.AWS.Lambda.Types.Principal
  
  
import Network.AWS.Lambda.Types.FunctionCodeLocation
  
import Network.AWS.Lambda.Types.Version
  
import Network.AWS.Lambda.Types.FunctionConfiguration
  
  
  
  
import Network.AWS.Lambda.Types.AliasRoutingConfiguration
  
  
import Network.AWS.Lambda.Types.NamespacedFunctionName
  
import Network.AWS.Lambda.Types.FunctionName
  
import Network.AWS.Lambda.Types.EnvironmentError
  
import Network.AWS.Lambda.Types.EventSourceMappingConfiguration
  
import Network.AWS.Lambda.Types.Concurrency
  
import Network.AWS.Lambda.Types.LicenseInfo
  
import Network.AWS.Lambda.Types.TagKey
  
import Network.AWS.Lambda.Types.Qualifier
  
import Network.AWS.Lambda.Types.FunctionVersion
  
  
  
import Network.AWS.Lambda.Types.Handler
  
import Network.AWS.Lambda.Types.StatementId
  
import Network.AWS.Lambda.Types.LastUpdateStatusReason
  
  
import Network.AWS.Lambda.Types.StateReason
  
  
  
  
import Network.AWS.Lambda.Types.Layer
  
import Network.AWS.Lambda.Types.LayerArn
  
import Network.AWS.Lambda.Types.TracingConfig
  
import Network.AWS.Lambda.Types.StateReasonCode
  
import Network.AWS.Lambda.Types.Description
  
import Network.AWS.Lambda.Types.LayerPermissionAllowedAction
  
import Network.AWS.Lambda.Types.S3Bucket
  
import Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
  
  
import Network.AWS.Lambda.Types.AliasConfiguration
  
import Network.AWS.Lambda.Types.DestinationConfig
  
import Network.AWS.Lambda.Types.EnvironmentResponse
  
  
import Network.AWS.Lambda.Types.OrganizationId
  
import Network.AWS.Lambda.Types.OnFailure
  
import Network.AWS.Lambda.Types.SourceAccessConfiguration
  
  
  
import Network.AWS.Lambda.Types.CodeSigningConfigId
  
  
  
import Network.AWS.Lambda.Types.LastModified
  
import Network.AWS.Lambda.Types.CreatedDate
  
import Network.AWS.Lambda.Types.Role
  
import Network.AWS.Lambda.Types.Destination
  
import Network.AWS.Lambda.Types.EventSourceArn
  
import Network.AWS.Lambda.Types.TargetArn
  
import Network.AWS.Lambda.Types.Name
  

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Lambda",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "lambda",
                 Core._svcVersion = "2015-03-31", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Lambda",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | Lambda was unable to decrypt the environment variables because the KMS key used is in an invalid state for Decrypt. Check the function's KMS key settings.
_KMSInvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSInvalidStateException
  = Core._MatchServiceError mkServiceConfig
      "KMSInvalidStateException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _KMSInvalidStateException #-}
{-# DEPRECATED _KMSInvalidStateException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Lambda was throttled by Amazon EC2 during Lambda function initialization using the execution role provided for the Lambda function.
_EC2ThrottledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EC2ThrottledException
  = Core._MatchServiceError mkServiceConfig "EC2ThrottledException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _EC2ThrottledException #-}
{-# DEPRECATED _EC2ThrottledException "Use generic-lens or generic-optics instead"  #-}

-- | The function couldn't make a network connection to the configured file system.
_EFSMountConnectivityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSMountConnectivityException
  = Core._MatchServiceError mkServiceConfig
      "EFSMountConnectivityException"
      Core.. Core.hasStatues 408
{-# INLINEABLE _EFSMountConnectivityException #-}
{-# DEPRECATED _EFSMountConnectivityException "Use generic-lens or generic-optics instead"  #-}

-- | The runtime or runtime version specified is not supported.
_InvalidRuntimeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRuntimeException
  = Core._MatchServiceError mkServiceConfig "InvalidRuntimeException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _InvalidRuntimeException #-}
{-# DEPRECATED _InvalidRuntimeException "Use generic-lens or generic-optics instead"  #-}

-- | The function couldn't mount the configured file system due to a permission or configuration issue.
_EFSMountFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSMountFailureException
  = Core._MatchServiceError mkServiceConfig
      "EFSMountFailureException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _EFSMountFailureException #-}
{-# DEPRECATED _EFSMountFailureException "Use generic-lens or generic-optics instead"  #-}

-- | The permissions policy for the resource is too large. <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more> 
_PolicyLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PolicyLengthExceededException
  = Core._MatchServiceError mkServiceConfig
      "PolicyLengthExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PolicyLengthExceededException #-}
{-# DEPRECATED _PolicyLengthExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The RevisionId provided does not match the latest RevisionId for the Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API to retrieve the latest RevisionId for your resource.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException
  = Core._MatchServiceError mkServiceConfig
      "PreconditionFailedException"
      Core.. Core.hasStatues 412
{-# INLINEABLE _PreconditionFailedException #-}
{-# DEPRECATED _PreconditionFailedException "Use generic-lens or generic-optics instead"  #-}

-- | Need additional permissions to configure VPC settings.
_EC2AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EC2AccessDeniedException
  = Core._MatchServiceError mkServiceConfig
      "EC2AccessDeniedException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _EC2AccessDeniedException #-}
{-# DEPRECATED _EC2AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The Subnet ID provided in the Lambda function VPC configuration is invalid.
_InvalidSubnetIDException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnetIDException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSubnetIDException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _InvalidSubnetIDException #-}
{-# DEPRECATED _InvalidSubnetIDException "Use generic-lens or generic-optics instead"  #-}

-- | The code signature failed one or more of the validation checks for signature mismatch or expiry, and the code signing policy is set to ENFORCE. Lambda blocks the deployment. 
_CodeVerificationFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeVerificationFailedException
  = Core._MatchServiceError mkServiceConfig
      "CodeVerificationFailedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CodeVerificationFailedException #-}
{-# DEPRECATED _CodeVerificationFailedException "Use generic-lens or generic-optics instead"  #-}

-- | The content type of the @Invoke@ request body is not JSON.
_UnsupportedMediaTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedMediaTypeException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedMediaTypeException"
      Core.. Core.hasStatues 415
{-# INLINEABLE _UnsupportedMediaTypeException #-}
{-# DEPRECATED _UnsupportedMediaTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The request body could not be parsed as JSON.
_InvalidRequestContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestContentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidRequestContentException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRequestContentException #-}
{-# DEPRECATED _InvalidRequestContentException "Use generic-lens or generic-optics instead"  #-}

-- | Lambda was unable to decrypt the environment variables because the KMS key was not found. Check the function's KMS key settings. 
_KMSNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSNotFoundException
  = Core._MatchServiceError mkServiceConfig "KMSNotFoundException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _KMSNotFoundException #-}
{-# DEPRECATED _KMSNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Lambda was not able to create an elastic network interface in the VPC, specified as part of Lambda function configuration, because the limit for network interfaces has been reached.
_ENILimitReachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ENILimitReachedException
  = Core._MatchServiceError mkServiceConfig
      "ENILimitReachedException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _ENILimitReachedException #-}
{-# DEPRECATED _ENILimitReachedException "Use generic-lens or generic-optics instead"  #-}

-- | One of the parameters in the request is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterValueException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterValueException #-}
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead"  #-}

-- | The request payload exceeded the @Invoke@ request body JSON input limit. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits> . 
_RequestTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestTooLargeException
  = Core._MatchServiceError mkServiceConfig
      "RequestTooLargeException"
      Core.. Core.hasStatues 413
{-# INLINEABLE _RequestTooLargeException #-}
{-# DEPRECATED _RequestTooLargeException "Use generic-lens or generic-optics instead"  #-}

-- | The code signature failed the integrity check. Lambda always blocks deployment if the integrity check fails, even if code signing policy is set to WARN.
_InvalidCodeSignatureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCodeSignatureException
  = Core._MatchServiceError mkServiceConfig
      "InvalidCodeSignatureException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCodeSignatureException #-}
{-# DEPRECATED _InvalidCodeSignatureException "Use generic-lens or generic-optics instead"  #-}

-- | The request throughput limit was exceeded.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | The Security Group ID provided in the Lambda function VPC configuration is invalid.
_InvalidSecurityGroupIDException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSecurityGroupIDException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSecurityGroupIDException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _InvalidSecurityGroupIDException #-}
{-# DEPRECATED _InvalidSecurityGroupIDException "Use generic-lens or generic-optics instead"  #-}

-- | Lambda was unable to decrypt the environment variables because the KMS key used is disabled. Check the Lambda function's KMS key settings.
_KMSDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSDisabledException
  = Core._MatchServiceError mkServiceConfig "KMSDisabledException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _KMSDisabledException #-}
{-# DEPRECATED _KMSDisabledException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Lambda was not able to set up VPC access for the Lambda function because one or more configured subnets has no available IP addresses.
_SubnetIPAddressLimitReachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetIPAddressLimitReachedException
  = Core._MatchServiceError mkServiceConfig
      "SubnetIPAddressLimitReachedException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _SubnetIPAddressLimitReachedException #-}
{-# DEPRECATED _SubnetIPAddressLimitReachedException "Use generic-lens or generic-optics instead"  #-}

-- | The AWS Lambda service encountered an internal error.
_ServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceException
  = Core._MatchServiceError mkServiceConfig "ServiceException" Core..
      Core.hasStatues 500
{-# INLINEABLE _ServiceException #-}
{-# DEPRECATED _ServiceException "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded your maximum total code size per account. <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more> 
_CodeStorageExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeStorageExceededException
  = Core._MatchServiceError mkServiceConfig
      "CodeStorageExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CodeStorageExceededException #-}
{-# DEPRECATED _CodeStorageExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified code signing configuration does not exist.
_CodeSigningConfigNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CodeSigningConfigNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "CodeSigningConfigNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _CodeSigningConfigNotFoundException #-}
{-# DEPRECATED _CodeSigningConfigNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Lambda could not unzip the deployment package.
_InvalidZipFileException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidZipFileException
  = Core._MatchServiceError mkServiceConfig "InvalidZipFileException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _InvalidZipFileException #-}
{-# DEPRECATED _InvalidZipFileException "Use generic-lens or generic-optics instead"  #-}

-- | The specified configuration does not exist.
_ProvisionedConcurrencyConfigNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedConcurrencyConfigNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ProvisionedConcurrencyConfigNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ProvisionedConcurrencyConfigNotFoundException #-}
{-# DEPRECATED _ProvisionedConcurrencyConfigNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The resource already exists, or another operation is in progress.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException
  = Core._MatchServiceError mkServiceConfig
      "ResourceConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ResourceConflictException #-}
{-# DEPRECATED _ResourceConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The function is inactive and its VPC connection is no longer available. Wait for the VPC connection to reestablish and try again.
_ResourceNotReadyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotReadyException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotReadyException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _ResourceNotReadyException #-}
{-# DEPRECATED _ResourceNotReadyException "Use generic-lens or generic-optics instead"  #-}

-- | AWS Lambda received an unexpected EC2 client exception while setting up for the Lambda function.
_EC2UnexpectedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EC2UnexpectedException
  = Core._MatchServiceError mkServiceConfig "EC2UnexpectedException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _EC2UnexpectedException #-}
{-# DEPRECATED _EC2UnexpectedException "Use generic-lens or generic-optics instead"  #-}

-- | The resource specified in the request does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | An error occured when reading from or writing to a connected file system.
_EFSIOException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSIOException
  = Core._MatchServiceError mkServiceConfig "EFSIOException" Core..
      Core.hasStatues 410
{-# INLINEABLE _EFSIOException #-}
{-# DEPRECATED _EFSIOException "Use generic-lens or generic-optics instead"  #-}

-- | The function was able to make a network connection to the configured file system, but the mount operation timed out.
_EFSMountTimeoutException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EFSMountTimeoutException
  = Core._MatchServiceError mkServiceConfig
      "EFSMountTimeoutException"
      Core.. Core.hasStatues 408
{-# INLINEABLE _EFSMountTimeoutException #-}
{-# DEPRECATED _EFSMountTimeoutException "Use generic-lens or generic-optics instead"  #-}

-- | Lambda was unable to decrypt the environment variables because KMS access was denied. Check the Lambda function's KMS permissions.
_KMSAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSAccessDeniedException
  = Core._MatchServiceError mkServiceConfig
      "KMSAccessDeniedException"
      Core.. Core.hasStatues 502
{-# INLINEABLE _KMSAccessDeniedException #-}
{-# DEPRECATED _KMSAccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The operation conflicts with the resource's availability. For example, you attempted to update an EventSource Mapping in CREATING, or tried to delete a EventSource mapping currently in the UPDATING state.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
