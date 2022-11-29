{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _InvalidNextTokenException,
    _InternalServiceError,
    _ConflictException,
    _ThrottlingException,
    _ResourceExistsException,
    _InvalidRequestException,

    -- * AgreementStatusType
    AgreementStatusType (..),

    -- * As2Transport
    As2Transport (..),

    -- * CertificateStatusType
    CertificateStatusType (..),

    -- * CertificateType
    CertificateType (..),

    -- * CertificateUsageType
    CertificateUsageType (..),

    -- * CompressionEnum
    CompressionEnum (..),

    -- * CustomStepStatus
    CustomStepStatus (..),

    -- * Domain
    Domain (..),

    -- * EncryptionAlg
    EncryptionAlg (..),

    -- * EndpointType
    EndpointType (..),

    -- * ExecutionErrorType
    ExecutionErrorType (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * HomeDirectoryType
    HomeDirectoryType (..),

    -- * IdentityProviderType
    IdentityProviderType (..),

    -- * MdnResponse
    MdnResponse (..),

    -- * MdnSigningAlg
    MdnSigningAlg (..),

    -- * OverwriteExisting
    OverwriteExisting (..),

    -- * ProfileType
    ProfileType (..),

    -- * Protocol
    Protocol (..),

    -- * SetStatOption
    SetStatOption (..),

    -- * SigningAlg
    SigningAlg (..),

    -- * State
    State (..),

    -- * TlsSessionResumptionMode
    TlsSessionResumptionMode (..),

    -- * WorkflowStepType
    WorkflowStepType (..),

    -- * As2ConnectorConfig
    As2ConnectorConfig (..),
    newAs2ConnectorConfig,
    as2ConnectorConfig_encryptionAlgorithm,
    as2ConnectorConfig_compression,
    as2ConnectorConfig_mdnSigningAlgorithm,
    as2ConnectorConfig_localProfileId,
    as2ConnectorConfig_mdnResponse,
    as2ConnectorConfig_messageSubject,
    as2ConnectorConfig_signingAlgorithm,
    as2ConnectorConfig_partnerProfileId,

    -- * CopyStepDetails
    CopyStepDetails (..),
    newCopyStepDetails,
    copyStepDetails_name,
    copyStepDetails_overwriteExisting,
    copyStepDetails_sourceFileLocation,
    copyStepDetails_destinationFileLocation,

    -- * CustomStepDetails
    CustomStepDetails (..),
    newCustomStepDetails,
    customStepDetails_name,
    customStepDetails_timeoutSeconds,
    customStepDetails_target,
    customStepDetails_sourceFileLocation,

    -- * DeleteStepDetails
    DeleteStepDetails (..),
    newDeleteStepDetails,
    deleteStepDetails_name,
    deleteStepDetails_sourceFileLocation,

    -- * DescribedAccess
    DescribedAccess (..),
    newDescribedAccess,
    describedAccess_homeDirectory,
    describedAccess_policy,
    describedAccess_posixProfile,
    describedAccess_externalId,
    describedAccess_role,
    describedAccess_homeDirectoryType,
    describedAccess_homeDirectoryMappings,

    -- * DescribedAgreement
    DescribedAgreement (..),
    newDescribedAgreement,
    describedAgreement_tags,
    describedAgreement_accessRole,
    describedAgreement_status,
    describedAgreement_baseDirectory,
    describedAgreement_description,
    describedAgreement_localProfileId,
    describedAgreement_agreementId,
    describedAgreement_serverId,
    describedAgreement_partnerProfileId,
    describedAgreement_arn,

    -- * DescribedCertificate
    DescribedCertificate (..),
    newDescribedCertificate,
    describedCertificate_tags,
    describedCertificate_usage,
    describedCertificate_notBeforeDate,
    describedCertificate_type,
    describedCertificate_notAfterDate,
    describedCertificate_serial,
    describedCertificate_certificate,
    describedCertificate_status,
    describedCertificate_description,
    describedCertificate_certificateId,
    describedCertificate_activeDate,
    describedCertificate_certificateChain,
    describedCertificate_inactiveDate,
    describedCertificate_arn,

    -- * DescribedConnector
    DescribedConnector (..),
    newDescribedConnector,
    describedConnector_tags,
    describedConnector_connectorId,
    describedConnector_accessRole,
    describedConnector_url,
    describedConnector_as2Config,
    describedConnector_loggingRole,
    describedConnector_arn,

    -- * DescribedExecution
    DescribedExecution (..),
    newDescribedExecution,
    describedExecution_executionRole,
    describedExecution_serviceMetadata,
    describedExecution_initialFileLocation,
    describedExecution_posixProfile,
    describedExecution_status,
    describedExecution_executionId,
    describedExecution_results,
    describedExecution_loggingConfiguration,

    -- * DescribedHostKey
    DescribedHostKey (..),
    newDescribedHostKey,
    describedHostKey_tags,
    describedHostKey_hostKeyId,
    describedHostKey_type,
    describedHostKey_description,
    describedHostKey_hostKeyFingerprint,
    describedHostKey_dateImported,
    describedHostKey_arn,

    -- * DescribedProfile
    DescribedProfile (..),
    newDescribedProfile,
    describedProfile_tags,
    describedProfile_profileId,
    describedProfile_certificateIds,
    describedProfile_as2Id,
    describedProfile_profileType,
    describedProfile_arn,

    -- * DescribedSecurityPolicy
    DescribedSecurityPolicy (..),
    newDescribedSecurityPolicy,
    describedSecurityPolicy_tlsCiphers,
    describedSecurityPolicy_sshKexs,
    describedSecurityPolicy_fips,
    describedSecurityPolicy_sshCiphers,
    describedSecurityPolicy_sshMacs,
    describedSecurityPolicy_securityPolicyName,

    -- * DescribedServer
    DescribedServer (..),
    newDescribedServer,
    describedServer_tags,
    describedServer_userCount,
    describedServer_preAuthenticationLoginBanner,
    describedServer_protocolDetails,
    describedServer_identityProviderDetails,
    describedServer_domain,
    describedServer_identityProviderType,
    describedServer_securityPolicyName,
    describedServer_endpointDetails,
    describedServer_state,
    describedServer_certificate,
    describedServer_protocols,
    describedServer_endpointType,
    describedServer_hostKeyFingerprint,
    describedServer_loggingRole,
    describedServer_serverId,
    describedServer_postAuthenticationLoginBanner,
    describedServer_workflowDetails,
    describedServer_arn,

    -- * DescribedUser
    DescribedUser (..),
    newDescribedUser,
    describedUser_tags,
    describedUser_homeDirectory,
    describedUser_policy,
    describedUser_userName,
    describedUser_posixProfile,
    describedUser_sshPublicKeys,
    describedUser_role,
    describedUser_homeDirectoryType,
    describedUser_homeDirectoryMappings,
    describedUser_arn,

    -- * DescribedWorkflow
    DescribedWorkflow (..),
    newDescribedWorkflow,
    describedWorkflow_tags,
    describedWorkflow_workflowId,
    describedWorkflow_steps,
    describedWorkflow_description,
    describedWorkflow_onExceptionSteps,
    describedWorkflow_arn,

    -- * EfsFileLocation
    EfsFileLocation (..),
    newEfsFileLocation,
    efsFileLocation_path,
    efsFileLocation_fileSystemId,

    -- * EndpointDetails
    EndpointDetails (..),
    newEndpointDetails,
    endpointDetails_securityGroupIds,
    endpointDetails_vpcEndpointId,
    endpointDetails_addressAllocationIds,
    endpointDetails_vpcId,
    endpointDetails_subnetIds,

    -- * ExecutionError
    ExecutionError (..),
    newExecutionError,
    executionError_type,
    executionError_message,

    -- * ExecutionResults
    ExecutionResults (..),
    newExecutionResults,
    executionResults_steps,
    executionResults_onExceptionSteps,

    -- * ExecutionStepResult
    ExecutionStepResult (..),
    newExecutionStepResult,
    executionStepResult_outputs,
    executionStepResult_stepType,
    executionStepResult_error,

    -- * FileLocation
    FileLocation (..),
    newFileLocation,
    fileLocation_s3FileLocation,
    fileLocation_efsFileLocation,

    -- * HomeDirectoryMapEntry
    HomeDirectoryMapEntry (..),
    newHomeDirectoryMapEntry,
    homeDirectoryMapEntry_entry,
    homeDirectoryMapEntry_target,

    -- * IdentityProviderDetails
    IdentityProviderDetails (..),
    newIdentityProviderDetails,
    identityProviderDetails_directoryId,
    identityProviderDetails_url,
    identityProviderDetails_invocationRole,
    identityProviderDetails_function,

    -- * InputFileLocation
    InputFileLocation (..),
    newInputFileLocation,
    inputFileLocation_s3FileLocation,
    inputFileLocation_efsFileLocation,

    -- * ListedAccess
    ListedAccess (..),
    newListedAccess,
    listedAccess_homeDirectory,
    listedAccess_externalId,
    listedAccess_role,
    listedAccess_homeDirectoryType,

    -- * ListedAgreement
    ListedAgreement (..),
    newListedAgreement,
    listedAgreement_arn,
    listedAgreement_status,
    listedAgreement_description,
    listedAgreement_localProfileId,
    listedAgreement_agreementId,
    listedAgreement_serverId,
    listedAgreement_partnerProfileId,

    -- * ListedCertificate
    ListedCertificate (..),
    newListedCertificate,
    listedCertificate_usage,
    listedCertificate_type,
    listedCertificate_arn,
    listedCertificate_status,
    listedCertificate_description,
    listedCertificate_certificateId,
    listedCertificate_activeDate,
    listedCertificate_inactiveDate,

    -- * ListedConnector
    ListedConnector (..),
    newListedConnector,
    listedConnector_connectorId,
    listedConnector_arn,
    listedConnector_url,

    -- * ListedExecution
    ListedExecution (..),
    newListedExecution,
    listedExecution_serviceMetadata,
    listedExecution_initialFileLocation,
    listedExecution_status,
    listedExecution_executionId,

    -- * ListedHostKey
    ListedHostKey (..),
    newListedHostKey,
    listedHostKey_hostKeyId,
    listedHostKey_type,
    listedHostKey_description,
    listedHostKey_fingerprint,
    listedHostKey_dateImported,
    listedHostKey_arn,

    -- * ListedProfile
    ListedProfile (..),
    newListedProfile,
    listedProfile_profileId,
    listedProfile_as2Id,
    listedProfile_arn,
    listedProfile_profileType,

    -- * ListedServer
    ListedServer (..),
    newListedServer,
    listedServer_userCount,
    listedServer_domain,
    listedServer_identityProviderType,
    listedServer_state,
    listedServer_endpointType,
    listedServer_loggingRole,
    listedServer_serverId,
    listedServer_arn,

    -- * ListedUser
    ListedUser (..),
    newListedUser,
    listedUser_homeDirectory,
    listedUser_userName,
    listedUser_role,
    listedUser_homeDirectoryType,
    listedUser_sshPublicKeyCount,
    listedUser_arn,

    -- * ListedWorkflow
    ListedWorkflow (..),
    newListedWorkflow,
    listedWorkflow_workflowId,
    listedWorkflow_arn,
    listedWorkflow_description,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_loggingRole,
    loggingConfiguration_logGroupName,

    -- * PosixProfile
    PosixProfile (..),
    newPosixProfile,
    posixProfile_secondaryGids,
    posixProfile_uid,
    posixProfile_gid,

    -- * ProtocolDetails
    ProtocolDetails (..),
    newProtocolDetails,
    protocolDetails_as2Transports,
    protocolDetails_passiveIp,
    protocolDetails_tlsSessionResumptionMode,
    protocolDetails_setStatOption,

    -- * S3FileLocation
    S3FileLocation (..),
    newS3FileLocation,
    s3FileLocation_key,
    s3FileLocation_bucket,
    s3FileLocation_etag,
    s3FileLocation_versionId,

    -- * S3InputFileLocation
    S3InputFileLocation (..),
    newS3InputFileLocation,
    s3InputFileLocation_key,
    s3InputFileLocation_bucket,

    -- * S3Tag
    S3Tag (..),
    newS3Tag,
    s3Tag_key,
    s3Tag_value,

    -- * ServiceMetadata
    ServiceMetadata (..),
    newServiceMetadata,
    serviceMetadata_userDetails,

    -- * SshPublicKey
    SshPublicKey (..),
    newSshPublicKey,
    sshPublicKey_dateImported,
    sshPublicKey_sshPublicKeyBody,
    sshPublicKey_sshPublicKeyId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagStepDetails
    TagStepDetails (..),
    newTagStepDetails,
    tagStepDetails_tags,
    tagStepDetails_name,
    tagStepDetails_sourceFileLocation,

    -- * UserDetails
    UserDetails (..),
    newUserDetails,
    userDetails_sessionId,
    userDetails_userName,
    userDetails_serverId,

    -- * WorkflowDetail
    WorkflowDetail (..),
    newWorkflowDetail,
    workflowDetail_workflowId,
    workflowDetail_executionRole,

    -- * WorkflowDetails
    WorkflowDetails (..),
    newWorkflowDetails,
    workflowDetails_onPartialUpload,
    workflowDetails_onUpload,

    -- * WorkflowStep
    WorkflowStep (..),
    newWorkflowStep,
    workflowStep_type,
    workflowStep_tagStepDetails,
    workflowStep_customStepDetails,
    workflowStep_deleteStepDetails,
    workflowStep_copyStepDetails,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Transfer.Types.AgreementStatusType
import Amazonka.Transfer.Types.As2ConnectorConfig
import Amazonka.Transfer.Types.As2Transport
import Amazonka.Transfer.Types.CertificateStatusType
import Amazonka.Transfer.Types.CertificateType
import Amazonka.Transfer.Types.CertificateUsageType
import Amazonka.Transfer.Types.CompressionEnum
import Amazonka.Transfer.Types.CopyStepDetails
import Amazonka.Transfer.Types.CustomStepDetails
import Amazonka.Transfer.Types.CustomStepStatus
import Amazonka.Transfer.Types.DeleteStepDetails
import Amazonka.Transfer.Types.DescribedAccess
import Amazonka.Transfer.Types.DescribedAgreement
import Amazonka.Transfer.Types.DescribedCertificate
import Amazonka.Transfer.Types.DescribedConnector
import Amazonka.Transfer.Types.DescribedExecution
import Amazonka.Transfer.Types.DescribedHostKey
import Amazonka.Transfer.Types.DescribedProfile
import Amazonka.Transfer.Types.DescribedSecurityPolicy
import Amazonka.Transfer.Types.DescribedServer
import Amazonka.Transfer.Types.DescribedUser
import Amazonka.Transfer.Types.DescribedWorkflow
import Amazonka.Transfer.Types.Domain
import Amazonka.Transfer.Types.EfsFileLocation
import Amazonka.Transfer.Types.EncryptionAlg
import Amazonka.Transfer.Types.EndpointDetails
import Amazonka.Transfer.Types.EndpointType
import Amazonka.Transfer.Types.ExecutionError
import Amazonka.Transfer.Types.ExecutionErrorType
import Amazonka.Transfer.Types.ExecutionResults
import Amazonka.Transfer.Types.ExecutionStatus
import Amazonka.Transfer.Types.ExecutionStepResult
import Amazonka.Transfer.Types.FileLocation
import Amazonka.Transfer.Types.HomeDirectoryMapEntry
import Amazonka.Transfer.Types.HomeDirectoryType
import Amazonka.Transfer.Types.IdentityProviderDetails
import Amazonka.Transfer.Types.IdentityProviderType
import Amazonka.Transfer.Types.InputFileLocation
import Amazonka.Transfer.Types.ListedAccess
import Amazonka.Transfer.Types.ListedAgreement
import Amazonka.Transfer.Types.ListedCertificate
import Amazonka.Transfer.Types.ListedConnector
import Amazonka.Transfer.Types.ListedExecution
import Amazonka.Transfer.Types.ListedHostKey
import Amazonka.Transfer.Types.ListedProfile
import Amazonka.Transfer.Types.ListedServer
import Amazonka.Transfer.Types.ListedUser
import Amazonka.Transfer.Types.ListedWorkflow
import Amazonka.Transfer.Types.LoggingConfiguration
import Amazonka.Transfer.Types.MdnResponse
import Amazonka.Transfer.Types.MdnSigningAlg
import Amazonka.Transfer.Types.OverwriteExisting
import Amazonka.Transfer.Types.PosixProfile
import Amazonka.Transfer.Types.ProfileType
import Amazonka.Transfer.Types.Protocol
import Amazonka.Transfer.Types.ProtocolDetails
import Amazonka.Transfer.Types.S3FileLocation
import Amazonka.Transfer.Types.S3InputFileLocation
import Amazonka.Transfer.Types.S3Tag
import Amazonka.Transfer.Types.ServiceMetadata
import Amazonka.Transfer.Types.SetStatOption
import Amazonka.Transfer.Types.SigningAlg
import Amazonka.Transfer.Types.SshPublicKey
import Amazonka.Transfer.Types.State
import Amazonka.Transfer.Types.Tag
import Amazonka.Transfer.Types.TagStepDetails
import Amazonka.Transfer.Types.TlsSessionResumptionMode
import Amazonka.Transfer.Types.UserDetails
import Amazonka.Transfer.Types.WorkflowDetail
import Amazonka.Transfer.Types.WorkflowDetails
import Amazonka.Transfer.Types.WorkflowStep
import Amazonka.Transfer.Types.WorkflowStepType

-- | API version @2018-11-05@ of the Amazon Transfer Family SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Transfer",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "transfer",
      Core.signingName = "transfer",
      Core.version = "2018-11-05",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Transfer",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request has failed because the Amazon Web ServicesTransfer Family
-- service is not available.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | This exception is thrown when a resource is not found by the Amazon Web
-- ServicesTransfer Family service.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The @NextToken@ parameter that was passed is invalid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | This exception is thrown when an error occurs in the Amazon Web
-- ServicesTransfer Family service.
_InternalServiceError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"

-- | This exception is thrown when the @UpdateServer@ is called for a file
-- transfer protocol-enabled server that has VPC as the endpoint type and
-- the server\'s @VpcEndpointID@ is not in the available state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The requested resource does not exist.
_ResourceExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | This exception is thrown when the client submits a malformed request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
