{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServiceError,
    _InvalidNextTokenException,
    _InvalidRequestException,
    _ResourceExistsException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _ThrottlingException,

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

    -- * EncryptionType
    EncryptionType (..),

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
    as2ConnectorConfig_compression,
    as2ConnectorConfig_encryptionAlgorithm,
    as2ConnectorConfig_localProfileId,
    as2ConnectorConfig_mdnResponse,
    as2ConnectorConfig_mdnSigningAlgorithm,
    as2ConnectorConfig_messageSubject,
    as2ConnectorConfig_partnerProfileId,
    as2ConnectorConfig_signingAlgorithm,

    -- * CopyStepDetails
    CopyStepDetails (..),
    newCopyStepDetails,
    copyStepDetails_destinationFileLocation,
    copyStepDetails_name,
    copyStepDetails_overwriteExisting,
    copyStepDetails_sourceFileLocation,

    -- * CustomStepDetails
    CustomStepDetails (..),
    newCustomStepDetails,
    customStepDetails_name,
    customStepDetails_sourceFileLocation,
    customStepDetails_target,
    customStepDetails_timeoutSeconds,

    -- * DecryptStepDetails
    DecryptStepDetails (..),
    newDecryptStepDetails,
    decryptStepDetails_name,
    decryptStepDetails_overwriteExisting,
    decryptStepDetails_sourceFileLocation,
    decryptStepDetails_type,
    decryptStepDetails_destinationFileLocation,

    -- * DeleteStepDetails
    DeleteStepDetails (..),
    newDeleteStepDetails,
    deleteStepDetails_name,
    deleteStepDetails_sourceFileLocation,

    -- * DescribedAccess
    DescribedAccess (..),
    newDescribedAccess,
    describedAccess_externalId,
    describedAccess_homeDirectory,
    describedAccess_homeDirectoryMappings,
    describedAccess_homeDirectoryType,
    describedAccess_policy,
    describedAccess_posixProfile,
    describedAccess_role,

    -- * DescribedAgreement
    DescribedAgreement (..),
    newDescribedAgreement,
    describedAgreement_accessRole,
    describedAgreement_agreementId,
    describedAgreement_baseDirectory,
    describedAgreement_description,
    describedAgreement_localProfileId,
    describedAgreement_partnerProfileId,
    describedAgreement_serverId,
    describedAgreement_status,
    describedAgreement_tags,
    describedAgreement_arn,

    -- * DescribedCertificate
    DescribedCertificate (..),
    newDescribedCertificate,
    describedCertificate_activeDate,
    describedCertificate_certificate,
    describedCertificate_certificateChain,
    describedCertificate_certificateId,
    describedCertificate_description,
    describedCertificate_inactiveDate,
    describedCertificate_notAfterDate,
    describedCertificate_notBeforeDate,
    describedCertificate_serial,
    describedCertificate_status,
    describedCertificate_tags,
    describedCertificate_type,
    describedCertificate_usage,
    describedCertificate_arn,

    -- * DescribedConnector
    DescribedConnector (..),
    newDescribedConnector,
    describedConnector_accessRole,
    describedConnector_as2Config,
    describedConnector_connectorId,
    describedConnector_loggingRole,
    describedConnector_tags,
    describedConnector_url,
    describedConnector_arn,

    -- * DescribedExecution
    DescribedExecution (..),
    newDescribedExecution,
    describedExecution_executionId,
    describedExecution_executionRole,
    describedExecution_initialFileLocation,
    describedExecution_loggingConfiguration,
    describedExecution_posixProfile,
    describedExecution_results,
    describedExecution_serviceMetadata,
    describedExecution_status,

    -- * DescribedHostKey
    DescribedHostKey (..),
    newDescribedHostKey,
    describedHostKey_dateImported,
    describedHostKey_description,
    describedHostKey_hostKeyFingerprint,
    describedHostKey_hostKeyId,
    describedHostKey_tags,
    describedHostKey_type,
    describedHostKey_arn,

    -- * DescribedProfile
    DescribedProfile (..),
    newDescribedProfile,
    describedProfile_as2Id,
    describedProfile_certificateIds,
    describedProfile_profileId,
    describedProfile_profileType,
    describedProfile_tags,
    describedProfile_arn,

    -- * DescribedSecurityPolicy
    DescribedSecurityPolicy (..),
    newDescribedSecurityPolicy,
    describedSecurityPolicy_fips,
    describedSecurityPolicy_sshCiphers,
    describedSecurityPolicy_sshKexs,
    describedSecurityPolicy_sshMacs,
    describedSecurityPolicy_tlsCiphers,
    describedSecurityPolicy_securityPolicyName,

    -- * DescribedServer
    DescribedServer (..),
    newDescribedServer,
    describedServer_certificate,
    describedServer_domain,
    describedServer_endpointDetails,
    describedServer_endpointType,
    describedServer_hostKeyFingerprint,
    describedServer_identityProviderDetails,
    describedServer_identityProviderType,
    describedServer_loggingRole,
    describedServer_postAuthenticationLoginBanner,
    describedServer_preAuthenticationLoginBanner,
    describedServer_protocolDetails,
    describedServer_protocols,
    describedServer_securityPolicyName,
    describedServer_serverId,
    describedServer_state,
    describedServer_tags,
    describedServer_userCount,
    describedServer_workflowDetails,
    describedServer_arn,

    -- * DescribedUser
    DescribedUser (..),
    newDescribedUser,
    describedUser_homeDirectory,
    describedUser_homeDirectoryMappings,
    describedUser_homeDirectoryType,
    describedUser_policy,
    describedUser_posixProfile,
    describedUser_role,
    describedUser_sshPublicKeys,
    describedUser_tags,
    describedUser_userName,
    describedUser_arn,

    -- * DescribedWorkflow
    DescribedWorkflow (..),
    newDescribedWorkflow,
    describedWorkflow_description,
    describedWorkflow_onExceptionSteps,
    describedWorkflow_steps,
    describedWorkflow_tags,
    describedWorkflow_workflowId,
    describedWorkflow_arn,

    -- * EfsFileLocation
    EfsFileLocation (..),
    newEfsFileLocation,
    efsFileLocation_fileSystemId,
    efsFileLocation_path,

    -- * EndpointDetails
    EndpointDetails (..),
    newEndpointDetails,
    endpointDetails_addressAllocationIds,
    endpointDetails_securityGroupIds,
    endpointDetails_subnetIds,
    endpointDetails_vpcEndpointId,
    endpointDetails_vpcId,

    -- * ExecutionError
    ExecutionError (..),
    newExecutionError,
    executionError_type,
    executionError_message,

    -- * ExecutionResults
    ExecutionResults (..),
    newExecutionResults,
    executionResults_onExceptionSteps,
    executionResults_steps,

    -- * ExecutionStepResult
    ExecutionStepResult (..),
    newExecutionStepResult,
    executionStepResult_error,
    executionStepResult_outputs,
    executionStepResult_stepType,

    -- * FileLocation
    FileLocation (..),
    newFileLocation,
    fileLocation_efsFileLocation,
    fileLocation_s3FileLocation,

    -- * HomeDirectoryMapEntry
    HomeDirectoryMapEntry (..),
    newHomeDirectoryMapEntry,
    homeDirectoryMapEntry_entry,
    homeDirectoryMapEntry_target,

    -- * IdentityProviderDetails
    IdentityProviderDetails (..),
    newIdentityProviderDetails,
    identityProviderDetails_directoryId,
    identityProviderDetails_function,
    identityProviderDetails_invocationRole,
    identityProviderDetails_url,

    -- * InputFileLocation
    InputFileLocation (..),
    newInputFileLocation,
    inputFileLocation_efsFileLocation,
    inputFileLocation_s3FileLocation,

    -- * ListedAccess
    ListedAccess (..),
    newListedAccess,
    listedAccess_externalId,
    listedAccess_homeDirectory,
    listedAccess_homeDirectoryType,
    listedAccess_role,

    -- * ListedAgreement
    ListedAgreement (..),
    newListedAgreement,
    listedAgreement_agreementId,
    listedAgreement_arn,
    listedAgreement_description,
    listedAgreement_localProfileId,
    listedAgreement_partnerProfileId,
    listedAgreement_serverId,
    listedAgreement_status,

    -- * ListedCertificate
    ListedCertificate (..),
    newListedCertificate,
    listedCertificate_activeDate,
    listedCertificate_arn,
    listedCertificate_certificateId,
    listedCertificate_description,
    listedCertificate_inactiveDate,
    listedCertificate_status,
    listedCertificate_type,
    listedCertificate_usage,

    -- * ListedConnector
    ListedConnector (..),
    newListedConnector,
    listedConnector_arn,
    listedConnector_connectorId,
    listedConnector_url,

    -- * ListedExecution
    ListedExecution (..),
    newListedExecution,
    listedExecution_executionId,
    listedExecution_initialFileLocation,
    listedExecution_serviceMetadata,
    listedExecution_status,

    -- * ListedHostKey
    ListedHostKey (..),
    newListedHostKey,
    listedHostKey_dateImported,
    listedHostKey_description,
    listedHostKey_fingerprint,
    listedHostKey_hostKeyId,
    listedHostKey_type,
    listedHostKey_arn,

    -- * ListedProfile
    ListedProfile (..),
    newListedProfile,
    listedProfile_arn,
    listedProfile_as2Id,
    listedProfile_profileId,
    listedProfile_profileType,

    -- * ListedServer
    ListedServer (..),
    newListedServer,
    listedServer_domain,
    listedServer_endpointType,
    listedServer_identityProviderType,
    listedServer_loggingRole,
    listedServer_serverId,
    listedServer_state,
    listedServer_userCount,
    listedServer_arn,

    -- * ListedUser
    ListedUser (..),
    newListedUser,
    listedUser_homeDirectory,
    listedUser_homeDirectoryType,
    listedUser_role,
    listedUser_sshPublicKeyCount,
    listedUser_userName,
    listedUser_arn,

    -- * ListedWorkflow
    ListedWorkflow (..),
    newListedWorkflow,
    listedWorkflow_arn,
    listedWorkflow_description,
    listedWorkflow_workflowId,

    -- * LoggingConfiguration
    LoggingConfiguration (..),
    newLoggingConfiguration,
    loggingConfiguration_logGroupName,
    loggingConfiguration_loggingRole,

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
    protocolDetails_setStatOption,
    protocolDetails_tlsSessionResumptionMode,

    -- * S3FileLocation
    S3FileLocation (..),
    newS3FileLocation,
    s3FileLocation_bucket,
    s3FileLocation_etag,
    s3FileLocation_key,
    s3FileLocation_versionId,

    -- * S3InputFileLocation
    S3InputFileLocation (..),
    newS3InputFileLocation,
    s3InputFileLocation_bucket,
    s3InputFileLocation_key,

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
    tagStepDetails_name,
    tagStepDetails_sourceFileLocation,
    tagStepDetails_tags,

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
    workflowStep_copyStepDetails,
    workflowStep_customStepDetails,
    workflowStep_decryptStepDetails,
    workflowStep_deleteStepDetails,
    workflowStep_tagStepDetails,
    workflowStep_type,
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
import Amazonka.Transfer.Types.DecryptStepDetails
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
import Amazonka.Transfer.Types.EncryptionType
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | This exception is thrown when the @UpdateServer@ is called for a file
-- transfer protocol-enabled server that has VPC as the endpoint type and
-- the server\'s @VpcEndpointID@ is not in the available state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | This exception is thrown when an error occurs in the Amazon Web
-- ServicesTransfer Family service.
_InternalServiceError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"

-- | The @NextToken@ parameter that was passed is invalid.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | This exception is thrown when the client submits a malformed request.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The requested resource does not exist.
_ResourceExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceExistsException"

-- | This exception is thrown when a resource is not found by the Amazon Web
-- ServicesTransfer Family service.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request has failed because the Amazon Web ServicesTransfer Family
-- service is not available.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
