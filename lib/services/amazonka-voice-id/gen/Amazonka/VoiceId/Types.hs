{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VoiceId.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * AuthenticationDecision
    AuthenticationDecision (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * DuplicateRegistrationAction
    DuplicateRegistrationAction (..),

    -- * ExistingEnrollmentAction
    ExistingEnrollmentAction (..),

    -- * FraudDetectionAction
    FraudDetectionAction (..),

    -- * FraudDetectionDecision
    FraudDetectionDecision (..),

    -- * FraudDetectionReason
    FraudDetectionReason (..),

    -- * FraudsterRegistrationJobStatus
    FraudsterRegistrationJobStatus (..),

    -- * ServerSideEncryptionUpdateStatus
    ServerSideEncryptionUpdateStatus (..),

    -- * SpeakerEnrollmentJobStatus
    SpeakerEnrollmentJobStatus (..),

    -- * SpeakerStatus
    SpeakerStatus (..),

    -- * StreamingStatus
    StreamingStatus (..),

    -- * AuthenticationConfiguration
    AuthenticationConfiguration (..),
    newAuthenticationConfiguration,
    authenticationConfiguration_acceptanceThreshold,

    -- * AuthenticationResult
    AuthenticationResult (..),
    newAuthenticationResult,
    authenticationResult_audioAggregationEndedAt,
    authenticationResult_authenticationResultId,
    authenticationResult_score,
    authenticationResult_configuration,
    authenticationResult_decision,
    authenticationResult_customerSpeakerId,
    authenticationResult_generatedSpeakerId,
    authenticationResult_audioAggregationStartedAt,

    -- * Domain
    Domain (..),
    newDomain,
    domain_name,
    domain_serverSideEncryptionUpdateDetails,
    domain_serverSideEncryptionConfiguration,
    domain_arn,
    domain_description,
    domain_domainStatus,
    domain_domainId,
    domain_createdAt,
    domain_updatedAt,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_name,
    domainSummary_serverSideEncryptionUpdateDetails,
    domainSummary_serverSideEncryptionConfiguration,
    domainSummary_arn,
    domainSummary_description,
    domainSummary_domainStatus,
    domainSummary_domainId,
    domainSummary_createdAt,
    domainSummary_updatedAt,

    -- * EnrollmentConfig
    EnrollmentConfig (..),
    newEnrollmentConfig,
    enrollmentConfig_fraudDetectionConfig,
    enrollmentConfig_existingEnrollmentAction,

    -- * EnrollmentJobFraudDetectionConfig
    EnrollmentJobFraudDetectionConfig (..),
    newEnrollmentJobFraudDetectionConfig,
    enrollmentJobFraudDetectionConfig_fraudDetectionAction,
    enrollmentJobFraudDetectionConfig_riskThreshold,

    -- * FailureDetails
    FailureDetails (..),
    newFailureDetails,
    failureDetails_message,
    failureDetails_statusCode,

    -- * FraudDetectionConfiguration
    FraudDetectionConfiguration (..),
    newFraudDetectionConfiguration,
    fraudDetectionConfiguration_riskThreshold,

    -- * FraudDetectionResult
    FraudDetectionResult (..),
    newFraudDetectionResult,
    fraudDetectionResult_audioAggregationEndedAt,
    fraudDetectionResult_fraudDetectionResultId,
    fraudDetectionResult_riskDetails,
    fraudDetectionResult_configuration,
    fraudDetectionResult_decision,
    fraudDetectionResult_audioAggregationStartedAt,
    fraudDetectionResult_reasons,

    -- * FraudRiskDetails
    FraudRiskDetails (..),
    newFraudRiskDetails,
    fraudRiskDetails_knownFraudsterRisk,
    fraudRiskDetails_voiceSpoofingRisk,

    -- * Fraudster
    Fraudster (..),
    newFraudster,
    fraudster_generatedFraudsterId,
    fraudster_domainId,
    fraudster_createdAt,

    -- * FraudsterRegistrationJob
    FraudsterRegistrationJob (..),
    newFraudsterRegistrationJob,
    fraudsterRegistrationJob_outputDataConfig,
    fraudsterRegistrationJob_jobStatus,
    fraudsterRegistrationJob_registrationConfig,
    fraudsterRegistrationJob_jobName,
    fraudsterRegistrationJob_endedAt,
    fraudsterRegistrationJob_jobId,
    fraudsterRegistrationJob_dataAccessRoleArn,
    fraudsterRegistrationJob_failureDetails,
    fraudsterRegistrationJob_domainId,
    fraudsterRegistrationJob_jobProgress,
    fraudsterRegistrationJob_inputDataConfig,
    fraudsterRegistrationJob_createdAt,

    -- * FraudsterRegistrationJobSummary
    FraudsterRegistrationJobSummary (..),
    newFraudsterRegistrationJobSummary,
    fraudsterRegistrationJobSummary_jobStatus,
    fraudsterRegistrationJobSummary_jobName,
    fraudsterRegistrationJobSummary_endedAt,
    fraudsterRegistrationJobSummary_jobId,
    fraudsterRegistrationJobSummary_failureDetails,
    fraudsterRegistrationJobSummary_domainId,
    fraudsterRegistrationJobSummary_jobProgress,
    fraudsterRegistrationJobSummary_createdAt,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_s3Uri,

    -- * JobProgress
    JobProgress (..),
    newJobProgress,
    jobProgress_percentComplete,

    -- * KnownFraudsterRisk
    KnownFraudsterRisk (..),
    newKnownFraudsterRisk,
    knownFraudsterRisk_generatedFraudsterId,
    knownFraudsterRisk_riskScore,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- * RegistrationConfig
    RegistrationConfig (..),
    newRegistrationConfig,
    registrationConfig_duplicateRegistrationAction,
    registrationConfig_fraudsterSimilarityThreshold,

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    newServerSideEncryptionConfiguration,
    serverSideEncryptionConfiguration_kmsKeyId,

    -- * ServerSideEncryptionUpdateDetails
    ServerSideEncryptionUpdateDetails (..),
    newServerSideEncryptionUpdateDetails,
    serverSideEncryptionUpdateDetails_message,
    serverSideEncryptionUpdateDetails_oldKmsKeyId,
    serverSideEncryptionUpdateDetails_updateStatus,

    -- * Speaker
    Speaker (..),
    newSpeaker,
    speaker_lastAccessedAt,
    speaker_status,
    speaker_customerSpeakerId,
    speaker_generatedSpeakerId,
    speaker_domainId,
    speaker_createdAt,
    speaker_updatedAt,

    -- * SpeakerEnrollmentJob
    SpeakerEnrollmentJob (..),
    newSpeakerEnrollmentJob,
    speakerEnrollmentJob_outputDataConfig,
    speakerEnrollmentJob_enrollmentConfig,
    speakerEnrollmentJob_jobStatus,
    speakerEnrollmentJob_jobName,
    speakerEnrollmentJob_endedAt,
    speakerEnrollmentJob_jobId,
    speakerEnrollmentJob_dataAccessRoleArn,
    speakerEnrollmentJob_failureDetails,
    speakerEnrollmentJob_domainId,
    speakerEnrollmentJob_jobProgress,
    speakerEnrollmentJob_inputDataConfig,
    speakerEnrollmentJob_createdAt,

    -- * SpeakerEnrollmentJobSummary
    SpeakerEnrollmentJobSummary (..),
    newSpeakerEnrollmentJobSummary,
    speakerEnrollmentJobSummary_jobStatus,
    speakerEnrollmentJobSummary_jobName,
    speakerEnrollmentJobSummary_endedAt,
    speakerEnrollmentJobSummary_jobId,
    speakerEnrollmentJobSummary_failureDetails,
    speakerEnrollmentJobSummary_domainId,
    speakerEnrollmentJobSummary_jobProgress,
    speakerEnrollmentJobSummary_createdAt,

    -- * SpeakerSummary
    SpeakerSummary (..),
    newSpeakerSummary,
    speakerSummary_lastAccessedAt,
    speakerSummary_status,
    speakerSummary_customerSpeakerId,
    speakerSummary_generatedSpeakerId,
    speakerSummary_domainId,
    speakerSummary_createdAt,
    speakerSummary_updatedAt,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * VoiceSpoofingRisk
    VoiceSpoofingRisk (..),
    newVoiceSpoofingRisk,
    voiceSpoofingRisk_riskScore,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.VoiceId.Types.AuthenticationConfiguration
import Amazonka.VoiceId.Types.AuthenticationDecision
import Amazonka.VoiceId.Types.AuthenticationResult
import Amazonka.VoiceId.Types.Domain
import Amazonka.VoiceId.Types.DomainStatus
import Amazonka.VoiceId.Types.DomainSummary
import Amazonka.VoiceId.Types.DuplicateRegistrationAction
import Amazonka.VoiceId.Types.EnrollmentConfig
import Amazonka.VoiceId.Types.EnrollmentJobFraudDetectionConfig
import Amazonka.VoiceId.Types.ExistingEnrollmentAction
import Amazonka.VoiceId.Types.FailureDetails
import Amazonka.VoiceId.Types.FraudDetectionAction
import Amazonka.VoiceId.Types.FraudDetectionConfiguration
import Amazonka.VoiceId.Types.FraudDetectionDecision
import Amazonka.VoiceId.Types.FraudDetectionReason
import Amazonka.VoiceId.Types.FraudDetectionResult
import Amazonka.VoiceId.Types.FraudRiskDetails
import Amazonka.VoiceId.Types.Fraudster
import Amazonka.VoiceId.Types.FraudsterRegistrationJob
import Amazonka.VoiceId.Types.FraudsterRegistrationJobStatus
import Amazonka.VoiceId.Types.FraudsterRegistrationJobSummary
import Amazonka.VoiceId.Types.InputDataConfig
import Amazonka.VoiceId.Types.JobProgress
import Amazonka.VoiceId.Types.KnownFraudsterRisk
import Amazonka.VoiceId.Types.OutputDataConfig
import Amazonka.VoiceId.Types.RegistrationConfig
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateStatus
import Amazonka.VoiceId.Types.Speaker
import Amazonka.VoiceId.Types.SpeakerEnrollmentJob
import Amazonka.VoiceId.Types.SpeakerEnrollmentJobStatus
import Amazonka.VoiceId.Types.SpeakerEnrollmentJobSummary
import Amazonka.VoiceId.Types.SpeakerStatus
import Amazonka.VoiceId.Types.SpeakerSummary
import Amazonka.VoiceId.Types.StreamingStatus
import Amazonka.VoiceId.Types.Tag
import Amazonka.VoiceId.Types.VoiceSpoofingRisk

-- | API version @2021-09-27@ of the Amazon Voice ID SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "VoiceId",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "voiceid",
      Core.signingName = "voiceid",
      Core.version = "2021-09-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "VoiceId",
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

-- | You do not have sufficient permissions to perform this action. Check the
-- error message and try again.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request failed due to an unknown error on the server side.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The request exceeded the service quota. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#voiceid-quotas Voice ID Service Quotas>
-- and try your request again.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The specified resource cannot be found. Check the @ResourceType@ and
-- error message for more details.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request failed due to a conflict. Check the @ConflictType@ and error
-- message for more details.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request was denied due to request throttling. Please slow down your
-- request rate. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html##voiceid-api-quotas Amazon Connect Voice ID Service API throttling quotas>
-- and try your request again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request failed one or more validations; check the error message for
-- more details.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
