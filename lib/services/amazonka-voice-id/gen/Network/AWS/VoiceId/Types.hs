{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.VoiceId.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.VoiceId.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

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
    authenticationResult_customerSpeakerId,
    authenticationResult_score,
    authenticationResult_authenticationResultId,
    authenticationResult_decision,
    authenticationResult_configuration,
    authenticationResult_audioAggregationStartedAt,
    authenticationResult_generatedSpeakerId,
    authenticationResult_audioAggregationEndedAt,

    -- * Domain
    Domain (..),
    newDomain,
    domain_domainStatus,
    domain_arn,
    domain_createdAt,
    domain_name,
    domain_domainId,
    domain_updatedAt,
    domain_description,
    domain_serverSideEncryptionConfiguration,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_domainStatus,
    domainSummary_arn,
    domainSummary_createdAt,
    domainSummary_name,
    domainSummary_domainId,
    domainSummary_updatedAt,
    domainSummary_description,
    domainSummary_serverSideEncryptionConfiguration,

    -- * EnrollmentConfig
    EnrollmentConfig (..),
    newEnrollmentConfig,
    enrollmentConfig_fraudDetectionConfig,
    enrollmentConfig_existingEnrollmentAction,

    -- * EnrollmentJobFraudDetectionConfig
    EnrollmentJobFraudDetectionConfig (..),
    newEnrollmentJobFraudDetectionConfig,
    enrollmentJobFraudDetectionConfig_riskThreshold,
    enrollmentJobFraudDetectionConfig_fraudDetectionAction,

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
    fraudDetectionResult_reasons,
    fraudDetectionResult_riskDetails,
    fraudDetectionResult_fraudDetectionResultId,
    fraudDetectionResult_decision,
    fraudDetectionResult_configuration,
    fraudDetectionResult_audioAggregationStartedAt,
    fraudDetectionResult_audioAggregationEndedAt,

    -- * FraudRiskDetails
    FraudRiskDetails (..),
    newFraudRiskDetails,
    fraudRiskDetails_knownFraudsterRisk,

    -- * Fraudster
    Fraudster (..),
    newFraudster,
    fraudster_createdAt,
    fraudster_generatedFraudsterId,
    fraudster_domainId,

    -- * FraudsterRegistrationJob
    FraudsterRegistrationJob (..),
    newFraudsterRegistrationJob,
    fraudsterRegistrationJob_failureDetails,
    fraudsterRegistrationJob_jobId,
    fraudsterRegistrationJob_createdAt,
    fraudsterRegistrationJob_jobName,
    fraudsterRegistrationJob_endedAt,
    fraudsterRegistrationJob_inputDataConfig,
    fraudsterRegistrationJob_jobProgress,
    fraudsterRegistrationJob_outputDataConfig,
    fraudsterRegistrationJob_dataAccessRoleArn,
    fraudsterRegistrationJob_domainId,
    fraudsterRegistrationJob_jobStatus,
    fraudsterRegistrationJob_registrationConfig,

    -- * FraudsterRegistrationJobSummary
    FraudsterRegistrationJobSummary (..),
    newFraudsterRegistrationJobSummary,
    fraudsterRegistrationJobSummary_failureDetails,
    fraudsterRegistrationJobSummary_jobId,
    fraudsterRegistrationJobSummary_createdAt,
    fraudsterRegistrationJobSummary_jobName,
    fraudsterRegistrationJobSummary_endedAt,
    fraudsterRegistrationJobSummary_jobProgress,
    fraudsterRegistrationJobSummary_domainId,
    fraudsterRegistrationJobSummary_jobStatus,

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

    -- * Speaker
    Speaker (..),
    newSpeaker,
    speaker_status,
    speaker_customerSpeakerId,
    speaker_createdAt,
    speaker_domainId,
    speaker_updatedAt,
    speaker_generatedSpeakerId,

    -- * SpeakerEnrollmentJob
    SpeakerEnrollmentJob (..),
    newSpeakerEnrollmentJob,
    speakerEnrollmentJob_failureDetails,
    speakerEnrollmentJob_jobId,
    speakerEnrollmentJob_createdAt,
    speakerEnrollmentJob_jobName,
    speakerEnrollmentJob_endedAt,
    speakerEnrollmentJob_enrollmentConfig,
    speakerEnrollmentJob_inputDataConfig,
    speakerEnrollmentJob_jobProgress,
    speakerEnrollmentJob_outputDataConfig,
    speakerEnrollmentJob_dataAccessRoleArn,
    speakerEnrollmentJob_domainId,
    speakerEnrollmentJob_jobStatus,

    -- * SpeakerEnrollmentJobSummary
    SpeakerEnrollmentJobSummary (..),
    newSpeakerEnrollmentJobSummary,
    speakerEnrollmentJobSummary_failureDetails,
    speakerEnrollmentJobSummary_jobId,
    speakerEnrollmentJobSummary_createdAt,
    speakerEnrollmentJobSummary_jobName,
    speakerEnrollmentJobSummary_endedAt,
    speakerEnrollmentJobSummary_jobProgress,
    speakerEnrollmentJobSummary_domainId,
    speakerEnrollmentJobSummary_jobStatus,

    -- * SpeakerSummary
    SpeakerSummary (..),
    newSpeakerSummary,
    speakerSummary_status,
    speakerSummary_customerSpeakerId,
    speakerSummary_createdAt,
    speakerSummary_domainId,
    speakerSummary_updatedAt,
    speakerSummary_generatedSpeakerId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.VoiceId.Types.AuthenticationConfiguration
import Network.AWS.VoiceId.Types.AuthenticationDecision
import Network.AWS.VoiceId.Types.AuthenticationResult
import Network.AWS.VoiceId.Types.Domain
import Network.AWS.VoiceId.Types.DomainStatus
import Network.AWS.VoiceId.Types.DomainSummary
import Network.AWS.VoiceId.Types.DuplicateRegistrationAction
import Network.AWS.VoiceId.Types.EnrollmentConfig
import Network.AWS.VoiceId.Types.EnrollmentJobFraudDetectionConfig
import Network.AWS.VoiceId.Types.ExistingEnrollmentAction
import Network.AWS.VoiceId.Types.FailureDetails
import Network.AWS.VoiceId.Types.FraudDetectionAction
import Network.AWS.VoiceId.Types.FraudDetectionConfiguration
import Network.AWS.VoiceId.Types.FraudDetectionDecision
import Network.AWS.VoiceId.Types.FraudDetectionReason
import Network.AWS.VoiceId.Types.FraudDetectionResult
import Network.AWS.VoiceId.Types.FraudRiskDetails
import Network.AWS.VoiceId.Types.Fraudster
import Network.AWS.VoiceId.Types.FraudsterRegistrationJob
import Network.AWS.VoiceId.Types.FraudsterRegistrationJobStatus
import Network.AWS.VoiceId.Types.FraudsterRegistrationJobSummary
import Network.AWS.VoiceId.Types.InputDataConfig
import Network.AWS.VoiceId.Types.JobProgress
import Network.AWS.VoiceId.Types.KnownFraudsterRisk
import Network.AWS.VoiceId.Types.OutputDataConfig
import Network.AWS.VoiceId.Types.RegistrationConfig
import Network.AWS.VoiceId.Types.ServerSideEncryptionConfiguration
import Network.AWS.VoiceId.Types.Speaker
import Network.AWS.VoiceId.Types.SpeakerEnrollmentJob
import Network.AWS.VoiceId.Types.SpeakerEnrollmentJobStatus
import Network.AWS.VoiceId.Types.SpeakerEnrollmentJobSummary
import Network.AWS.VoiceId.Types.SpeakerStatus
import Network.AWS.VoiceId.Types.SpeakerSummary
import Network.AWS.VoiceId.Types.StreamingStatus
import Network.AWS.VoiceId.Types.Tag

-- | API version @2021-09-27@ of the Amazon Voice ID SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "VoiceId",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "voiceid",
      Core._serviceSigningName = "voiceid",
      Core._serviceVersion = "2021-09-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "VoiceId",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request failed one or more validations; check the error message for
-- more details.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | You do not have sufficient permissions to perform this action. Check the
-- error message and try again.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request failed due to a conflict. Check the @ConflictType@ and error
-- message for more details.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request exceeded the service quota. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html#voiceid-quotas Voice ID Service Quotas>
-- and try your request again.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling. Please slow down your
-- request rate. Refer to
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html##voiceid-api-quotas Amazon Connect Voice ID Service API throttling quotas>
-- and try your request again.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request failed due to an unknown error on the server side.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The specified resource cannot be found. Check the @ResourceType@ and
-- error message for more details.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
