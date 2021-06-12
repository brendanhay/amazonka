{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _LimitExceededException,
    _ConflictException,
    _InternalFailureException,

    -- * BaseModelName
    BaseModelName (..),

    -- * CLMLanguageCode
    CLMLanguageCode (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * MediaFormat
    MediaFormat (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * OutputLocationType
    OutputLocationType (..),

    -- * RedactionOutput
    RedactionOutput (..),

    -- * RedactionType
    RedactionType (..),

    -- * Specialty
    Specialty (..),

    -- * TranscriptionJobStatus
    TranscriptionJobStatus (..),

    -- * Type
    Type (..),

    -- * VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- * VocabularyState
    VocabularyState (..),

    -- * ContentRedaction
    ContentRedaction (..),
    newContentRedaction,
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- * JobExecutionSettings
    JobExecutionSettings (..),
    newJobExecutionSettings,
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- * LanguageModel
    LanguageModel (..),
    newLanguageModel,
    languageModel_languageCode,
    languageModel_inputDataConfig,
    languageModel_modelStatus,
    languageModel_failureReason,
    languageModel_upgradeAvailability,
    languageModel_createTime,
    languageModel_lastModifiedTime,
    languageModel_modelName,
    languageModel_baseModelName,

    -- * Media
    Media (..),
    newMedia,
    media_mediaFileUri,

    -- * MedicalTranscript
    MedicalTranscript (..),
    newMedicalTranscript,
    medicalTranscript_transcriptFileUri,

    -- * MedicalTranscriptionJob
    MedicalTranscriptionJob (..),
    newMedicalTranscriptionJob,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_settings,

    -- * MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (..),
    newMedicalTranscriptionJobSummary,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_failureReason,
    medicalTranscriptionJobSummary_type,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,

    -- * MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    newMedicalTranscriptionSetting,
    medicalTranscriptionSetting_showAlternatives,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_showSpeakerLabels,
    medicalTranscriptionSetting_vocabularyName,
    medicalTranscriptionSetting_maxSpeakerLabels,

    -- * ModelSettings
    ModelSettings (..),
    newModelSettings,
    modelSettings_languageModelName,

    -- * Settings
    Settings (..),
    newSettings,
    settings_vocabularyFilterMethod,
    settings_vocabularyFilterName,
    settings_showAlternatives,
    settings_channelIdentification,
    settings_maxAlternatives,
    settings_showSpeakerLabels,
    settings_vocabularyName,
    settings_maxSpeakerLabels,

    -- * Transcript
    Transcript (..),
    newTranscript,
    transcript_transcriptFileUri,
    transcript_redactedTranscriptFileUri,

    -- * TranscriptionJob
    TranscriptionJob (..),
    newTranscriptionJob,
    transcriptionJob_languageCode,
    transcriptionJob_mediaFormat,
    transcriptionJob_contentRedaction,
    transcriptionJob_media,
    transcriptionJob_creationTime,
    transcriptionJob_completionTime,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_transcript,
    transcriptionJob_identifyLanguage,
    transcriptionJob_startTime,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_modelSettings,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_failureReason,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_settings,
    transcriptionJob_languageOptions,

    -- * TranscriptionJobSummary
    TranscriptionJobSummary (..),
    newTranscriptionJobSummary,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_failureReason,

    -- * VocabularyFilterInfo
    VocabularyFilterInfo (..),
    newVocabularyFilterInfo,
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_vocabularyFilterName,
    vocabularyFilterInfo_lastModifiedTime,

    -- * VocabularyInfo
    VocabularyInfo (..),
    newVocabularyInfo,
    vocabularyInfo_languageCode,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_vocabularyState,
    vocabularyInfo_vocabularyName,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Transcribe.Types.BaseModelName
import Network.AWS.Transcribe.Types.CLMLanguageCode
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.LanguageModel
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionJob
import Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.ModelStatus
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.RedactionOutput
import Network.AWS.Transcribe.Types.RedactionType
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptionJob
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.TranscriptionJobSummary
import Network.AWS.Transcribe.Types.Type
import Network.AWS.Transcribe.Types.VocabularyFilterInfo
import Network.AWS.Transcribe.Types.VocabularyFilterMethod
import Network.AWS.Transcribe.Types.VocabularyInfo
import Network.AWS.Transcribe.Types.VocabularyState

-- | API version @2017-10-26@ of the Amazon Transcribe Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Transcribe",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "transcribe",
      Core._serviceSigningName = "transcribe",
      Core._serviceVersion = "2017-10-26",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Transcribe",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | We can\'t find the requested resource. Check the name and try your
-- request again.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | Your request didn\'t pass one or more validation tests. For example, if
-- the entity that you\'re trying to delete doesn\'t exist or if it is in a
-- non-terminal state (for example, it\'s \"in progress\"). See the
-- exception @Message@ field for more information.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | Either you have sent too many requests or your input file is too long.
-- Wait before you resend your request, or use a smaller file and resend
-- the request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | There is already a resource with that name.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | There was an internal error. Check the error message and try your
-- request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
