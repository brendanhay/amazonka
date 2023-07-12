{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _ConflictException,
    _InternalFailureException,
    _LimitExceededException,
    _NotFoundException,

    -- * BaseModelName
    BaseModelName (..),

    -- * CLMLanguageCode
    CLMLanguageCode (..),

    -- * CallAnalyticsJobStatus
    CallAnalyticsJobStatus (..),

    -- * InputType
    InputType (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * MediaFormat
    MediaFormat (..),

    -- * MedicalContentIdentificationType
    MedicalContentIdentificationType (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * OutputLocationType
    OutputLocationType (..),

    -- * ParticipantRole
    ParticipantRole (..),

    -- * PiiEntityType
    PiiEntityType (..),

    -- * RedactionOutput
    RedactionOutput (..),

    -- * RedactionType
    RedactionType (..),

    -- * SentimentValue
    SentimentValue (..),

    -- * Specialty
    Specialty (..),

    -- * SubtitleFormat
    SubtitleFormat (..),

    -- * TranscriptFilterType
    TranscriptFilterType (..),

    -- * TranscriptionJobStatus
    TranscriptionJobStatus (..),

    -- * Type
    Type (..),

    -- * VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- * VocabularyState
    VocabularyState (..),

    -- * AbsoluteTimeRange
    AbsoluteTimeRange (..),
    newAbsoluteTimeRange,
    absoluteTimeRange_endTime,
    absoluteTimeRange_first,
    absoluteTimeRange_last,
    absoluteTimeRange_startTime,

    -- * CallAnalyticsJob
    CallAnalyticsJob (..),
    newCallAnalyticsJob,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_channelDefinitions,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_media,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_mediaSampleRateHertz,
    callAnalyticsJob_settings,
    callAnalyticsJob_startTime,
    callAnalyticsJob_transcript,

    -- * CallAnalyticsJobSettings
    CallAnalyticsJobSettings (..),
    newCallAnalyticsJobSettings,
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_languageIdSettings,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_languageOptions,
    callAnalyticsJobSettings_vocabularyFilterMethod,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_vocabularyName,

    -- * CallAnalyticsJobSummary
    CallAnalyticsJobSummary (..),
    newCallAnalyticsJobSummary,
    callAnalyticsJobSummary_callAnalyticsJobName,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_failureReason,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_startTime,

    -- * CategoryProperties
    CategoryProperties (..),
    newCategoryProperties,
    categoryProperties_categoryName,
    categoryProperties_createTime,
    categoryProperties_inputType,
    categoryProperties_lastUpdateTime,
    categoryProperties_rules,

    -- * ChannelDefinition
    ChannelDefinition (..),
    newChannelDefinition,
    channelDefinition_channelId,
    channelDefinition_participantRole,

    -- * ContentRedaction
    ContentRedaction (..),
    newContentRedaction,
    contentRedaction_piiEntityTypes,
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- * InterruptionFilter
    InterruptionFilter (..),
    newInterruptionFilter,
    interruptionFilter_absoluteTimeRange,
    interruptionFilter_negate,
    interruptionFilter_participantRole,
    interruptionFilter_relativeTimeRange,
    interruptionFilter_threshold,

    -- * JobExecutionSettings
    JobExecutionSettings (..),
    newJobExecutionSettings,
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- * LanguageCodeItem
    LanguageCodeItem (..),
    newLanguageCodeItem,
    languageCodeItem_durationInSeconds,
    languageCodeItem_languageCode,

    -- * LanguageIdSettings
    LanguageIdSettings (..),
    newLanguageIdSettings,
    languageIdSettings_languageModelName,
    languageIdSettings_vocabularyFilterName,
    languageIdSettings_vocabularyName,

    -- * LanguageModel
    LanguageModel (..),
    newLanguageModel,
    languageModel_baseModelName,
    languageModel_createTime,
    languageModel_failureReason,
    languageModel_inputDataConfig,
    languageModel_languageCode,
    languageModel_lastModifiedTime,
    languageModel_modelName,
    languageModel_modelStatus,
    languageModel_upgradeAvailability,

    -- * Media
    Media (..),
    newMedia,
    media_mediaFileUri,
    media_redactedMediaFileUri,

    -- * MedicalTranscript
    MedicalTranscript (..),
    newMedicalTranscript,
    medicalTranscript_transcriptFileUri,

    -- * MedicalTranscriptionJob
    MedicalTranscriptionJob (..),
    newMedicalTranscriptionJob,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_settings,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_type,

    -- * MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (..),
    newMedicalTranscriptionJobSummary,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_contentIdentificationType,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_failureReason,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_type,

    -- * MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    newMedicalTranscriptionSetting,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_maxSpeakerLabels,
    medicalTranscriptionSetting_showAlternatives,
    medicalTranscriptionSetting_showSpeakerLabels,
    medicalTranscriptionSetting_vocabularyName,

    -- * ModelSettings
    ModelSettings (..),
    newModelSettings,
    modelSettings_languageModelName,

    -- * NonTalkTimeFilter
    NonTalkTimeFilter (..),
    newNonTalkTimeFilter,
    nonTalkTimeFilter_absoluteTimeRange,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_relativeTimeRange,
    nonTalkTimeFilter_threshold,

    -- * RelativeTimeRange
    RelativeTimeRange (..),
    newRelativeTimeRange,
    relativeTimeRange_endPercentage,
    relativeTimeRange_first,
    relativeTimeRange_last,
    relativeTimeRange_startPercentage,

    -- * Rule
    Rule (..),
    newRule,
    rule_interruptionFilter,
    rule_nonTalkTimeFilter,
    rule_sentimentFilter,
    rule_transcriptFilter,

    -- * SentimentFilter
    SentimentFilter (..),
    newSentimentFilter,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_negate,
    sentimentFilter_participantRole,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_sentiments,

    -- * Settings
    Settings (..),
    newSettings,
    settings_channelIdentification,
    settings_maxAlternatives,
    settings_maxSpeakerLabels,
    settings_showAlternatives,
    settings_showSpeakerLabels,
    settings_vocabularyFilterMethod,
    settings_vocabularyFilterName,
    settings_vocabularyName,

    -- * Subtitles
    Subtitles (..),
    newSubtitles,
    subtitles_formats,
    subtitles_outputStartIndex,

    -- * SubtitlesOutput
    SubtitlesOutput (..),
    newSubtitlesOutput,
    subtitlesOutput_formats,
    subtitlesOutput_outputStartIndex,
    subtitlesOutput_subtitleFileUris,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Transcript
    Transcript (..),
    newTranscript,
    transcript_redactedTranscriptFileUri,
    transcript_transcriptFileUri,

    -- * TranscriptFilter
    TranscriptFilter (..),
    newTranscriptFilter,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_negate,
    transcriptFilter_participantRole,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- * TranscriptionJob
    TranscriptionJob (..),
    newTranscriptionJob,
    transcriptionJob_completionTime,
    transcriptionJob_contentRedaction,
    transcriptionJob_creationTime,
    transcriptionJob_failureReason,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_identifyLanguage,
    transcriptionJob_identifyMultipleLanguages,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_languageCode,
    transcriptionJob_languageCodes,
    transcriptionJob_languageIdSettings,
    transcriptionJob_languageOptions,
    transcriptionJob_media,
    transcriptionJob_mediaFormat,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_modelSettings,
    transcriptionJob_settings,
    transcriptionJob_startTime,
    transcriptionJob_subtitles,
    transcriptionJob_tags,
    transcriptionJob_transcript,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_transcriptionJobStatus,

    -- * TranscriptionJobSummary
    TranscriptionJobSummary (..),
    newTranscriptionJobSummary,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_failureReason,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_identifyMultipleLanguages,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_languageCodes,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_transcriptionJobStatus,

    -- * VocabularyFilterInfo
    VocabularyFilterInfo (..),
    newVocabularyFilterInfo,
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_lastModifiedTime,
    vocabularyFilterInfo_vocabularyFilterName,

    -- * VocabularyInfo
    VocabularyInfo (..),
    newVocabularyInfo,
    vocabularyInfo_languageCode,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_vocabularyName,
    vocabularyInfo_vocabularyState,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.BaseModelName
import Amazonka.Transcribe.Types.CLMLanguageCode
import Amazonka.Transcribe.Types.CallAnalyticsJob
import Amazonka.Transcribe.Types.CallAnalyticsJobSettings
import Amazonka.Transcribe.Types.CallAnalyticsJobStatus
import Amazonka.Transcribe.Types.CallAnalyticsJobSummary
import Amazonka.Transcribe.Types.CategoryProperties
import Amazonka.Transcribe.Types.ChannelDefinition
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.InputDataConfig
import Amazonka.Transcribe.Types.InputType
import Amazonka.Transcribe.Types.InterruptionFilter
import Amazonka.Transcribe.Types.JobExecutionSettings
import Amazonka.Transcribe.Types.LanguageCode
import Amazonka.Transcribe.Types.LanguageCodeItem
import Amazonka.Transcribe.Types.LanguageIdSettings
import Amazonka.Transcribe.Types.LanguageModel
import Amazonka.Transcribe.Types.Media
import Amazonka.Transcribe.Types.MediaFormat
import Amazonka.Transcribe.Types.MedicalContentIdentificationType
import Amazonka.Transcribe.Types.MedicalTranscript
import Amazonka.Transcribe.Types.MedicalTranscriptionJob
import Amazonka.Transcribe.Types.MedicalTranscriptionJobSummary
import Amazonka.Transcribe.Types.MedicalTranscriptionSetting
import Amazonka.Transcribe.Types.ModelSettings
import Amazonka.Transcribe.Types.ModelStatus
import Amazonka.Transcribe.Types.NonTalkTimeFilter
import Amazonka.Transcribe.Types.OutputLocationType
import Amazonka.Transcribe.Types.ParticipantRole
import Amazonka.Transcribe.Types.PiiEntityType
import Amazonka.Transcribe.Types.RedactionOutput
import Amazonka.Transcribe.Types.RedactionType
import Amazonka.Transcribe.Types.RelativeTimeRange
import Amazonka.Transcribe.Types.Rule
import Amazonka.Transcribe.Types.SentimentFilter
import Amazonka.Transcribe.Types.SentimentValue
import Amazonka.Transcribe.Types.Settings
import Amazonka.Transcribe.Types.Specialty
import Amazonka.Transcribe.Types.SubtitleFormat
import Amazonka.Transcribe.Types.Subtitles
import Amazonka.Transcribe.Types.SubtitlesOutput
import Amazonka.Transcribe.Types.Tag
import Amazonka.Transcribe.Types.Transcript
import Amazonka.Transcribe.Types.TranscriptFilter
import Amazonka.Transcribe.Types.TranscriptFilterType
import Amazonka.Transcribe.Types.TranscriptionJob
import Amazonka.Transcribe.Types.TranscriptionJobStatus
import Amazonka.Transcribe.Types.TranscriptionJobSummary
import Amazonka.Transcribe.Types.Type
import Amazonka.Transcribe.Types.VocabularyFilterInfo
import Amazonka.Transcribe.Types.VocabularyFilterMethod
import Amazonka.Transcribe.Types.VocabularyInfo
import Amazonka.Transcribe.Types.VocabularyState

-- | API version @2017-10-26@ of the Amazon Transcribe Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Transcribe",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "transcribe",
      Core.signingName = "transcribe",
      Core.version = "2017-10-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Transcribe",
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

-- | Your request didn\'t pass one or more validation tests. This can occur
-- when the entity you\'re trying to delete doesn\'t exist or if it\'s in a
-- non-terminal state (such as @IN PROGRESS@). See the exception message
-- field for more information.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | A resource already exists with this name. Resource names must be unique
-- within an Amazon Web Services account.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | There was an internal error. Check the error message, correct the issue,
-- and try your request again.
_InternalFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"

-- | You\'ve either sent too many requests or your input file is too long.
-- Wait before retrying your request, or use a smaller file and try your
-- request again.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | We can\'t find the requested resource. Check that the specified name is
-- correct and try your request again.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
