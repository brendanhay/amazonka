{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _LimitExceededException,
    _ConflictException,
    _BadRequestException,
    _InternalFailureException,

    -- * BaseModelName
    BaseModelName (..),

    -- * CLMLanguageCode
    CLMLanguageCode (..),

    -- * CallAnalyticsJobStatus
    CallAnalyticsJobStatus (..),

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
    absoluteTimeRange_last,
    absoluteTimeRange_first,
    absoluteTimeRange_startTime,

    -- * CallAnalyticsJob
    CallAnalyticsJob (..),
    newCallAnalyticsJob,
    callAnalyticsJob_transcript,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_settings,
    callAnalyticsJob_mediaSampleRateHertz,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_startTime,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_media,
    callAnalyticsJob_channelDefinitions,

    -- * CallAnalyticsJobSettings
    CallAnalyticsJobSettings (..),
    newCallAnalyticsJobSettings,
    callAnalyticsJobSettings_vocabularyFilterMethod,
    callAnalyticsJobSettings_vocabularyName,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_languageIdSettings,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_languageOptions,

    -- * CallAnalyticsJobSummary
    CallAnalyticsJobSummary (..),
    newCallAnalyticsJobSummary,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_callAnalyticsJobName,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_startTime,
    callAnalyticsJobSummary_failureReason,

    -- * CategoryProperties
    CategoryProperties (..),
    newCategoryProperties,
    categoryProperties_rules,
    categoryProperties_lastUpdateTime,
    categoryProperties_createTime,
    categoryProperties_categoryName,

    -- * ChannelDefinition
    ChannelDefinition (..),
    newChannelDefinition,
    channelDefinition_participantRole,
    channelDefinition_channelId,

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
    interruptionFilter_negate,
    interruptionFilter_absoluteTimeRange,
    interruptionFilter_participantRole,
    interruptionFilter_threshold,
    interruptionFilter_relativeTimeRange,

    -- * JobExecutionSettings
    JobExecutionSettings (..),
    newJobExecutionSettings,
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- * LanguageCodeItem
    LanguageCodeItem (..),
    newLanguageCodeItem,
    languageCodeItem_languageCode,
    languageCodeItem_durationInSeconds,

    -- * LanguageIdSettings
    LanguageIdSettings (..),
    newLanguageIdSettings,
    languageIdSettings_vocabularyName,
    languageIdSettings_languageModelName,
    languageIdSettings_vocabularyFilterName,

    -- * LanguageModel
    LanguageModel (..),
    newLanguageModel,
    languageModel_modelStatus,
    languageModel_upgradeAvailability,
    languageModel_lastModifiedTime,
    languageModel_languageCode,
    languageModel_modelName,
    languageModel_baseModelName,
    languageModel_createTime,
    languageModel_inputDataConfig,
    languageModel_failureReason,

    -- * Media
    Media (..),
    newMedia,
    media_redactedMediaFileUri,
    media_mediaFileUri,

    -- * MedicalTranscript
    MedicalTranscript (..),
    newMedicalTranscript,
    medicalTranscript_transcriptFileUri,

    -- * MedicalTranscriptionJob
    MedicalTranscriptionJob (..),
    newMedicalTranscriptionJob,
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_settings,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_media,

    -- * MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (..),
    newMedicalTranscriptionJobSummary,
    medicalTranscriptionJobSummary_type,
    medicalTranscriptionJobSummary_contentIdentificationType,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_failureReason,

    -- * MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    newMedicalTranscriptionSetting,
    medicalTranscriptionSetting_vocabularyName,
    medicalTranscriptionSetting_maxSpeakerLabels,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_showSpeakerLabels,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_showAlternatives,

    -- * ModelSettings
    ModelSettings (..),
    newModelSettings,
    modelSettings_languageModelName,

    -- * NonTalkTimeFilter
    NonTalkTimeFilter (..),
    newNonTalkTimeFilter,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_absoluteTimeRange,
    nonTalkTimeFilter_threshold,
    nonTalkTimeFilter_relativeTimeRange,

    -- * RelativeTimeRange
    RelativeTimeRange (..),
    newRelativeTimeRange,
    relativeTimeRange_last,
    relativeTimeRange_endPercentage,
    relativeTimeRange_startPercentage,
    relativeTimeRange_first,

    -- * Rule
    Rule (..),
    newRule,
    rule_transcriptFilter,
    rule_interruptionFilter,
    rule_sentimentFilter,
    rule_nonTalkTimeFilter,

    -- * SentimentFilter
    SentimentFilter (..),
    newSentimentFilter,
    sentimentFilter_negate,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_participantRole,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_sentiments,

    -- * Settings
    Settings (..),
    newSettings,
    settings_vocabularyFilterMethod,
    settings_vocabularyName,
    settings_maxSpeakerLabels,
    settings_maxAlternatives,
    settings_vocabularyFilterName,
    settings_showSpeakerLabels,
    settings_channelIdentification,
    settings_showAlternatives,

    -- * Subtitles
    Subtitles (..),
    newSubtitles,
    subtitles_outputStartIndex,
    subtitles_formats,

    -- * SubtitlesOutput
    SubtitlesOutput (..),
    newSubtitlesOutput,
    subtitlesOutput_outputStartIndex,
    subtitlesOutput_formats,
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
    transcriptFilter_negate,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_participantRole,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- * TranscriptionJob
    TranscriptionJob (..),
    newTranscriptionJob,
    transcriptionJob_tags,
    transcriptionJob_transcript,
    transcriptionJob_identifyMultipleLanguages,
    transcriptionJob_mediaFormat,
    transcriptionJob_identifyLanguage,
    transcriptionJob_contentRedaction,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_completionTime,
    transcriptionJob_subtitles,
    transcriptionJob_languageIdSettings,
    transcriptionJob_settings,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_languageCode,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_creationTime,
    transcriptionJob_modelSettings,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_startTime,
    transcriptionJob_failureReason,
    transcriptionJob_languageOptions,
    transcriptionJob_languageCodes,
    transcriptionJob_media,

    -- * TranscriptionJobSummary
    TranscriptionJobSummary (..),
    newTranscriptionJobSummary,
    transcriptionJobSummary_identifyMultipleLanguages,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_failureReason,
    transcriptionJobSummary_languageCodes,

    -- * VocabularyFilterInfo
    VocabularyFilterInfo (..),
    newVocabularyFilterInfo,
    vocabularyFilterInfo_lastModifiedTime,
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_vocabularyFilterName,

    -- * VocabularyInfo
    VocabularyInfo (..),
    newVocabularyInfo,
    vocabularyInfo_vocabularyName,
    vocabularyInfo_vocabularyState,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_languageCode,
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

-- | We can\'t find the requested resource. Check that the specified name is
-- correct and try your request again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | You\'ve either sent too many requests or your input file is too long.
-- Wait before retrying your request, or use a smaller file and try your
-- request again.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | A resource already exists with this name. Resource names must be unique
-- within an Amazon Web Services account.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Your request didn\'t pass one or more validation tests. This can occur
-- when the entity you\'re trying to delete doesn\'t exist or if it\'s in a
-- non-terminal state (such as @IN PROGRESS@). See the exception message
-- field for more information.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | There was an internal error. Check the error message, correct the issue,
-- and try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
