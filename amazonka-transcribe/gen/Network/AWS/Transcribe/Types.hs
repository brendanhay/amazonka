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
    _ConflictException,
    _LimitExceededException,
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
    absoluteTimeRange_startTime,
    absoluteTimeRange_endTime,
    absoluteTimeRange_last,
    absoluteTimeRange_first,

    -- * CallAnalyticsJob
    CallAnalyticsJob (..),
    newCallAnalyticsJob,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_media,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_transcript,
    callAnalyticsJob_startTime,
    callAnalyticsJob_channelDefinitions,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_mediaSampleRateHertz,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_settings,

    -- * CallAnalyticsJobSettings
    CallAnalyticsJobSettings (..),
    newCallAnalyticsJobSettings,
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_vocabularyFilterMethod,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_vocabularyName,
    callAnalyticsJobSettings_languageOptions,

    -- * CallAnalyticsJobSummary
    CallAnalyticsJobSummary (..),
    newCallAnalyticsJobSummary,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_startTime,
    callAnalyticsJobSummary_callAnalyticsJobName,
    callAnalyticsJobSummary_failureReason,

    -- * CategoryProperties
    CategoryProperties (..),
    newCategoryProperties,
    categoryProperties_lastUpdateTime,
    categoryProperties_rules,
    categoryProperties_categoryName,
    categoryProperties_createTime,

    -- * ChannelDefinition
    ChannelDefinition (..),
    newChannelDefinition,
    channelDefinition_channelId,
    channelDefinition_participantRole,

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

    -- * InterruptionFilter
    InterruptionFilter (..),
    newInterruptionFilter,
    interruptionFilter_threshold,
    interruptionFilter_relativeTimeRange,
    interruptionFilter_negate,
    interruptionFilter_participantRole,
    interruptionFilter_absoluteTimeRange,

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
    languageModel_createTime,
    languageModel_upgradeAvailability,
    languageModel_lastModifiedTime,
    languageModel_modelName,
    languageModel_baseModelName,

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
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_tags,
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
    medicalTranscriptionJobSummary_contentIdentificationType,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_startTime,
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

    -- * NonTalkTimeFilter
    NonTalkTimeFilter (..),
    newNonTalkTimeFilter,
    nonTalkTimeFilter_threshold,
    nonTalkTimeFilter_relativeTimeRange,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_absoluteTimeRange,

    -- * RelativeTimeRange
    RelativeTimeRange (..),
    newRelativeTimeRange,
    relativeTimeRange_startPercentage,
    relativeTimeRange_endPercentage,
    relativeTimeRange_last,
    relativeTimeRange_first,

    -- * Rule
    Rule (..),
    newRule,
    rule_sentimentFilter,
    rule_interruptionFilter,
    rule_transcriptFilter,
    rule_nonTalkTimeFilter,

    -- * SentimentFilter
    SentimentFilter (..),
    newSentimentFilter,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_negate,
    sentimentFilter_participantRole,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_sentiments,

    -- * Settings
    Settings (..),
    newSettings,
    settings_vocabularyFilterName,
    settings_vocabularyFilterMethod,
    settings_showAlternatives,
    settings_channelIdentification,
    settings_maxAlternatives,
    settings_showSpeakerLabels,
    settings_vocabularyName,
    settings_maxSpeakerLabels,

    -- * Subtitles
    Subtitles (..),
    newSubtitles,
    subtitles_formats,

    -- * SubtitlesOutput
    SubtitlesOutput (..),
    newSubtitlesOutput,
    subtitlesOutput_subtitleFileUris,
    subtitlesOutput_formats,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Transcript
    Transcript (..),
    newTranscript,
    transcript_transcriptFileUri,
    transcript_redactedTranscriptFileUri,

    -- * TranscriptFilter
    TranscriptFilter (..),
    newTranscriptFilter,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_negate,
    transcriptFilter_participantRole,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- * TranscriptionJob
    TranscriptionJob (..),
    newTranscriptionJob,
    transcriptionJob_languageCode,
    transcriptionJob_mediaFormat,
    transcriptionJob_contentRedaction,
    transcriptionJob_creationTime,
    transcriptionJob_media,
    transcriptionJob_completionTime,
    transcriptionJob_transcript,
    transcriptionJob_identifyLanguage,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_startTime,
    transcriptionJob_modelSettings,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_subtitles,
    transcriptionJob_failureReason,
    transcriptionJob_tags,
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
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_modelSettings,
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
    vocabularyInfo_vocabularyName,
    vocabularyInfo_vocabularyState,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Transcribe.Types.AbsoluteTimeRange
import Network.AWS.Transcribe.Types.BaseModelName
import Network.AWS.Transcribe.Types.CLMLanguageCode
import Network.AWS.Transcribe.Types.CallAnalyticsJob
import Network.AWS.Transcribe.Types.CallAnalyticsJobSettings
import Network.AWS.Transcribe.Types.CallAnalyticsJobStatus
import Network.AWS.Transcribe.Types.CallAnalyticsJobSummary
import Network.AWS.Transcribe.Types.CategoryProperties
import Network.AWS.Transcribe.Types.ChannelDefinition
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.InterruptionFilter
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.LanguageModel
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.MedicalContentIdentificationType
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionJob
import Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.ModelStatus
import Network.AWS.Transcribe.Types.NonTalkTimeFilter
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.ParticipantRole
import Network.AWS.Transcribe.Types.RedactionOutput
import Network.AWS.Transcribe.Types.RedactionType
import Network.AWS.Transcribe.Types.RelativeTimeRange
import Network.AWS.Transcribe.Types.Rule
import Network.AWS.Transcribe.Types.SentimentFilter
import Network.AWS.Transcribe.Types.SentimentValue
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.SubtitleFormat
import Network.AWS.Transcribe.Types.Subtitles
import Network.AWS.Transcribe.Types.SubtitlesOutput
import Network.AWS.Transcribe.Types.Tag
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptFilter
import Network.AWS.Transcribe.Types.TranscriptFilterType
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
      Core._serviceTimeout = Prelude.Just 70,
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
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | We can\'t find the requested resource. Check the name and try your
-- request again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | Your request didn\'t pass one or more validation tests. For example, if
-- the entity that you\'re trying to delete doesn\'t exist or if it is in a
-- non-terminal state (for example, it\'s \"in progress\"). See the
-- exception @Message@ field for more information.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | There is already a resource with that name.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Either you have sent too many requests or your input file is too long.
-- Wait before you resend your request, or use a smaller file and resend
-- the request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | There was an internal error. Check the error message and try your
-- request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
