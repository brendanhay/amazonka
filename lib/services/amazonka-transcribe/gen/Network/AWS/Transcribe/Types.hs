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
    _ConflictException,
    _NotFoundException,
    _InternalFailureException,
    _BadRequestException,
    _LimitExceededException,

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
    absoluteTimeRange_first,
    absoluteTimeRange_startTime,
    absoluteTimeRange_last,
    absoluteTimeRange_endTime,

    -- * CallAnalyticsJob
    CallAnalyticsJob (..),
    newCallAnalyticsJob,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_settings,
    callAnalyticsJob_startTime,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_media,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_channelDefinitions,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_transcript,
    callAnalyticsJob_mediaSampleRateHertz,

    -- * CallAnalyticsJobSettings
    CallAnalyticsJobSettings (..),
    newCallAnalyticsJobSettings,
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_languageOptions,
    callAnalyticsJobSettings_vocabularyName,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_vocabularyFilterMethod,

    -- * CallAnalyticsJobSummary
    CallAnalyticsJobSummary (..),
    newCallAnalyticsJobSummary,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_failureReason,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_startTime,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_callAnalyticsJobName,

    -- * CategoryProperties
    CategoryProperties (..),
    newCategoryProperties,
    categoryProperties_rules,
    categoryProperties_categoryName,
    categoryProperties_lastUpdateTime,
    categoryProperties_createTime,

    -- * ChannelDefinition
    ChannelDefinition (..),
    newChannelDefinition,
    channelDefinition_participantRole,
    channelDefinition_channelId,

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
    interruptionFilter_participantRole,
    interruptionFilter_relativeTimeRange,
    interruptionFilter_negate,
    interruptionFilter_threshold,
    interruptionFilter_absoluteTimeRange,

    -- * JobExecutionSettings
    JobExecutionSettings (..),
    newJobExecutionSettings,
    jobExecutionSettings_dataAccessRoleArn,
    jobExecutionSettings_allowDeferredExecution,

    -- * LanguageModel
    LanguageModel (..),
    newLanguageModel,
    languageModel_failureReason,
    languageModel_languageCode,
    languageModel_modelName,
    languageModel_lastModifiedTime,
    languageModel_upgradeAvailability,
    languageModel_inputDataConfig,
    languageModel_baseModelName,
    languageModel_modelStatus,
    languageModel_createTime,

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
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_settings,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_mediaSampleRateHertz,

    -- * MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (..),
    newMedicalTranscriptionJobSummary,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_failureReason,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_type,
    medicalTranscriptionJobSummary_contentIdentificationType,

    -- * MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    newMedicalTranscriptionSetting,
    medicalTranscriptionSetting_vocabularyName,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_showAlternatives,
    medicalTranscriptionSetting_maxSpeakerLabels,
    medicalTranscriptionSetting_showSpeakerLabels,

    -- * ModelSettings
    ModelSettings (..),
    newModelSettings,
    modelSettings_languageModelName,

    -- * NonTalkTimeFilter
    NonTalkTimeFilter (..),
    newNonTalkTimeFilter,
    nonTalkTimeFilter_relativeTimeRange,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_threshold,
    nonTalkTimeFilter_absoluteTimeRange,

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
    rule_nonTalkTimeFilter,
    rule_transcriptFilter,
    rule_sentimentFilter,
    rule_interruptionFilter,

    -- * SentimentFilter
    SentimentFilter (..),
    newSentimentFilter,
    sentimentFilter_participantRole,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_negate,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_sentiments,

    -- * Settings
    Settings (..),
    newSettings,
    settings_vocabularyName,
    settings_maxAlternatives,
    settings_channelIdentification,
    settings_showAlternatives,
    settings_maxSpeakerLabels,
    settings_vocabularyFilterName,
    settings_showSpeakerLabels,
    settings_vocabularyFilterMethod,

    -- * Subtitles
    Subtitles (..),
    newSubtitles,
    subtitles_formats,

    -- * SubtitlesOutput
    SubtitlesOutput (..),
    newSubtitlesOutput,
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
    transcriptFilter_participantRole,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_negate,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- * TranscriptionJob
    TranscriptionJob (..),
    newTranscriptionJob,
    transcriptionJob_creationTime,
    transcriptionJob_failureReason,
    transcriptionJob_contentRedaction,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_subtitles,
    transcriptionJob_languageCode,
    transcriptionJob_languageOptions,
    transcriptionJob_settings,
    transcriptionJob_startTime,
    transcriptionJob_completionTime,
    transcriptionJob_media,
    transcriptionJob_mediaFormat,
    transcriptionJob_modelSettings,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_identifyLanguage,
    transcriptionJob_transcript,
    transcriptionJob_tags,
    transcriptionJob_mediaSampleRateHertz,

    -- * TranscriptionJobSummary
    TranscriptionJobSummary (..),
    newTranscriptionJobSummary,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_failureReason,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_identifyLanguage,

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
    vocabularyInfo_vocabularyName,
    vocabularyInfo_lastModifiedTime,
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

-- | There is already a resource with that name.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | We can\'t find the requested resource. Check the name and try your
-- request again.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | There was an internal error. Check the error message and try your
-- request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"

-- | Your request didn\'t pass one or more validation tests. For example, if
-- the entity that you\'re trying to delete doesn\'t exist or if it is in a
-- non-terminal state (for example, it\'s \"in progress\"). See the
-- exception @Message@ field for more information.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | Either you have sent too many requests or your input file is too long.
-- Wait before you resend your request, or use a smaller file and resend
-- the request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
