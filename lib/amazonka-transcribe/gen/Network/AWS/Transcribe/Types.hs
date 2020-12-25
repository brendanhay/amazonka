-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ConflictException,
    _NotFoundException,
    _InternalFailureException,
    _BadRequestException,
    _LimitExceededException,

    -- * Specialty
    Specialty (..),

    -- * FailureReason
    FailureReason (..),

    -- * ContentRedaction
    ContentRedaction (..),
    mkContentRedaction,
    crRedactionType,
    crRedactionOutput,

    -- * VocabularyFilterInfo
    VocabularyFilterInfo (..),
    mkVocabularyFilterInfo,
    vfiLanguageCode,
    vfiLastModifiedTime,
    vfiVocabularyFilterName,

    -- * LanguageCode
    LanguageCode (..),

    -- * OutputLocationType
    OutputLocationType (..),

    -- * RedactionOutput
    RedactionOutput (..),

    -- * MedicalTranscriptionJob
    MedicalTranscriptionJob (..),
    mkMedicalTranscriptionJob,
    mtjCompletionTime,
    mtjCreationTime,
    mtjFailureReason,
    mtjLanguageCode,
    mtjMedia,
    mtjMediaFormat,
    mtjMediaSampleRateHertz,
    mtjMedicalTranscriptionJobName,
    mtjSettings,
    mtjSpecialty,
    mtjStartTime,
    mtjTranscript,
    mtjTranscriptionJobStatus,
    mtjType,

    -- * LanguageModel
    LanguageModel (..),
    mkLanguageModel,
    lmBaseModelName,
    lmCreateTime,
    lmFailureReason,
    lmInputDataConfig,
    lmLanguageCode,
    lmLastModifiedTime,
    lmModelName,
    lmModelStatus,
    lmUpgradeAvailability,

    -- * Settings
    Settings (..),
    mkSettings,
    sChannelIdentification,
    sMaxAlternatives,
    sMaxSpeakerLabels,
    sShowAlternatives,
    sShowSpeakerLabels,
    sVocabularyFilterMethod,
    sVocabularyFilterName,
    sVocabularyName,

    -- * VocabularyName
    VocabularyName (..),

    -- * RedactionType
    RedactionType (..),

    -- * TranscriptionJobSummary
    TranscriptionJobSummary (..),
    mkTranscriptionJobSummary,
    tjsCompletionTime,
    tjsContentRedaction,
    tjsCreationTime,
    tjsFailureReason,
    tjsIdentifiedLanguageScore,
    tjsIdentifyLanguage,
    tjsLanguageCode,
    tjsModelSettings,
    tjsOutputLocationType,
    tjsStartTime,
    tjsTranscriptionJobName,
    tjsTranscriptionJobStatus,

    -- * ModelName
    ModelName (..),

    -- * MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    mkMedicalTranscriptionSetting,
    mtsChannelIdentification,
    mtsMaxAlternatives,
    mtsMaxSpeakerLabels,
    mtsShowAlternatives,
    mtsShowSpeakerLabels,
    mtsVocabularyName,

    -- * OutputBucketName
    OutputBucketName (..),

    -- * Uri
    Uri (..),

    -- * VocabularyInfo
    VocabularyInfo (..),
    mkVocabularyInfo,
    viLanguageCode,
    viLastModifiedTime,
    viVocabularyName,
    viVocabularyState,

    -- * NextToken
    NextToken (..),

    -- * InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcS3Uri,
    idcDataAccessRoleArn,
    idcTuningDataS3Uri,

    -- * Media
    Media (..),
    mkMedia,
    mMediaFileUri,

    -- * KMSKeyId
    KMSKeyId (..),

    -- * MediaFormat
    MediaFormat (..),

    -- * ModelSettings
    ModelSettings (..),
    mkModelSettings,
    msLanguageModelName,

    -- * VocabularyFilterName
    VocabularyFilterName (..),

    -- * BaseModelName
    BaseModelName (..),

    -- * CLMLanguageCode
    CLMLanguageCode (..),

    -- * TranscriptionJobStatus
    TranscriptionJobStatus (..),

    -- * MedicalTranscript
    MedicalTranscript (..),
    mkMedicalTranscript,
    mtTranscriptFileUri,

    -- * Phrase
    Phrase (..),

    -- * VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- * DataAccessRoleArn
    DataAccessRoleArn (..),

    -- * VocabularyState
    VocabularyState (..),

    -- * JobExecutionSettings
    JobExecutionSettings (..),
    mkJobExecutionSettings,
    jesAllowDeferredExecution,
    jesDataAccessRoleArn,

    -- * Type
    Type (..),

    -- * OutputKey
    OutputKey (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * TranscriptionJobName
    TranscriptionJobName (..),

    -- * Word
    Word (..),

    -- * Transcript
    Transcript (..),
    mkTranscript,
    tRedactedTranscriptFileUri,
    tTranscriptFileUri,

    -- * MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (..),
    mkMedicalTranscriptionJobSummary,
    mtjsCompletionTime,
    mtjsCreationTime,
    mtjsFailureReason,
    mtjsLanguageCode,
    mtjsMedicalTranscriptionJobName,
    mtjsOutputLocationType,
    mtjsSpecialty,
    mtjsStartTime,
    mtjsTranscriptionJobStatus,
    mtjsType,

    -- * TranscriptionJob
    TranscriptionJob (..),
    mkTranscriptionJob,
    tjCompletionTime,
    tjContentRedaction,
    tjCreationTime,
    tjFailureReason,
    tjIdentifiedLanguageScore,
    tjIdentifyLanguage,
    tjJobExecutionSettings,
    tjLanguageCode,
    tjLanguageOptions,
    tjMedia,
    tjMediaFormat,
    tjMediaSampleRateHertz,
    tjModelSettings,
    tjSettings,
    tjStartTime,
    tjTranscript,
    tjTranscriptionJobName,
    tjTranscriptionJobStatus,

    -- * DownloadUri
    DownloadUri (..),

    -- * VocabularyFilterFileUri
    VocabularyFilterFileUri (..),

    -- * MedicalTranscriptionJobName
    MedicalTranscriptionJobName (..),

    -- * VocabularyFileUri
    VocabularyFileUri (..),

    -- * JobNameContains
    JobNameContains (..),

    -- * NameContains
    NameContains (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Transcribe.Types.BaseModelName
import Network.AWS.Transcribe.Types.CLMLanguageCode
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.DataAccessRoleArn
import Network.AWS.Transcribe.Types.DownloadUri
import Network.AWS.Transcribe.Types.FailureReason
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.JobNameContains
import Network.AWS.Transcribe.Types.KMSKeyId
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.LanguageModel
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MediaFormat
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionJob
import Network.AWS.Transcribe.Types.MedicalTranscriptionJobName
import Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.ModelName
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.ModelStatus
import Network.AWS.Transcribe.Types.NameContains
import Network.AWS.Transcribe.Types.NextToken
import Network.AWS.Transcribe.Types.OutputBucketName
import Network.AWS.Transcribe.Types.OutputKey
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.Phrase
import Network.AWS.Transcribe.Types.RedactionOutput
import Network.AWS.Transcribe.Types.RedactionType
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Specialty
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptionJob
import Network.AWS.Transcribe.Types.TranscriptionJobName
import Network.AWS.Transcribe.Types.TranscriptionJobStatus
import Network.AWS.Transcribe.Types.TranscriptionJobSummary
import Network.AWS.Transcribe.Types.Type
import Network.AWS.Transcribe.Types.Uri
import Network.AWS.Transcribe.Types.VocabularyFileUri
import Network.AWS.Transcribe.Types.VocabularyFilterFileUri
import Network.AWS.Transcribe.Types.VocabularyFilterInfo
import Network.AWS.Transcribe.Types.VocabularyFilterMethod
import Network.AWS.Transcribe.Types.VocabularyFilterName
import Network.AWS.Transcribe.Types.VocabularyInfo
import Network.AWS.Transcribe.Types.VocabularyName
import Network.AWS.Transcribe.Types.VocabularyState
import Network.AWS.Transcribe.Types.Word

-- | API version @2017-10-26@ of the Amazon Transcribe Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Transcribe",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "transcribe",
      Core._svcVersion = "2017-10-26",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Transcribe",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | There is already a resource with that name.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | We can't find the requested resource. Check the name and try your request again.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | There was an internal error. Check the error message and try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalFailureException"
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | Your request didn't pass one or more validation tests. For example, if the entity that you're trying to delete doesn't exist or if it is in a non-terminal state (for example, it's "in progress"). See the exception @Message@ field for more information.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}

-- | Either you have sent too many requests or your input file is too long. Wait before you resend your request, or use a smaller file and resend the request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
