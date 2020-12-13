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
    transcribeService,

    -- * Errors

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
    mkContentRedaction,
    crRedactionOutput,
    crRedactionType,

    -- * InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcTuningDataS3URI,
    idcDataAccessRoleARN,
    idcS3URI,

    -- * JobExecutionSettings
    JobExecutionSettings (..),
    mkJobExecutionSettings,
    jesDataAccessRoleARN,
    jesAllowDeferredExecution,

    -- * LanguageModel
    LanguageModel (..),
    mkLanguageModel,
    lmFailureReason,
    lmLanguageCode,
    lmModelName,
    lmLastModifiedTime,
    lmUpgradeAvailability,
    lmInputDataConfig,
    lmBaseModelName,
    lmModelStatus,
    lmCreateTime,

    -- * Media
    Media (..),
    mkMedia,
    mMediaFileURI,

    -- * MedicalTranscript
    MedicalTranscript (..),
    mkMedicalTranscript,
    mtTranscriptFileURI,

    -- * MedicalTranscriptionJob
    MedicalTranscriptionJob (..),
    mkMedicalTranscriptionJob,
    mtjCreationTime,
    mtjSpecialty,
    mtjFailureReason,
    mtjLanguageCode,
    mtjSettings,
    mtjStartTime,
    mtjCompletionTime,
    mtjMedia,
    mtjMediaFormat,
    mtjMedicalTranscriptionJobName,
    mtjTranscriptionJobStatus,
    mtjType,
    mtjTranscript,
    mtjMediaSampleRateHertz,

    -- * MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (..),
    mkMedicalTranscriptionJobSummary,
    mtjsCreationTime,
    mtjsSpecialty,
    mtjsFailureReason,
    mtjsLanguageCode,
    mtjsOutputLocationType,
    mtjsStartTime,
    mtjsCompletionTime,
    mtjsMedicalTranscriptionJobName,
    mtjsTranscriptionJobStatus,
    mtjsType,

    -- * MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    mkMedicalTranscriptionSetting,
    mtsVocabularyName,
    mtsMaxAlternatives,
    mtsChannelIdentification,
    mtsShowAlternatives,
    mtsMaxSpeakerLabels,
    mtsShowSpeakerLabels,

    -- * ModelSettings
    ModelSettings (..),
    mkModelSettings,
    msLanguageModelName,

    -- * Settings
    Settings (..),
    mkSettings,
    sVocabularyName,
    sMaxAlternatives,
    sChannelIdentification,
    sShowAlternatives,
    sMaxSpeakerLabels,
    sVocabularyFilterName,
    sShowSpeakerLabels,
    sVocabularyFilterMethod,

    -- * Transcript
    Transcript (..),
    mkTranscript,
    tRedactedTranscriptFileURI,
    tTranscriptFileURI,

    -- * TranscriptionJob
    TranscriptionJob (..),
    mkTranscriptionJob,
    tjCreationTime,
    tjFailureReason,
    tjContentRedaction,
    tjIdentifiedLanguageScore,
    tjLanguageCode,
    tjLanguageOptions,
    tjSettings,
    tjStartTime,
    tjCompletionTime,
    tjMedia,
    tjMediaFormat,
    tjModelSettings,
    tjTranscriptionJobStatus,
    tjJobExecutionSettings,
    tjTranscriptionJobName,
    tjIdentifyLanguage,
    tjTranscript,
    tjMediaSampleRateHertz,

    -- * TranscriptionJobSummary
    TranscriptionJobSummary (..),
    mkTranscriptionJobSummary,
    tjsCreationTime,
    tjsFailureReason,
    tjsContentRedaction,
    tjsIdentifiedLanguageScore,
    tjsLanguageCode,
    tjsOutputLocationType,
    tjsStartTime,
    tjsCompletionTime,
    tjsModelSettings,
    tjsTranscriptionJobStatus,
    tjsTranscriptionJobName,
    tjsIdentifyLanguage,

    -- * VocabularyFilterInfo
    VocabularyFilterInfo (..),
    mkVocabularyFilterInfo,
    vfiLanguageCode,
    vfiLastModifiedTime,
    vfiVocabularyFilterName,

    -- * VocabularyInfo
    VocabularyInfo (..),
    mkVocabularyInfo,
    viLanguageCode,
    viVocabularyName,
    viLastModifiedTime,
    viVocabularyState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
transcribeService :: Lude.Service
transcribeService =
  Lude.Service
    { Lude._svcAbbrev = "Transcribe",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "transcribe",
      Lude._svcVersion = "2017-10-26",
      Lude._svcEndpoint = Lude.defaultEndpoint transcribeService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Transcribe",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
