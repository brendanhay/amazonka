{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Operations and objects for transcribing speech to text.
module Network.AWS.Transcribe
  ( -- * Service configuration
    transcribeService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListLanguageModels
    module Network.AWS.Transcribe.ListLanguageModels,

    -- ** GetVocabulary
    module Network.AWS.Transcribe.GetVocabulary,

    -- ** DeleteLanguageModel
    module Network.AWS.Transcribe.DeleteLanguageModel,

    -- ** GetTranscriptionJob
    module Network.AWS.Transcribe.GetTranscriptionJob,

    -- ** StartMedicalTranscriptionJob
    module Network.AWS.Transcribe.StartMedicalTranscriptionJob,

    -- ** DeleteMedicalVocabulary
    module Network.AWS.Transcribe.DeleteMedicalVocabulary,

    -- ** UpdateMedicalVocabulary
    module Network.AWS.Transcribe.UpdateMedicalVocabulary,

    -- ** DeleteTranscriptionJob
    module Network.AWS.Transcribe.DeleteTranscriptionJob,

    -- ** DescribeLanguageModel
    module Network.AWS.Transcribe.DescribeLanguageModel,

    -- ** DeleteMedicalTranscriptionJob
    module Network.AWS.Transcribe.DeleteMedicalTranscriptionJob,

    -- ** DeleteVocabulary
    module Network.AWS.Transcribe.DeleteVocabulary,

    -- ** UpdateVocabulary
    module Network.AWS.Transcribe.UpdateVocabulary,

    -- ** CreateVocabularyFilter
    module Network.AWS.Transcribe.CreateVocabularyFilter,

    -- ** GetMedicalTranscriptionJob
    module Network.AWS.Transcribe.GetMedicalTranscriptionJob,

    -- ** GetVocabularyFilter
    module Network.AWS.Transcribe.GetVocabularyFilter,

    -- ** GetMedicalVocabulary
    module Network.AWS.Transcribe.GetMedicalVocabulary,

    -- ** CreateMedicalVocabulary
    module Network.AWS.Transcribe.CreateMedicalVocabulary,

    -- ** ListMedicalVocabularies
    module Network.AWS.Transcribe.ListMedicalVocabularies,

    -- ** ListTranscriptionJobs
    module Network.AWS.Transcribe.ListTranscriptionJobs,

    -- ** ListMedicalTranscriptionJobs
    module Network.AWS.Transcribe.ListMedicalTranscriptionJobs,

    -- ** DeleteVocabularyFilter
    module Network.AWS.Transcribe.DeleteVocabularyFilter,

    -- ** ListVocabularyFilters
    module Network.AWS.Transcribe.ListVocabularyFilters,

    -- ** UpdateVocabularyFilter
    module Network.AWS.Transcribe.UpdateVocabularyFilter,

    -- ** ListVocabularies
    module Network.AWS.Transcribe.ListVocabularies,

    -- ** CreateVocabulary
    module Network.AWS.Transcribe.CreateVocabulary,

    -- ** CreateLanguageModel
    module Network.AWS.Transcribe.CreateLanguageModel,

    -- ** StartTranscriptionJob
    module Network.AWS.Transcribe.StartTranscriptionJob,

    -- * Types

    -- ** BaseModelName
    BaseModelName (..),

    -- ** CLMLanguageCode
    CLMLanguageCode (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** MediaFormat
    MediaFormat (..),

    -- ** ModelStatus
    ModelStatus (..),

    -- ** OutputLocationType
    OutputLocationType (..),

    -- ** RedactionOutput
    RedactionOutput (..),

    -- ** RedactionType
    RedactionType (..),

    -- ** Specialty
    Specialty (..),

    -- ** TranscriptionJobStatus
    TranscriptionJobStatus (..),

    -- ** Type
    Type (..),

    -- ** VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- ** VocabularyState
    VocabularyState (..),

    -- ** ContentRedaction
    ContentRedaction (..),
    mkContentRedaction,
    crRedactionType,
    crRedactionOutput,

    -- ** InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcTuningDataS3URI,
    idcS3URI,
    idcDataAccessRoleARN,

    -- ** JobExecutionSettings
    JobExecutionSettings (..),
    mkJobExecutionSettings,
    jesDataAccessRoleARN,
    jesAllowDeferredExecution,

    -- ** LanguageModel
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

    -- ** Media
    Media (..),
    mkMedia,
    mMediaFileURI,

    -- ** MedicalTranscript
    MedicalTranscript (..),
    mkMedicalTranscript,
    mtTranscriptFileURI,

    -- ** MedicalTranscriptionJob
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

    -- ** MedicalTranscriptionJobSummary
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

    -- ** MedicalTranscriptionSetting
    MedicalTranscriptionSetting (..),
    mkMedicalTranscriptionSetting,
    mtsVocabularyName,
    mtsMaxAlternatives,
    mtsChannelIdentification,
    mtsShowAlternatives,
    mtsMaxSpeakerLabels,
    mtsShowSpeakerLabels,

    -- ** ModelSettings
    ModelSettings (..),
    mkModelSettings,
    msLanguageModelName,

    -- ** Settings
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

    -- ** Transcript
    Transcript (..),
    mkTranscript,
    tRedactedTranscriptFileURI,
    tTranscriptFileURI,

    -- ** TranscriptionJob
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

    -- ** TranscriptionJobSummary
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

    -- ** VocabularyFilterInfo
    VocabularyFilterInfo (..),
    mkVocabularyFilterInfo,
    vfiLanguageCode,
    vfiLastModifiedTime,
    vfiVocabularyFilterName,

    -- ** VocabularyInfo
    VocabularyInfo (..),
    mkVocabularyInfo,
    viLanguageCode,
    viVocabularyName,
    viLastModifiedTime,
    viVocabularyState,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.CreateLanguageModel
import Network.AWS.Transcribe.CreateMedicalVocabulary
import Network.AWS.Transcribe.CreateVocabulary
import Network.AWS.Transcribe.CreateVocabularyFilter
import Network.AWS.Transcribe.DeleteLanguageModel
import Network.AWS.Transcribe.DeleteMedicalTranscriptionJob
import Network.AWS.Transcribe.DeleteMedicalVocabulary
import Network.AWS.Transcribe.DeleteTranscriptionJob
import Network.AWS.Transcribe.DeleteVocabulary
import Network.AWS.Transcribe.DeleteVocabularyFilter
import Network.AWS.Transcribe.DescribeLanguageModel
import Network.AWS.Transcribe.GetMedicalTranscriptionJob
import Network.AWS.Transcribe.GetMedicalVocabulary
import Network.AWS.Transcribe.GetTranscriptionJob
import Network.AWS.Transcribe.GetVocabulary
import Network.AWS.Transcribe.GetVocabularyFilter
import Network.AWS.Transcribe.ListLanguageModels
import Network.AWS.Transcribe.ListMedicalTranscriptionJobs
import Network.AWS.Transcribe.ListMedicalVocabularies
import Network.AWS.Transcribe.ListTranscriptionJobs
import Network.AWS.Transcribe.ListVocabularies
import Network.AWS.Transcribe.ListVocabularyFilters
import Network.AWS.Transcribe.StartMedicalTranscriptionJob
import Network.AWS.Transcribe.StartTranscriptionJob
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.UpdateMedicalVocabulary
import Network.AWS.Transcribe.UpdateVocabulary
import Network.AWS.Transcribe.UpdateVocabularyFilter
import Network.AWS.Transcribe.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Transcribe'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
