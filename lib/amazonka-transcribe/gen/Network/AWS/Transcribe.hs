{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

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
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** ConflictException
    , _ConflictException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListLanguageModels 
    , module Network.AWS.Transcribe.ListLanguageModels

    -- ** GetVocabulary 
    , module Network.AWS.Transcribe.GetVocabulary

    -- ** DeleteLanguageModel 
    , module Network.AWS.Transcribe.DeleteLanguageModel

    -- ** GetTranscriptionJob 
    , module Network.AWS.Transcribe.GetTranscriptionJob

    -- ** StartMedicalTranscriptionJob 
    , module Network.AWS.Transcribe.StartMedicalTranscriptionJob

    -- ** DeleteMedicalVocabulary 
    , module Network.AWS.Transcribe.DeleteMedicalVocabulary

    -- ** UpdateMedicalVocabulary 
    , module Network.AWS.Transcribe.UpdateMedicalVocabulary

    -- ** DeleteTranscriptionJob 
    , module Network.AWS.Transcribe.DeleteTranscriptionJob

    -- ** DescribeLanguageModel 
    , module Network.AWS.Transcribe.DescribeLanguageModel

    -- ** DeleteMedicalTranscriptionJob 
    , module Network.AWS.Transcribe.DeleteMedicalTranscriptionJob

    -- ** DeleteVocabulary 
    , module Network.AWS.Transcribe.DeleteVocabulary

    -- ** UpdateVocabulary 
    , module Network.AWS.Transcribe.UpdateVocabulary

    -- ** CreateVocabularyFilter 
    , module Network.AWS.Transcribe.CreateVocabularyFilter

    -- ** GetMedicalTranscriptionJob 
    , module Network.AWS.Transcribe.GetMedicalTranscriptionJob

    -- ** GetVocabularyFilter 
    , module Network.AWS.Transcribe.GetVocabularyFilter

    -- ** GetMedicalVocabulary 
    , module Network.AWS.Transcribe.GetMedicalVocabulary

    -- ** CreateMedicalVocabulary 
    , module Network.AWS.Transcribe.CreateMedicalVocabulary

    -- ** ListMedicalVocabularies 
    , module Network.AWS.Transcribe.ListMedicalVocabularies

    -- ** ListTranscriptionJobs 
    , module Network.AWS.Transcribe.ListTranscriptionJobs

    -- ** ListMedicalTranscriptionJobs 
    , module Network.AWS.Transcribe.ListMedicalTranscriptionJobs

    -- ** DeleteVocabularyFilter 
    , module Network.AWS.Transcribe.DeleteVocabularyFilter

    -- ** ListVocabularyFilters 
    , module Network.AWS.Transcribe.ListVocabularyFilters

    -- ** UpdateVocabularyFilter 
    , module Network.AWS.Transcribe.UpdateVocabularyFilter

    -- ** ListVocabularies 
    , module Network.AWS.Transcribe.ListVocabularies

    -- ** CreateVocabulary 
    , module Network.AWS.Transcribe.CreateVocabulary

    -- ** CreateLanguageModel 
    , module Network.AWS.Transcribe.CreateLanguageModel

    -- ** StartTranscriptionJob 
    , module Network.AWS.Transcribe.StartTranscriptionJob

    -- * Types

    -- ** Specialty
    , Specialty (..)

    -- ** FailureReason
    , FailureReason (..)

    -- ** ContentRedaction
    , ContentRedaction (..)
    , mkContentRedaction
    , crRedactionType
    , crRedactionOutput

    -- ** VocabularyFilterInfo
    , VocabularyFilterInfo (..)
    , mkVocabularyFilterInfo
    , vfiLanguageCode
    , vfiLastModifiedTime
    , vfiVocabularyFilterName

    -- ** LanguageCode
    , LanguageCode (..)

    -- ** OutputLocationType
    , OutputLocationType (..)

    -- ** RedactionOutput
    , RedactionOutput (..)

    -- ** MedicalTranscriptionJob
    , MedicalTranscriptionJob (..)
    , mkMedicalTranscriptionJob
    , mtjCompletionTime
    , mtjCreationTime
    , mtjFailureReason
    , mtjLanguageCode
    , mtjMedia
    , mtjMediaFormat
    , mtjMediaSampleRateHertz
    , mtjMedicalTranscriptionJobName
    , mtjSettings
    , mtjSpecialty
    , mtjStartTime
    , mtjTranscript
    , mtjTranscriptionJobStatus
    , mtjType

    -- ** LanguageModel
    , LanguageModel (..)
    , mkLanguageModel
    , lmBaseModelName
    , lmCreateTime
    , lmFailureReason
    , lmInputDataConfig
    , lmLanguageCode
    , lmLastModifiedTime
    , lmModelName
    , lmModelStatus
    , lmUpgradeAvailability

    -- ** Settings
    , Settings (..)
    , mkSettings
    , sChannelIdentification
    , sMaxAlternatives
    , sMaxSpeakerLabels
    , sShowAlternatives
    , sShowSpeakerLabels
    , sVocabularyFilterMethod
    , sVocabularyFilterName
    , sVocabularyName

    -- ** VocabularyName
    , VocabularyName (..)

    -- ** RedactionType
    , RedactionType (..)

    -- ** TranscriptionJobSummary
    , TranscriptionJobSummary (..)
    , mkTranscriptionJobSummary
    , tjsCompletionTime
    , tjsContentRedaction
    , tjsCreationTime
    , tjsFailureReason
    , tjsIdentifiedLanguageScore
    , tjsIdentifyLanguage
    , tjsLanguageCode
    , tjsModelSettings
    , tjsOutputLocationType
    , tjsStartTime
    , tjsTranscriptionJobName
    , tjsTranscriptionJobStatus

    -- ** ModelName
    , ModelName (..)

    -- ** MedicalTranscriptionSetting
    , MedicalTranscriptionSetting (..)
    , mkMedicalTranscriptionSetting
    , mtsChannelIdentification
    , mtsMaxAlternatives
    , mtsMaxSpeakerLabels
    , mtsShowAlternatives
    , mtsShowSpeakerLabels
    , mtsVocabularyName

    -- ** OutputBucketName
    , OutputBucketName (..)

    -- ** Uri
    , Uri (..)

    -- ** VocabularyInfo
    , VocabularyInfo (..)
    , mkVocabularyInfo
    , viLanguageCode
    , viLastModifiedTime
    , viVocabularyName
    , viVocabularyState

    -- ** NextToken
    , NextToken (..)

    -- ** InputDataConfig
    , InputDataConfig (..)
    , mkInputDataConfig
    , idcS3Uri
    , idcDataAccessRoleArn
    , idcTuningDataS3Uri

    -- ** Media
    , Media (..)
    , mkMedia
    , mMediaFileUri

    -- ** KMSKeyId
    , KMSKeyId (..)

    -- ** MediaFormat
    , MediaFormat (..)

    -- ** ModelSettings
    , ModelSettings (..)
    , mkModelSettings
    , msLanguageModelName

    -- ** VocabularyFilterName
    , VocabularyFilterName (..)

    -- ** BaseModelName
    , BaseModelName (..)

    -- ** CLMLanguageCode
    , CLMLanguageCode (..)

    -- ** TranscriptionJobStatus
    , TranscriptionJobStatus (..)

    -- ** MedicalTranscript
    , MedicalTranscript (..)
    , mkMedicalTranscript
    , mtTranscriptFileUri

    -- ** Phrase
    , Phrase (..)

    -- ** VocabularyFilterMethod
    , VocabularyFilterMethod (..)

    -- ** DataAccessRoleArn
    , DataAccessRoleArn (..)

    -- ** VocabularyState
    , VocabularyState (..)

    -- ** JobExecutionSettings
    , JobExecutionSettings (..)
    , mkJobExecutionSettings
    , jesAllowDeferredExecution
    , jesDataAccessRoleArn

    -- ** Type
    , Type (..)

    -- ** OutputKey
    , OutputKey (..)

    -- ** ModelStatus
    , ModelStatus (..)

    -- ** TranscriptionJobName
    , TranscriptionJobName (..)

    -- ** Word
    , Word (..)

    -- ** Transcript
    , Transcript (..)
    , mkTranscript
    , tRedactedTranscriptFileUri
    , tTranscriptFileUri

    -- ** MedicalTranscriptionJobSummary
    , MedicalTranscriptionJobSummary (..)
    , mkMedicalTranscriptionJobSummary
    , mtjsCompletionTime
    , mtjsCreationTime
    , mtjsFailureReason
    , mtjsLanguageCode
    , mtjsMedicalTranscriptionJobName
    , mtjsOutputLocationType
    , mtjsSpecialty
    , mtjsStartTime
    , mtjsTranscriptionJobStatus
    , mtjsType

    -- ** TranscriptionJob
    , TranscriptionJob (..)
    , mkTranscriptionJob
    , tjCompletionTime
    , tjContentRedaction
    , tjCreationTime
    , tjFailureReason
    , tjIdentifiedLanguageScore
    , tjIdentifyLanguage
    , tjJobExecutionSettings
    , tjLanguageCode
    , tjLanguageOptions
    , tjMedia
    , tjMediaFormat
    , tjMediaSampleRateHertz
    , tjModelSettings
    , tjSettings
    , tjStartTime
    , tjTranscript
    , tjTranscriptionJobName
    , tjTranscriptionJobStatus

    -- ** DownloadUri
    , DownloadUri (..)

    -- ** VocabularyFilterFileUri
    , VocabularyFilterFileUri (..)

    -- ** MedicalTranscriptionJobName
    , MedicalTranscriptionJobName (..)

    -- ** VocabularyFileUri
    , VocabularyFileUri (..)

    -- ** JobNameContains
    , JobNameContains (..)

    -- ** NameContains
    , NameContains (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Waiters
import Network.AWS.Transcribe.ListLanguageModels
import Network.AWS.Transcribe.GetVocabulary
import Network.AWS.Transcribe.DeleteLanguageModel
import Network.AWS.Transcribe.GetTranscriptionJob
import Network.AWS.Transcribe.StartMedicalTranscriptionJob
import Network.AWS.Transcribe.DeleteMedicalVocabulary
import Network.AWS.Transcribe.UpdateMedicalVocabulary
import Network.AWS.Transcribe.DeleteTranscriptionJob
import Network.AWS.Transcribe.DescribeLanguageModel
import Network.AWS.Transcribe.DeleteMedicalTranscriptionJob
import Network.AWS.Transcribe.DeleteVocabulary
import Network.AWS.Transcribe.UpdateVocabulary
import Network.AWS.Transcribe.CreateVocabularyFilter
import Network.AWS.Transcribe.GetMedicalTranscriptionJob
import Network.AWS.Transcribe.GetVocabularyFilter
import Network.AWS.Transcribe.GetMedicalVocabulary
import Network.AWS.Transcribe.CreateMedicalVocabulary
import Network.AWS.Transcribe.ListMedicalVocabularies
import Network.AWS.Transcribe.ListTranscriptionJobs
import Network.AWS.Transcribe.ListMedicalTranscriptionJobs
import Network.AWS.Transcribe.DeleteVocabularyFilter
import Network.AWS.Transcribe.ListVocabularyFilters
import Network.AWS.Transcribe.UpdateVocabularyFilter
import Network.AWS.Transcribe.ListVocabularies
import Network.AWS.Transcribe.CreateVocabulary
import Network.AWS.Transcribe.CreateLanguageModel
import Network.AWS.Transcribe.StartTranscriptionJob
import qualified Network.AWS.Prelude as Lude

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Transcribe'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
