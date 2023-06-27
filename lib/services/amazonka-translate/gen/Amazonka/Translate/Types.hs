{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Translate.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConcurrentModificationException,
    _ConflictException,
    _DetectedLanguageLowConfidenceException,
    _InternalServerException,
    _InvalidFilterException,
    _InvalidParameterValueException,
    _InvalidRequestException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _TextSizeLimitExceededException,
    _TooManyRequestsException,
    _TooManyTagsException,
    _UnsupportedDisplayLanguageCodeException,
    _UnsupportedLanguagePairException,

    -- * Directionality
    Directionality (..),

    -- * DisplayLanguageCode
    DisplayLanguageCode (..),

    -- * EncryptionKeyType
    EncryptionKeyType (..),

    -- * Formality
    Formality (..),

    -- * JobStatus
    JobStatus (..),

    -- * MergeStrategy
    MergeStrategy (..),

    -- * ParallelDataFormat
    ParallelDataFormat (..),

    -- * ParallelDataStatus
    ParallelDataStatus (..),

    -- * Profanity
    Profanity (..),

    -- * TerminologyDataFormat
    TerminologyDataFormat (..),

    -- * AppliedTerminology
    AppliedTerminology (..),
    newAppliedTerminology,
    appliedTerminology_name,
    appliedTerminology_terms,

    -- * Document
    Document (..),
    newDocument,
    document_content,
    document_contentType,

    -- * EncryptionKey
    EncryptionKey (..),
    newEncryptionKey,
    encryptionKey_type,
    encryptionKey_id,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_s3Uri,
    inputDataConfig_contentType,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_documentsWithErrorsCount,
    jobDetails_inputDocumentsCount,
    jobDetails_translatedDocumentsCount,

    -- * Language
    Language (..),
    newLanguage,
    language_languageName,
    language_languageCode,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
    outputDataConfig_encryptionKey,
    outputDataConfig_s3Uri,

    -- * ParallelDataConfig
    ParallelDataConfig (..),
    newParallelDataConfig,
    parallelDataConfig_s3Uri,
    parallelDataConfig_format,

    -- * ParallelDataDataLocation
    ParallelDataDataLocation (..),
    newParallelDataDataLocation,
    parallelDataDataLocation_repositoryType,
    parallelDataDataLocation_location,

    -- * ParallelDataProperties
    ParallelDataProperties (..),
    newParallelDataProperties,
    parallelDataProperties_arn,
    parallelDataProperties_createdAt,
    parallelDataProperties_description,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_importedDataSize,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_lastUpdatedAt,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_message,
    parallelDataProperties_name,
    parallelDataProperties_parallelDataConfig,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_status,
    parallelDataProperties_targetLanguageCodes,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Term
    Term (..),
    newTerm,
    term_sourceText,
    term_targetText,

    -- * TerminologyData
    TerminologyData (..),
    newTerminologyData,
    terminologyData_directionality,
    terminologyData_file,
    terminologyData_format,

    -- * TerminologyDataLocation
    TerminologyDataLocation (..),
    newTerminologyDataLocation,
    terminologyDataLocation_repositoryType,
    terminologyDataLocation_location,

    -- * TerminologyProperties
    TerminologyProperties (..),
    newTerminologyProperties,
    terminologyProperties_arn,
    terminologyProperties_createdAt,
    terminologyProperties_description,
    terminologyProperties_directionality,
    terminologyProperties_encryptionKey,
    terminologyProperties_format,
    terminologyProperties_lastUpdatedAt,
    terminologyProperties_message,
    terminologyProperties_name,
    terminologyProperties_sizeBytes,
    terminologyProperties_skippedTermCount,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_termCount,

    -- * TextTranslationJobFilter
    TextTranslationJobFilter (..),
    newTextTranslationJobFilter,
    textTranslationJobFilter_jobName,
    textTranslationJobFilter_jobStatus,
    textTranslationJobFilter_submittedAfterTime,
    textTranslationJobFilter_submittedBeforeTime,

    -- * TextTranslationJobProperties
    TextTranslationJobProperties (..),
    newTextTranslationJobProperties,
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_message,
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_parallelDataNames,
    textTranslationJobProperties_settings,
    textTranslationJobProperties_sourceLanguageCode,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_terminologyNames,

    -- * TranslatedDocument
    TranslatedDocument (..),
    newTranslatedDocument,
    translatedDocument_content,

    -- * TranslationSettings
    TranslationSettings (..),
    newTranslationSettings,
    translationSettings_formality,
    translationSettings_profanity,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Translate.Types.AppliedTerminology
import Amazonka.Translate.Types.Directionality
import Amazonka.Translate.Types.DisplayLanguageCode
import Amazonka.Translate.Types.Document
import Amazonka.Translate.Types.EncryptionKey
import Amazonka.Translate.Types.EncryptionKeyType
import Amazonka.Translate.Types.Formality
import Amazonka.Translate.Types.InputDataConfig
import Amazonka.Translate.Types.JobDetails
import Amazonka.Translate.Types.JobStatus
import Amazonka.Translate.Types.Language
import Amazonka.Translate.Types.MergeStrategy
import Amazonka.Translate.Types.OutputDataConfig
import Amazonka.Translate.Types.ParallelDataConfig
import Amazonka.Translate.Types.ParallelDataDataLocation
import Amazonka.Translate.Types.ParallelDataFormat
import Amazonka.Translate.Types.ParallelDataProperties
import Amazonka.Translate.Types.ParallelDataStatus
import Amazonka.Translate.Types.Profanity
import Amazonka.Translate.Types.Tag
import Amazonka.Translate.Types.Term
import Amazonka.Translate.Types.TerminologyData
import Amazonka.Translate.Types.TerminologyDataFormat
import Amazonka.Translate.Types.TerminologyDataLocation
import Amazonka.Translate.Types.TerminologyProperties
import Amazonka.Translate.Types.TextTranslationJobFilter
import Amazonka.Translate.Types.TextTranslationJobProperties
import Amazonka.Translate.Types.TranslatedDocument
import Amazonka.Translate.Types.TranslationSettings

-- | API version @2017-07-01@ of the Amazon Translate SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Translate",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "translate",
      Core.signingName = "translate",
      Core.version = "2017-07-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Translate",
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

-- | Another modification is being made. That modification must complete
-- before you can make your change.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The confidence that Amazon Comprehend accurately detected the source
-- language is low. If a low confidence level is acceptable for your
-- application, you can use the language in the exception to call Amazon
-- Translate again. For more information, see the
-- <https://docs.aws.amazon.com/comprehend/latest/dg/API_DetectDominantLanguage.html DetectDominantLanguage>
-- operation in the /Amazon Comprehend Developer Guide/.
_DetectedLanguageLowConfidenceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DetectedLanguageLowConfidenceException =
  Core._MatchServiceError
    defaultService
    "DetectedLanguageLowConfidenceException"

-- | An internal server error occurred. Retry your request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The filter specified for the operation is not valid. Specify a different
-- filter.
_InvalidFilterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidFilterException"

-- | The value of the parameter is not valid. Review the value of the
-- parameter you are using to correct it, and then retry your operation.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | The request that you made is not valid. Check your request to determine
-- why it\'s not valid and then retry the request.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The specified limit has been exceeded. Review your request and retry it
-- with a quantity below the stated limit.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The resource you are looking for has not been found. Review the resource
-- you\'re looking for and see if a different resource will accomplish your
-- needs before retrying the revised request.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The Amazon Translate service is temporarily unavailable. Wait a bit and
-- then retry your request.
_ServiceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The size of the text you submitted exceeds the size limit. Reduce the
-- size of the text or use a smaller document and then retry your request.
_TextSizeLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | You have made too many requests within a short period of time. Wait for
-- a short time and then try your request again.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | You have added too many tags to this resource. The maximum is 50 tags.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | Requested display language code is not supported.
_UnsupportedDisplayLanguageCodeException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedDisplayLanguageCodeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedDisplayLanguageCodeException"

-- | Amazon Translate does not support translation from the language of the
-- source text into the requested target language. For more information,
-- see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is-languages.html Supported languages>.
_UnsupportedLanguagePairException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedLanguagePairException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguagePairException"
