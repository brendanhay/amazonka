{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Translate.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _UnsupportedLanguagePairException,
    _DetectedLanguageLowConfidenceException,
    _ConflictException,
    _InvalidParameterValueException,
    _TooManyRequestsException,
    _ConcurrentModificationException,
    _InternalServerException,
    _ServiceUnavailableException,
    _InvalidFilterException,
    _ResourceNotFoundException,
    _TextSizeLimitExceededException,
    _LimitExceededException,

    -- * EncryptionKeyType
    EncryptionKeyType (..),

    -- * JobStatus
    JobStatus (..),

    -- * MergeStrategy
    MergeStrategy (..),

    -- * ParallelDataFormat
    ParallelDataFormat (..),

    -- * ParallelDataStatus
    ParallelDataStatus (..),

    -- * TerminologyDataFormat
    TerminologyDataFormat (..),

    -- * AppliedTerminology
    AppliedTerminology (..),
    newAppliedTerminology,
    appliedTerminology_terms,
    appliedTerminology_name,

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
    jobDetails_translatedDocumentsCount,
    jobDetails_documentsWithErrorsCount,
    jobDetails_inputDocumentsCount,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
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
    parallelDataProperties_status,
    parallelDataProperties_lastUpdatedAt,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_arn,
    parallelDataProperties_targetLanguageCodes,
    parallelDataProperties_createdAt,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_importedDataSize,
    parallelDataProperties_name,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_message,
    parallelDataProperties_description,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_parallelDataConfig,

    -- * Term
    Term (..),
    newTerm,
    term_targetText,
    term_sourceText,

    -- * TerminologyData
    TerminologyData (..),
    newTerminologyData,
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
    terminologyProperties_sizeBytes,
    terminologyProperties_lastUpdatedAt,
    terminologyProperties_arn,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_createdAt,
    terminologyProperties_name,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_termCount,
    terminologyProperties_encryptionKey,
    terminologyProperties_description,

    -- * TextTranslationJobFilter
    TextTranslationJobFilter (..),
    newTextTranslationJobFilter,
    textTranslationJobFilter_submittedBeforeTime,
    textTranslationJobFilter_submittedAfterTime,
    textTranslationJobFilter_jobName,
    textTranslationJobFilter_jobStatus,

    -- * TextTranslationJobProperties
    TextTranslationJobProperties (..),
    newTextTranslationJobProperties,
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_parallelDataNames,
    textTranslationJobProperties_terminologyNames,
    textTranslationJobProperties_sourceLanguageCode,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_message,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Translate.Types.AppliedTerminology
import Amazonka.Translate.Types.EncryptionKey
import Amazonka.Translate.Types.EncryptionKeyType
import Amazonka.Translate.Types.InputDataConfig
import Amazonka.Translate.Types.JobDetails
import Amazonka.Translate.Types.JobStatus
import Amazonka.Translate.Types.MergeStrategy
import Amazonka.Translate.Types.OutputDataConfig
import Amazonka.Translate.Types.ParallelDataConfig
import Amazonka.Translate.Types.ParallelDataDataLocation
import Amazonka.Translate.Types.ParallelDataFormat
import Amazonka.Translate.Types.ParallelDataProperties
import Amazonka.Translate.Types.ParallelDataStatus
import Amazonka.Translate.Types.Term
import Amazonka.Translate.Types.TerminologyData
import Amazonka.Translate.Types.TerminologyDataFormat
import Amazonka.Translate.Types.TerminologyDataLocation
import Amazonka.Translate.Types.TerminologyProperties
import Amazonka.Translate.Types.TextTranslationJobFilter
import Amazonka.Translate.Types.TextTranslationJobProperties

-- | API version @2017-07-01@ of the Amazon Translate SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Translate",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "translate",
      Core._serviceSigningName = "translate",
      Core._serviceVersion = "2017-07-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Translate",
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

-- | The request that you made is invalid. Check your request to determine
-- why it\'s invalid and then retry the request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Amazon Translate does not support translation from the language of the
-- source text into the requested target language. For more information,
-- see how-to-error-msg.
_UnsupportedLanguagePairException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguagePairException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguagePairException"

-- | The confidence that Amazon Comprehend accurately detected the source
-- language is low. If a low confidence level is acceptable for your
-- application, you can use the language in the exception to call Amazon
-- Translate again. For more information, see the
-- <https://docs.aws.amazon.com/comprehend/latest/dg/API_DetectDominantLanguage.html DetectDominantLanguage>
-- operation in the /Amazon Comprehend Developer Guide/.
_DetectedLanguageLowConfidenceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DetectedLanguageLowConfidenceException =
  Core._MatchServiceError
    defaultService
    "DetectedLanguageLowConfidenceException"

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The value of the parameter is invalid. Review the value of the parameter
-- you are using to correct it, and then retry your operation.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | You have made too many requests within a short period of time. Wait for
-- a short time and then try your request again.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | Another modification is being made. That modification must complete
-- before you can make your change.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The Amazon Translate service is temporarily unavailable. Please wait a
-- bit and then retry your request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The filter specified for the operation is invalid. Specify a different
-- filter.
_InvalidFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidFilterException"

-- | The resource you are looking for has not been found. Review the resource
-- you\'re looking for and see if a different resource will accomplish your
-- needs before retrying the revised request.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The size of the text you submitted exceeds the size limit. Reduce the
-- size of the text or use a smaller document and then retry your request.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | The specified limit has been exceeded. Review your request and retry it
-- with a quantity below the stated limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
