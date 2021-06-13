{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidFilterException,
    _DetectedLanguageLowConfidenceException,
    _ServiceUnavailableException,
    _ConcurrentModificationException,
    _UnsupportedLanguagePairException,
    _InvalidRequestException,
    _InvalidParameterValueException,
    _LimitExceededException,
    _ConflictException,
    _TextSizeLimitExceededException,
    _ResourceNotFoundException,
    _InternalServerException,
    _TooManyRequestsException,

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
    jobDetails_inputDocumentsCount,
    jobDetails_documentsWithErrorsCount,
    jobDetails_translatedDocumentsCount,

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
    parallelDataProperties_importedDataSize,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_message,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_arn,
    parallelDataProperties_targetLanguageCodes,
    parallelDataProperties_createdAt,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_name,
    parallelDataProperties_parallelDataConfig,
    parallelDataProperties_description,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_lastUpdatedAt,

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
    terminologyProperties_encryptionKey,
    terminologyProperties_arn,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_createdAt,
    terminologyProperties_name,
    terminologyProperties_sizeBytes,
    terminologyProperties_description,
    terminologyProperties_termCount,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_lastUpdatedAt,

    -- * TextTranslationJobFilter
    TextTranslationJobFilter (..),
    newTextTranslationJobFilter,
    textTranslationJobFilter_jobStatus,
    textTranslationJobFilter_submittedAfterTime,
    textTranslationJobFilter_submittedBeforeTime,
    textTranslationJobFilter_jobName,

    -- * TextTranslationJobProperties
    TextTranslationJobProperties (..),
    newTextTranslationJobProperties,
    textTranslationJobProperties_parallelDataNames,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_message,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_terminologyNames,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_sourceLanguageCode,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Translate.Types.AppliedTerminology
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.EncryptionKeyType
import Network.AWS.Translate.Types.InputDataConfig
import Network.AWS.Translate.Types.JobDetails
import Network.AWS.Translate.Types.JobStatus
import Network.AWS.Translate.Types.MergeStrategy
import Network.AWS.Translate.Types.OutputDataConfig
import Network.AWS.Translate.Types.ParallelDataConfig
import Network.AWS.Translate.Types.ParallelDataDataLocation
import Network.AWS.Translate.Types.ParallelDataFormat
import Network.AWS.Translate.Types.ParallelDataProperties
import Network.AWS.Translate.Types.ParallelDataStatus
import Network.AWS.Translate.Types.Term
import Network.AWS.Translate.Types.TerminologyData
import Network.AWS.Translate.Types.TerminologyDataFormat
import Network.AWS.Translate.Types.TerminologyDataLocation
import Network.AWS.Translate.Types.TerminologyProperties
import Network.AWS.Translate.Types.TextTranslationJobFilter
import Network.AWS.Translate.Types.TextTranslationJobProperties

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

-- | The filter specified for the operation is invalid. Specify a different
-- filter.
_InvalidFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidFilterException"

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

-- | The Amazon Translate service is temporarily unavailable. Please wait a
-- bit and then retry your request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | Another modification is being made. That modification must complete
-- before you can make your change.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | Amazon Translate does not support translation from the language of the
-- source text into the requested target language. For more information,
-- see how-to-error-msg.
_UnsupportedLanguagePairException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguagePairException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguagePairException"

-- | The request that you made is invalid. Check your request to determine
-- why it\'s invalid and then retry the request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The value of the parameter is invalid. Review the value of the parameter
-- you are using to correct it, and then retry your operation.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | The specified limit has been exceeded. Review your request and retry it
-- with a quantity below the stated limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The size of the text you submitted exceeds the size limit. Reduce the
-- size of the text or use a smaller document and then retry your request.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | The resource you are looking for has not been found. Review the resource
-- you\'re looking for and see if a different resource will accomplish your
-- needs before retrying the revised request.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | You have made too many requests within a short period of time. Wait for
-- a short time and then try your request again.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
