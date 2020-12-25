-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types
  ( -- * Service configuration
    mkServiceConfig,

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

    -- * IamRoleArn
    IamRoleArn (..),

    -- * ClientTokenString
    ClientTokenString (..),

    -- * TerminologyProperties
    TerminologyProperties (..),
    mkTerminologyProperties,
    tpArn,
    tpCreatedAt,
    tpDescription,
    tpEncryptionKey,
    tpLastUpdatedAt,
    tpName,
    tpSizeBytes,
    tpSourceLanguageCode,
    tpTargetLanguageCodes,
    tpTermCount,

    -- * LanguageCodeString
    LanguageCodeString (..),

    -- * ParallelDataProperties
    ParallelDataProperties (..),
    mkParallelDataProperties,
    pdpArn,
    pdpCreatedAt,
    pdpDescription,
    pdpEncryptionKey,
    pdpFailedRecordCount,
    pdpImportedDataSize,
    pdpImportedRecordCount,
    pdpLastUpdatedAt,
    pdpLatestUpdateAttemptAt,
    pdpLatestUpdateAttemptStatus,
    pdpMessage,
    pdpName,
    pdpParallelDataConfig,
    pdpSkippedRecordCount,
    pdpSourceLanguageCode,
    pdpStatus,
    pdpTargetLanguageCodes,

    -- * JobId
    JobId (..),

    -- * ResourceName
    ResourceName (..),

    -- * String
    String (..),

    -- * EncryptionKeyID
    EncryptionKeyID (..),

    -- * TextTranslationJobProperties
    TextTranslationJobProperties (..),
    mkTextTranslationJobProperties,
    ttjpDataAccessRoleArn,
    ttjpEndTime,
    ttjpInputDataConfig,
    ttjpJobDetails,
    ttjpJobId,
    ttjpJobName,
    ttjpJobStatus,
    ttjpMessage,
    ttjpOutputDataConfig,
    ttjpParallelDataNames,
    ttjpSourceLanguageCode,
    ttjpSubmittedTime,
    ttjpTargetLanguageCodes,
    ttjpTerminologyNames,

    -- * JobName
    JobName (..),

    -- * EncryptionKeyType
    EncryptionKeyType (..),

    -- * ParallelDataDataLocation
    ParallelDataDataLocation (..),
    mkParallelDataDataLocation,
    pddlRepositoryType,
    pddlLocation,

    -- * Term
    Term (..),
    mkTerm,
    tSourceText,
    tTargetText,

    -- * TerminologyDataLocation
    TerminologyDataLocation (..),
    mkTerminologyDataLocation,
    tdlRepositoryType,
    tdlLocation,

    -- * TextTranslationJobFilter
    TextTranslationJobFilter (..),
    mkTextTranslationJobFilter,
    ttjfJobName,
    ttjfJobStatus,
    ttjfSubmittedAfterTime,
    ttjfSubmittedBeforeTime,

    -- * NextToken
    NextToken (..),

    -- * InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcS3Uri,
    idcContentType,

    -- * ParallelDataStatus
    ParallelDataStatus (..),

    -- * OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcS3Uri,

    -- * JobDetails
    JobDetails (..),
    mkJobDetails,
    jdDocumentsWithErrorsCount,
    jdInputDocumentsCount,
    jdTranslatedDocumentsCount,

    -- * EncryptionKey
    EncryptionKey (..),
    mkEncryptionKey,
    ekType,
    ekId,

    -- * TerminologyDataFormat
    TerminologyDataFormat (..),

    -- * ParallelDataFormat
    ParallelDataFormat (..),

    -- * JobStatus
    JobStatus (..),

    -- * MergeStrategy
    MergeStrategy (..),

    -- * TerminologyData
    TerminologyData (..),
    mkTerminologyData,
    tdFile,
    tdFormat,

    -- * Description
    Description (..),

    -- * S3Uri
    S3Uri (..),

    -- * AppliedTerminology
    AppliedTerminology (..),
    mkAppliedTerminology,
    atName,
    atTerms,

    -- * ContentType
    ContentType (..),

    -- * ParallelDataConfig
    ParallelDataConfig (..),
    mkParallelDataConfig,
    pdcS3Uri,
    pdcFormat,

    -- * Name
    Name (..),

    -- * Message
    Message (..),

    -- * Arn
    Arn (..),

    -- * SourceLanguageCode
    SourceLanguageCode (..),

    -- * Text
    Text (..),

    -- * TargetLanguageCode
    TargetLanguageCode (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Translate.Types.AppliedTerminology
import Network.AWS.Translate.Types.Arn
import Network.AWS.Translate.Types.ClientTokenString
import Network.AWS.Translate.Types.ContentType
import Network.AWS.Translate.Types.Description
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.EncryptionKeyID
import Network.AWS.Translate.Types.EncryptionKeyType
import Network.AWS.Translate.Types.IamRoleArn
import Network.AWS.Translate.Types.InputDataConfig
import Network.AWS.Translate.Types.JobDetails
import Network.AWS.Translate.Types.JobId
import Network.AWS.Translate.Types.JobName
import Network.AWS.Translate.Types.JobStatus
import Network.AWS.Translate.Types.LanguageCodeString
import Network.AWS.Translate.Types.MergeStrategy
import Network.AWS.Translate.Types.Message
import Network.AWS.Translate.Types.Name
import Network.AWS.Translate.Types.NextToken
import Network.AWS.Translate.Types.OutputDataConfig
import Network.AWS.Translate.Types.ParallelDataConfig
import Network.AWS.Translate.Types.ParallelDataDataLocation
import Network.AWS.Translate.Types.ParallelDataFormat
import Network.AWS.Translate.Types.ParallelDataProperties
import Network.AWS.Translate.Types.ParallelDataStatus
import Network.AWS.Translate.Types.ResourceName
import Network.AWS.Translate.Types.S3Uri
import Network.AWS.Translate.Types.SourceLanguageCode
import Network.AWS.Translate.Types.String
import Network.AWS.Translate.Types.TargetLanguageCode
import Network.AWS.Translate.Types.Term
import Network.AWS.Translate.Types.TerminologyData
import Network.AWS.Translate.Types.TerminologyDataFormat
import Network.AWS.Translate.Types.TerminologyDataLocation
import Network.AWS.Translate.Types.TerminologyProperties
import Network.AWS.Translate.Types.Text
import Network.AWS.Translate.Types.TextTranslationJobFilter
import Network.AWS.Translate.Types.TextTranslationJobProperties

-- | API version @2017-07-01@ of the Amazon Translate SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Translate",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "translate",
      Core._svcVersion = "2017-07-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Translate",
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

-- | The request that you made is invalid. Check your request to determine why it's invalid and then retry the request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | Amazon Translate does not support translation from the language of the source text into the requested target language. For more information, see 'how-to-error-msg' .
_UnsupportedLanguagePairException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguagePairException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedLanguagePairException"
{-# DEPRECATED _UnsupportedLanguagePairException "Use generic-lens or generic-optics instead." #-}

-- | The confidence that Amazon Comprehend accurately detected the source language is low. If a low confidence level is acceptable for your application, you can use the language in the exception to call Amazon Translate again. For more information, see the <https://docs.aws.amazon.com/comprehend/latest/dg/API_DetectDominantLanguage.html DetectDominantLanguage> operation in the /Amazon Comprehend Developer Guide/ .
_DetectedLanguageLowConfidenceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DetectedLanguageLowConfidenceException =
  Core._MatchServiceError
    mkServiceConfig
    "DetectedLanguageLowConfidenceException"
{-# DEPRECATED _DetectedLanguageLowConfidenceException "Use generic-lens or generic-optics instead." #-}

-- | There was a conflict processing the request. Try your request again.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError mkServiceConfig "ConflictException"
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead." #-}

-- | The value of the parameter is invalid. Review the value of the parameter you are using to correct it, and then retry your operation.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterValueException"
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead." #-}

-- | You have made too many requests within a short period of time. Wait for a short time and then try your request again.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | Another modification is being made. That modification must complete before you can make your change.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError mkServiceConfig "InternalServerException"
{-# DEPRECATED _InternalServerException "Use generic-lens or generic-optics instead." #-}

-- | The Amazon Translate service is temporarily unavailable. Please wait a bit and then retry your request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | The filter specified for the operation is invalid. Specify a different filter.
_InvalidFilterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError mkServiceConfig "InvalidFilterException"
{-# DEPRECATED _InvalidFilterException "Use generic-lens or generic-optics instead." #-}

-- | The resource you are looking for has not been found. Review the resource you're looking for and see if a different resource will accomplish your needs before retrying the revised request.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The size of the text you submitted exceeds the size limit. Reduce the size of the text or use a smaller document and then retry your request.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "TextSizeLimitExceededException"
{-# DEPRECATED _TextSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified limit has been exceeded. Review your request and retry it with a quantity below the stated limit.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}
