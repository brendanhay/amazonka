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
    translateService,

    -- * Errors

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
    mkAppliedTerminology,
    atTerms,
    atName,

    -- * EncryptionKey
    EncryptionKey (..),
    mkEncryptionKey,
    ekId,
    ekType,

    -- * InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcS3URI,
    idcContentType,

    -- * JobDetails
    JobDetails (..),
    mkJobDetails,
    jdTranslatedDocumentsCount,
    jdDocumentsWithErrorsCount,
    jdInputDocumentsCount,

    -- * OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcS3URI,

    -- * ParallelDataConfig
    ParallelDataConfig (..),
    mkParallelDataConfig,
    pdcFormat,
    pdcS3URI,

    -- * ParallelDataDataLocation
    ParallelDataDataLocation (..),
    mkParallelDataDataLocation,
    pddlLocation,
    pddlRepositoryType,

    -- * ParallelDataProperties
    ParallelDataProperties (..),
    mkParallelDataProperties,
    pdpStatus,
    pdpLastUpdatedAt,
    pdpImportedRecordCount,
    pdpARN,
    pdpTargetLanguageCodes,
    pdpCreatedAt,
    pdpFailedRecordCount,
    pdpImportedDataSize,
    pdpName,
    pdpSourceLanguageCode,
    pdpLatestUpdateAttemptAt,
    pdpEncryptionKey,
    pdpLatestUpdateAttemptStatus,
    pdpMessage,
    pdpDescription,
    pdpSkippedRecordCount,
    pdpParallelDataConfig,

    -- * Term
    Term (..),
    mkTerm,
    tTargetText,
    tSourceText,

    -- * TerminologyData
    TerminologyData (..),
    mkTerminologyData,
    tdFormat,
    tdFile,

    -- * TerminologyDataLocation
    TerminologyDataLocation (..),
    mkTerminologyDataLocation,
    tdlLocation,
    tdlRepositoryType,

    -- * TerminologyProperties
    TerminologyProperties (..),
    mkTerminologyProperties,
    tpSizeBytes,
    tpLastUpdatedAt,
    tpARN,
    tpTargetLanguageCodes,
    tpCreatedAt,
    tpName,
    tpSourceLanguageCode,
    tpTermCount,
    tpEncryptionKey,
    tpDescription,

    -- * TextTranslationJobFilter
    TextTranslationJobFilter (..),
    mkTextTranslationJobFilter,
    ttjfSubmittedBeforeTime,
    ttjfSubmittedAfterTime,
    ttjfJobName,
    ttjfJobStatus,

    -- * TextTranslationJobProperties
    TextTranslationJobProperties (..),
    mkTextTranslationJobProperties,
    ttjpJobId,
    ttjpTargetLanguageCodes,
    ttjpJobName,
    ttjpSubmittedTime,
    ttjpInputDataConfig,
    ttjpParallelDataNames,
    ttjpTerminologyNames,
    ttjpSourceLanguageCode,
    ttjpEndTime,
    ttjpOutputDataConfig,
    ttjpJobDetails,
    ttjpDataAccessRoleARN,
    ttjpJobStatus,
    ttjpMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
translateService :: Lude.Service
translateService =
  Lude.Service
    { Lude._svcAbbrev = "Translate",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "translate",
      Lude._svcVersion = "2017-07-01",
      Lude._svcEndpoint = Lude.defaultEndpoint translateService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Translate",
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
