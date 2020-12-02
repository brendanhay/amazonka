{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types
  ( -- * Service Configuration
    translate,

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
    AppliedTerminology,
    appliedTerminology,
    atTerms,
    atName,

    -- * EncryptionKey
    EncryptionKey,
    encryptionKey,
    ekType,
    ekId,

    -- * InputDataConfig
    InputDataConfig,
    inputDataConfig,
    idcS3URI,
    idcContentType,

    -- * JobDetails
    JobDetails,
    jobDetails,
    jdTranslatedDocumentsCount,
    jdDocumentsWithErrorsCount,
    jdInputDocumentsCount,

    -- * OutputDataConfig
    OutputDataConfig,
    outputDataConfig,
    odcS3URI,

    -- * ParallelDataConfig
    ParallelDataConfig,
    parallelDataConfig,
    pdcS3URI,
    pdcFormat,

    -- * ParallelDataDataLocation
    ParallelDataDataLocation,
    parallelDataDataLocation,
    pddlRepositoryType,
    pddlLocation,

    -- * ParallelDataProperties
    ParallelDataProperties,
    parallelDataProperties,
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
    Term,
    term,
    tTargetText,
    tSourceText,

    -- * TerminologyData
    TerminologyData,
    terminologyData,
    tdFile,
    tdFormat,

    -- * TerminologyDataLocation
    TerminologyDataLocation,
    terminologyDataLocation,
    tdlRepositoryType,
    tdlLocation,

    -- * TerminologyProperties
    TerminologyProperties,
    terminologyProperties,
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
    TextTranslationJobFilter,
    textTranslationJobFilter,
    ttjfSubmittedBeforeTime,
    ttjfSubmittedAfterTime,
    ttjfJobName,
    ttjfJobStatus,

    -- * TextTranslationJobProperties
    TextTranslationJobProperties,
    textTranslationJobProperties,
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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
translate :: Service
translate =
  Service
    { _svcAbbrev = "Translate",
      _svcSigner = v4,
      _svcPrefix = "translate",
      _svcVersion = "2017-07-01",
      _svcEndpoint = defaultEndpoint translate,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Translate",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
