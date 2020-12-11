{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides translation between one source language and another of the same set of languages.
module Network.AWS.Translate
  ( -- * Service configuration
    translateService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeTextTranslationJob
    module Network.AWS.Translate.DescribeTextTranslationJob,

    -- ** ListTerminologies (Paginated)
    module Network.AWS.Translate.ListTerminologies,

    -- ** CreateParallelData
    module Network.AWS.Translate.CreateParallelData,

    -- ** UpdateParallelData
    module Network.AWS.Translate.UpdateParallelData,

    -- ** DeleteParallelData
    module Network.AWS.Translate.DeleteParallelData,

    -- ** GetParallelData
    module Network.AWS.Translate.GetParallelData,

    -- ** GetTerminology
    module Network.AWS.Translate.GetTerminology,

    -- ** TranslateText
    module Network.AWS.Translate.TranslateText,

    -- ** ImportTerminology
    module Network.AWS.Translate.ImportTerminology,

    -- ** StopTextTranslationJob
    module Network.AWS.Translate.StopTextTranslationJob,

    -- ** DeleteTerminology
    module Network.AWS.Translate.DeleteTerminology,

    -- ** ListTextTranslationJobs
    module Network.AWS.Translate.ListTextTranslationJobs,

    -- ** StartTextTranslationJob
    module Network.AWS.Translate.StartTextTranslationJob,

    -- ** ListParallelData
    module Network.AWS.Translate.ListParallelData,

    -- * Types

    -- ** EncryptionKeyType
    EncryptionKeyType (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** MergeStrategy
    MergeStrategy (..),

    -- ** ParallelDataFormat
    ParallelDataFormat (..),

    -- ** ParallelDataStatus
    ParallelDataStatus (..),

    -- ** TerminologyDataFormat
    TerminologyDataFormat (..),

    -- ** AppliedTerminology
    AppliedTerminology (..),
    mkAppliedTerminology,
    atTerms,
    atName,

    -- ** EncryptionKey
    EncryptionKey (..),
    mkEncryptionKey,
    ekType,
    ekId,

    -- ** InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcS3URI,
    idcContentType,

    -- ** JobDetails
    JobDetails (..),
    mkJobDetails,
    jdTranslatedDocumentsCount,
    jdDocumentsWithErrorsCount,
    jdInputDocumentsCount,

    -- ** OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcS3URI,

    -- ** ParallelDataConfig
    ParallelDataConfig (..),
    mkParallelDataConfig,
    pdcS3URI,
    pdcFormat,

    -- ** ParallelDataDataLocation
    ParallelDataDataLocation (..),
    mkParallelDataDataLocation,
    pddlRepositoryType,
    pddlLocation,

    -- ** ParallelDataProperties
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

    -- ** Term
    Term (..),
    mkTerm,
    tTargetText,
    tSourceText,

    -- ** TerminologyData
    TerminologyData (..),
    mkTerminologyData,
    tdFile,
    tdFormat,

    -- ** TerminologyDataLocation
    TerminologyDataLocation (..),
    mkTerminologyDataLocation,
    tdlRepositoryType,
    tdlLocation,

    -- ** TerminologyProperties
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

    -- ** TextTranslationJobFilter
    TextTranslationJobFilter (..),
    mkTextTranslationJobFilter,
    ttjfSubmittedBeforeTime,
    ttjfSubmittedAfterTime,
    ttjfJobName,
    ttjfJobStatus,

    -- ** TextTranslationJobProperties
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

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.CreateParallelData
import Network.AWS.Translate.DeleteParallelData
import Network.AWS.Translate.DeleteTerminology
import Network.AWS.Translate.DescribeTextTranslationJob
import Network.AWS.Translate.GetParallelData
import Network.AWS.Translate.GetTerminology
import Network.AWS.Translate.ImportTerminology
import Network.AWS.Translate.ListParallelData
import Network.AWS.Translate.ListTerminologies
import Network.AWS.Translate.ListTextTranslationJobs
import Network.AWS.Translate.StartTextTranslationJob
import Network.AWS.Translate.StopTextTranslationJob
import Network.AWS.Translate.TranslateText
import Network.AWS.Translate.Types
import Network.AWS.Translate.UpdateParallelData
import Network.AWS.Translate.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Translate'.

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
