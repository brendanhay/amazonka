{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides translation between one source language and another of the same
-- set of languages.
module Network.AWS.Translate
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidFilterException
    _InvalidFilterException,

    -- ** DetectedLanguageLowConfidenceException
    _DetectedLanguageLowConfidenceException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** UnsupportedLanguagePairException
    _UnsupportedLanguagePairException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateParallelData
    CreateParallelData (CreateParallelData'),
    newCreateParallelData,
    CreateParallelDataResponse (CreateParallelDataResponse'),
    newCreateParallelDataResponse,

    -- ** DescribeTextTranslationJob
    DescribeTextTranslationJob (DescribeTextTranslationJob'),
    newDescribeTextTranslationJob,
    DescribeTextTranslationJobResponse (DescribeTextTranslationJobResponse'),
    newDescribeTextTranslationJobResponse,

    -- ** StopTextTranslationJob
    StopTextTranslationJob (StopTextTranslationJob'),
    newStopTextTranslationJob,
    StopTextTranslationJobResponse (StopTextTranslationJobResponse'),
    newStopTextTranslationJobResponse,

    -- ** StartTextTranslationJob
    StartTextTranslationJob (StartTextTranslationJob'),
    newStartTextTranslationJob,
    StartTextTranslationJobResponse (StartTextTranslationJobResponse'),
    newStartTextTranslationJobResponse,

    -- ** ImportTerminology
    ImportTerminology (ImportTerminology'),
    newImportTerminology,
    ImportTerminologyResponse (ImportTerminologyResponse'),
    newImportTerminologyResponse,

    -- ** ListTextTranslationJobs
    ListTextTranslationJobs (ListTextTranslationJobs'),
    newListTextTranslationJobs,
    ListTextTranslationJobsResponse (ListTextTranslationJobsResponse'),
    newListTextTranslationJobsResponse,

    -- ** GetParallelData
    GetParallelData (GetParallelData'),
    newGetParallelData,
    GetParallelDataResponse (GetParallelDataResponse'),
    newGetParallelDataResponse,

    -- ** DeleteParallelData
    DeleteParallelData (DeleteParallelData'),
    newDeleteParallelData,
    DeleteParallelDataResponse (DeleteParallelDataResponse'),
    newDeleteParallelDataResponse,

    -- ** UpdateParallelData
    UpdateParallelData (UpdateParallelData'),
    newUpdateParallelData,
    UpdateParallelDataResponse (UpdateParallelDataResponse'),
    newUpdateParallelDataResponse,

    -- ** DeleteTerminology
    DeleteTerminology (DeleteTerminology'),
    newDeleteTerminology,
    DeleteTerminologyResponse (DeleteTerminologyResponse'),
    newDeleteTerminologyResponse,

    -- ** ListTerminologies (Paginated)
    ListTerminologies (ListTerminologies'),
    newListTerminologies,
    ListTerminologiesResponse (ListTerminologiesResponse'),
    newListTerminologiesResponse,

    -- ** ListParallelData
    ListParallelData (ListParallelData'),
    newListParallelData,
    ListParallelDataResponse (ListParallelDataResponse'),
    newListParallelDataResponse,

    -- ** TranslateText
    TranslateText (TranslateText'),
    newTranslateText,
    TranslateTextResponse (TranslateTextResponse'),
    newTranslateTextResponse,

    -- ** GetTerminology
    GetTerminology (GetTerminology'),
    newGetTerminology,
    GetTerminologyResponse (GetTerminologyResponse'),
    newGetTerminologyResponse,

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
    AppliedTerminology (AppliedTerminology'),
    newAppliedTerminology,

    -- ** EncryptionKey
    EncryptionKey (EncryptionKey'),
    newEncryptionKey,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** JobDetails
    JobDetails (JobDetails'),
    newJobDetails,

    -- ** OutputDataConfig
    OutputDataConfig (OutputDataConfig'),
    newOutputDataConfig,

    -- ** ParallelDataConfig
    ParallelDataConfig (ParallelDataConfig'),
    newParallelDataConfig,

    -- ** ParallelDataDataLocation
    ParallelDataDataLocation (ParallelDataDataLocation'),
    newParallelDataDataLocation,

    -- ** ParallelDataProperties
    ParallelDataProperties (ParallelDataProperties'),
    newParallelDataProperties,

    -- ** Term
    Term (Term'),
    newTerm,

    -- ** TerminologyData
    TerminologyData (TerminologyData'),
    newTerminologyData,

    -- ** TerminologyDataLocation
    TerminologyDataLocation (TerminologyDataLocation'),
    newTerminologyDataLocation,

    -- ** TerminologyProperties
    TerminologyProperties (TerminologyProperties'),
    newTerminologyProperties,

    -- ** TextTranslationJobFilter
    TextTranslationJobFilter (TextTranslationJobFilter'),
    newTextTranslationJobFilter,

    -- ** TextTranslationJobProperties
    TextTranslationJobProperties (TextTranslationJobProperties'),
    newTextTranslationJobProperties,
  )
where

import Network.AWS.Translate.CreateParallelData
import Network.AWS.Translate.DeleteParallelData
import Network.AWS.Translate.DeleteTerminology
import Network.AWS.Translate.DescribeTextTranslationJob
import Network.AWS.Translate.GetParallelData
import Network.AWS.Translate.GetTerminology
import Network.AWS.Translate.ImportTerminology
import Network.AWS.Translate.Lens
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
