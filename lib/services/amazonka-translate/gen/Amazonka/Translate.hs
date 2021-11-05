{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Translate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Provides translation between one source language and another of the same
-- set of languages.
module Amazonka.Translate
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** UnsupportedLanguagePairException
    _UnsupportedLanguagePairException,

    -- ** DetectedLanguageLowConfidenceException
    _DetectedLanguageLowConfidenceException,

    -- ** ConflictException
    _ConflictException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** InvalidFilterException
    _InvalidFilterException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeTextTranslationJob
    DescribeTextTranslationJob (DescribeTextTranslationJob'),
    newDescribeTextTranslationJob,
    DescribeTextTranslationJobResponse (DescribeTextTranslationJobResponse'),
    newDescribeTextTranslationJobResponse,

    -- ** ListTerminologies (Paginated)
    ListTerminologies (ListTerminologies'),
    newListTerminologies,
    ListTerminologiesResponse (ListTerminologiesResponse'),
    newListTerminologiesResponse,

    -- ** CreateParallelData
    CreateParallelData (CreateParallelData'),
    newCreateParallelData,
    CreateParallelDataResponse (CreateParallelDataResponse'),
    newCreateParallelDataResponse,

    -- ** UpdateParallelData
    UpdateParallelData (UpdateParallelData'),
    newUpdateParallelData,
    UpdateParallelDataResponse (UpdateParallelDataResponse'),
    newUpdateParallelDataResponse,

    -- ** DeleteParallelData
    DeleteParallelData (DeleteParallelData'),
    newDeleteParallelData,
    DeleteParallelDataResponse (DeleteParallelDataResponse'),
    newDeleteParallelDataResponse,

    -- ** GetParallelData
    GetParallelData (GetParallelData'),
    newGetParallelData,
    GetParallelDataResponse (GetParallelDataResponse'),
    newGetParallelDataResponse,

    -- ** GetTerminology
    GetTerminology (GetTerminology'),
    newGetTerminology,
    GetTerminologyResponse (GetTerminologyResponse'),
    newGetTerminologyResponse,

    -- ** TranslateText
    TranslateText (TranslateText'),
    newTranslateText,
    TranslateTextResponse (TranslateTextResponse'),
    newTranslateTextResponse,

    -- ** ImportTerminology
    ImportTerminology (ImportTerminology'),
    newImportTerminology,
    ImportTerminologyResponse (ImportTerminologyResponse'),
    newImportTerminologyResponse,

    -- ** StopTextTranslationJob
    StopTextTranslationJob (StopTextTranslationJob'),
    newStopTextTranslationJob,
    StopTextTranslationJobResponse (StopTextTranslationJobResponse'),
    newStopTextTranslationJobResponse,

    -- ** DeleteTerminology
    DeleteTerminology (DeleteTerminology'),
    newDeleteTerminology,
    DeleteTerminologyResponse (DeleteTerminologyResponse'),
    newDeleteTerminologyResponse,

    -- ** ListTextTranslationJobs
    ListTextTranslationJobs (ListTextTranslationJobs'),
    newListTextTranslationJobs,
    ListTextTranslationJobsResponse (ListTextTranslationJobsResponse'),
    newListTextTranslationJobsResponse,

    -- ** StartTextTranslationJob
    StartTextTranslationJob (StartTextTranslationJob'),
    newStartTextTranslationJob,
    StartTextTranslationJobResponse (StartTextTranslationJobResponse'),
    newStartTextTranslationJobResponse,

    -- ** ListParallelData
    ListParallelData (ListParallelData'),
    newListParallelData,
    ListParallelDataResponse (ListParallelDataResponse'),
    newListParallelDataResponse,

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

import Amazonka.Translate.CreateParallelData
import Amazonka.Translate.DeleteParallelData
import Amazonka.Translate.DeleteTerminology
import Amazonka.Translate.DescribeTextTranslationJob
import Amazonka.Translate.GetParallelData
import Amazonka.Translate.GetTerminology
import Amazonka.Translate.ImportTerminology
import Amazonka.Translate.Lens
import Amazonka.Translate.ListParallelData
import Amazonka.Translate.ListTerminologies
import Amazonka.Translate.ListTextTranslationJobs
import Amazonka.Translate.StartTextTranslationJob
import Amazonka.Translate.StopTextTranslationJob
import Amazonka.Translate.TranslateText
import Amazonka.Translate.Types
import Amazonka.Translate.UpdateParallelData
import Amazonka.Translate.Waiters

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
