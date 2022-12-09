{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Translate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Provides language translation for input text in the source language to
-- the specified target language.
module Amazonka.Translate
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConflictException
    _ConflictException,

    -- ** DetectedLanguageLowConfidenceException
    _DetectedLanguageLowConfidenceException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidFilterException
    _InvalidFilterException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnsupportedDisplayLanguageCodeException
    _UnsupportedDisplayLanguageCodeException,

    -- ** UnsupportedLanguagePairException
    _UnsupportedLanguagePairException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateParallelData
    CreateParallelData (CreateParallelData'),
    newCreateParallelData,
    CreateParallelDataResponse (CreateParallelDataResponse'),
    newCreateParallelDataResponse,

    -- ** DeleteParallelData
    DeleteParallelData (DeleteParallelData'),
    newDeleteParallelData,
    DeleteParallelDataResponse (DeleteParallelDataResponse'),
    newDeleteParallelDataResponse,

    -- ** DeleteTerminology
    DeleteTerminology (DeleteTerminology'),
    newDeleteTerminology,
    DeleteTerminologyResponse (DeleteTerminologyResponse'),
    newDeleteTerminologyResponse,

    -- ** DescribeTextTranslationJob
    DescribeTextTranslationJob (DescribeTextTranslationJob'),
    newDescribeTextTranslationJob,
    DescribeTextTranslationJobResponse (DescribeTextTranslationJobResponse'),
    newDescribeTextTranslationJobResponse,

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

    -- ** ImportTerminology
    ImportTerminology (ImportTerminology'),
    newImportTerminology,
    ImportTerminologyResponse (ImportTerminologyResponse'),
    newImportTerminologyResponse,

    -- ** ListLanguages
    ListLanguages (ListLanguages'),
    newListLanguages,
    ListLanguagesResponse (ListLanguagesResponse'),
    newListLanguagesResponse,

    -- ** ListParallelData
    ListParallelData (ListParallelData'),
    newListParallelData,
    ListParallelDataResponse (ListParallelDataResponse'),
    newListParallelDataResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTerminologies (Paginated)
    ListTerminologies (ListTerminologies'),
    newListTerminologies,
    ListTerminologiesResponse (ListTerminologiesResponse'),
    newListTerminologiesResponse,

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

    -- ** StopTextTranslationJob
    StopTextTranslationJob (StopTextTranslationJob'),
    newStopTextTranslationJob,
    StopTextTranslationJobResponse (StopTextTranslationJobResponse'),
    newStopTextTranslationJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TranslateText
    TranslateText (TranslateText'),
    newTranslateText,
    TranslateTextResponse (TranslateTextResponse'),
    newTranslateTextResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateParallelData
    UpdateParallelData (UpdateParallelData'),
    newUpdateParallelData,
    UpdateParallelDataResponse (UpdateParallelDataResponse'),
    newUpdateParallelDataResponse,

    -- * Types

    -- ** Directionality
    Directionality (..),

    -- ** DisplayLanguageCode
    DisplayLanguageCode (..),

    -- ** EncryptionKeyType
    EncryptionKeyType (..),

    -- ** Formality
    Formality (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** MergeStrategy
    MergeStrategy (..),

    -- ** ParallelDataFormat
    ParallelDataFormat (..),

    -- ** ParallelDataStatus
    ParallelDataStatus (..),

    -- ** Profanity
    Profanity (..),

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

    -- ** Language
    Language (Language'),
    newLanguage,

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

    -- ** Tag
    Tag (Tag'),
    newTag,

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

    -- ** TranslationSettings
    TranslationSettings (TranslationSettings'),
    newTranslationSettings,
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
import Amazonka.Translate.ListLanguages
import Amazonka.Translate.ListParallelData
import Amazonka.Translate.ListTagsForResource
import Amazonka.Translate.ListTerminologies
import Amazonka.Translate.ListTextTranslationJobs
import Amazonka.Translate.StartTextTranslationJob
import Amazonka.Translate.StopTextTranslationJob
import Amazonka.Translate.TagResource
import Amazonka.Translate.TranslateText
import Amazonka.Translate.Types
import Amazonka.Translate.UntagResource
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
