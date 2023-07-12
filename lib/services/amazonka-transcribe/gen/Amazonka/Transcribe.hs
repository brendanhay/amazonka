{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Transcribe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-10-26@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Transcribe offers three main types of batch transcription:
-- __Standard__, __Medical__, and __Call Analytics__.
--
-- -   __Standard transcriptions__ are the most common option. Refer to for
--     details.
--
-- -   __Medical transcriptions__ are tailored to medical professionals and
--     incorporate medical terms. A common use case for this service is
--     transcribing doctor-patient dialogue into after-visit notes. Refer
--     to for details.
--
-- -   __Call Analytics transcriptions__ are designed for use with call
--     center audio on two different channels; if you\'re looking for
--     insight into customer service calls, use this option. Refer to for
--     details.
module Amazonka.Transcribe
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateCallAnalyticsCategory
    CreateCallAnalyticsCategory (CreateCallAnalyticsCategory'),
    newCreateCallAnalyticsCategory,
    CreateCallAnalyticsCategoryResponse (CreateCallAnalyticsCategoryResponse'),
    newCreateCallAnalyticsCategoryResponse,

    -- ** CreateLanguageModel
    CreateLanguageModel (CreateLanguageModel'),
    newCreateLanguageModel,
    CreateLanguageModelResponse (CreateLanguageModelResponse'),
    newCreateLanguageModelResponse,

    -- ** CreateMedicalVocabulary
    CreateMedicalVocabulary (CreateMedicalVocabulary'),
    newCreateMedicalVocabulary,
    CreateMedicalVocabularyResponse (CreateMedicalVocabularyResponse'),
    newCreateMedicalVocabularyResponse,

    -- ** CreateVocabulary
    CreateVocabulary (CreateVocabulary'),
    newCreateVocabulary,
    CreateVocabularyResponse (CreateVocabularyResponse'),
    newCreateVocabularyResponse,

    -- ** CreateVocabularyFilter
    CreateVocabularyFilter (CreateVocabularyFilter'),
    newCreateVocabularyFilter,
    CreateVocabularyFilterResponse (CreateVocabularyFilterResponse'),
    newCreateVocabularyFilterResponse,

    -- ** DeleteCallAnalyticsCategory
    DeleteCallAnalyticsCategory (DeleteCallAnalyticsCategory'),
    newDeleteCallAnalyticsCategory,
    DeleteCallAnalyticsCategoryResponse (DeleteCallAnalyticsCategoryResponse'),
    newDeleteCallAnalyticsCategoryResponse,

    -- ** DeleteCallAnalyticsJob
    DeleteCallAnalyticsJob (DeleteCallAnalyticsJob'),
    newDeleteCallAnalyticsJob,
    DeleteCallAnalyticsJobResponse (DeleteCallAnalyticsJobResponse'),
    newDeleteCallAnalyticsJobResponse,

    -- ** DeleteLanguageModel
    DeleteLanguageModel (DeleteLanguageModel'),
    newDeleteLanguageModel,
    DeleteLanguageModelResponse (DeleteLanguageModelResponse'),
    newDeleteLanguageModelResponse,

    -- ** DeleteMedicalTranscriptionJob
    DeleteMedicalTranscriptionJob (DeleteMedicalTranscriptionJob'),
    newDeleteMedicalTranscriptionJob,
    DeleteMedicalTranscriptionJobResponse (DeleteMedicalTranscriptionJobResponse'),
    newDeleteMedicalTranscriptionJobResponse,

    -- ** DeleteMedicalVocabulary
    DeleteMedicalVocabulary (DeleteMedicalVocabulary'),
    newDeleteMedicalVocabulary,
    DeleteMedicalVocabularyResponse (DeleteMedicalVocabularyResponse'),
    newDeleteMedicalVocabularyResponse,

    -- ** DeleteTranscriptionJob
    DeleteTranscriptionJob (DeleteTranscriptionJob'),
    newDeleteTranscriptionJob,
    DeleteTranscriptionJobResponse (DeleteTranscriptionJobResponse'),
    newDeleteTranscriptionJobResponse,

    -- ** DeleteVocabulary
    DeleteVocabulary (DeleteVocabulary'),
    newDeleteVocabulary,
    DeleteVocabularyResponse (DeleteVocabularyResponse'),
    newDeleteVocabularyResponse,

    -- ** DeleteVocabularyFilter
    DeleteVocabularyFilter (DeleteVocabularyFilter'),
    newDeleteVocabularyFilter,
    DeleteVocabularyFilterResponse (DeleteVocabularyFilterResponse'),
    newDeleteVocabularyFilterResponse,

    -- ** DescribeLanguageModel
    DescribeLanguageModel (DescribeLanguageModel'),
    newDescribeLanguageModel,
    DescribeLanguageModelResponse (DescribeLanguageModelResponse'),
    newDescribeLanguageModelResponse,

    -- ** GetCallAnalyticsCategory
    GetCallAnalyticsCategory (GetCallAnalyticsCategory'),
    newGetCallAnalyticsCategory,
    GetCallAnalyticsCategoryResponse (GetCallAnalyticsCategoryResponse'),
    newGetCallAnalyticsCategoryResponse,

    -- ** GetCallAnalyticsJob
    GetCallAnalyticsJob (GetCallAnalyticsJob'),
    newGetCallAnalyticsJob,
    GetCallAnalyticsJobResponse (GetCallAnalyticsJobResponse'),
    newGetCallAnalyticsJobResponse,

    -- ** GetMedicalTranscriptionJob
    GetMedicalTranscriptionJob (GetMedicalTranscriptionJob'),
    newGetMedicalTranscriptionJob,
    GetMedicalTranscriptionJobResponse (GetMedicalTranscriptionJobResponse'),
    newGetMedicalTranscriptionJobResponse,

    -- ** GetMedicalVocabulary
    GetMedicalVocabulary (GetMedicalVocabulary'),
    newGetMedicalVocabulary,
    GetMedicalVocabularyResponse (GetMedicalVocabularyResponse'),
    newGetMedicalVocabularyResponse,

    -- ** GetTranscriptionJob
    GetTranscriptionJob (GetTranscriptionJob'),
    newGetTranscriptionJob,
    GetTranscriptionJobResponse (GetTranscriptionJobResponse'),
    newGetTranscriptionJobResponse,

    -- ** GetVocabulary
    GetVocabulary (GetVocabulary'),
    newGetVocabulary,
    GetVocabularyResponse (GetVocabularyResponse'),
    newGetVocabularyResponse,

    -- ** GetVocabularyFilter
    GetVocabularyFilter (GetVocabularyFilter'),
    newGetVocabularyFilter,
    GetVocabularyFilterResponse (GetVocabularyFilterResponse'),
    newGetVocabularyFilterResponse,

    -- ** ListCallAnalyticsCategories
    ListCallAnalyticsCategories (ListCallAnalyticsCategories'),
    newListCallAnalyticsCategories,
    ListCallAnalyticsCategoriesResponse (ListCallAnalyticsCategoriesResponse'),
    newListCallAnalyticsCategoriesResponse,

    -- ** ListCallAnalyticsJobs
    ListCallAnalyticsJobs (ListCallAnalyticsJobs'),
    newListCallAnalyticsJobs,
    ListCallAnalyticsJobsResponse (ListCallAnalyticsJobsResponse'),
    newListCallAnalyticsJobsResponse,

    -- ** ListLanguageModels
    ListLanguageModels (ListLanguageModels'),
    newListLanguageModels,
    ListLanguageModelsResponse (ListLanguageModelsResponse'),
    newListLanguageModelsResponse,

    -- ** ListMedicalTranscriptionJobs
    ListMedicalTranscriptionJobs (ListMedicalTranscriptionJobs'),
    newListMedicalTranscriptionJobs,
    ListMedicalTranscriptionJobsResponse (ListMedicalTranscriptionJobsResponse'),
    newListMedicalTranscriptionJobsResponse,

    -- ** ListMedicalVocabularies
    ListMedicalVocabularies (ListMedicalVocabularies'),
    newListMedicalVocabularies,
    ListMedicalVocabulariesResponse (ListMedicalVocabulariesResponse'),
    newListMedicalVocabulariesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTranscriptionJobs
    ListTranscriptionJobs (ListTranscriptionJobs'),
    newListTranscriptionJobs,
    ListTranscriptionJobsResponse (ListTranscriptionJobsResponse'),
    newListTranscriptionJobsResponse,

    -- ** ListVocabularies
    ListVocabularies (ListVocabularies'),
    newListVocabularies,
    ListVocabulariesResponse (ListVocabulariesResponse'),
    newListVocabulariesResponse,

    -- ** ListVocabularyFilters
    ListVocabularyFilters (ListVocabularyFilters'),
    newListVocabularyFilters,
    ListVocabularyFiltersResponse (ListVocabularyFiltersResponse'),
    newListVocabularyFiltersResponse,

    -- ** StartCallAnalyticsJob
    StartCallAnalyticsJob (StartCallAnalyticsJob'),
    newStartCallAnalyticsJob,
    StartCallAnalyticsJobResponse (StartCallAnalyticsJobResponse'),
    newStartCallAnalyticsJobResponse,

    -- ** StartMedicalTranscriptionJob
    StartMedicalTranscriptionJob (StartMedicalTranscriptionJob'),
    newStartMedicalTranscriptionJob,
    StartMedicalTranscriptionJobResponse (StartMedicalTranscriptionJobResponse'),
    newStartMedicalTranscriptionJobResponse,

    -- ** StartTranscriptionJob
    StartTranscriptionJob (StartTranscriptionJob'),
    newStartTranscriptionJob,
    StartTranscriptionJobResponse (StartTranscriptionJobResponse'),
    newStartTranscriptionJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateCallAnalyticsCategory
    UpdateCallAnalyticsCategory (UpdateCallAnalyticsCategory'),
    newUpdateCallAnalyticsCategory,
    UpdateCallAnalyticsCategoryResponse (UpdateCallAnalyticsCategoryResponse'),
    newUpdateCallAnalyticsCategoryResponse,

    -- ** UpdateMedicalVocabulary
    UpdateMedicalVocabulary (UpdateMedicalVocabulary'),
    newUpdateMedicalVocabulary,
    UpdateMedicalVocabularyResponse (UpdateMedicalVocabularyResponse'),
    newUpdateMedicalVocabularyResponse,

    -- ** UpdateVocabulary
    UpdateVocabulary (UpdateVocabulary'),
    newUpdateVocabulary,
    UpdateVocabularyResponse (UpdateVocabularyResponse'),
    newUpdateVocabularyResponse,

    -- ** UpdateVocabularyFilter
    UpdateVocabularyFilter (UpdateVocabularyFilter'),
    newUpdateVocabularyFilter,
    UpdateVocabularyFilterResponse (UpdateVocabularyFilterResponse'),
    newUpdateVocabularyFilterResponse,

    -- * Types

    -- ** BaseModelName
    BaseModelName (..),

    -- ** CLMLanguageCode
    CLMLanguageCode (..),

    -- ** CallAnalyticsJobStatus
    CallAnalyticsJobStatus (..),

    -- ** InputType
    InputType (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** MediaFormat
    MediaFormat (..),

    -- ** MedicalContentIdentificationType
    MedicalContentIdentificationType (..),

    -- ** ModelStatus
    ModelStatus (..),

    -- ** OutputLocationType
    OutputLocationType (..),

    -- ** ParticipantRole
    ParticipantRole (..),

    -- ** PiiEntityType
    PiiEntityType (..),

    -- ** RedactionOutput
    RedactionOutput (..),

    -- ** RedactionType
    RedactionType (..),

    -- ** SentimentValue
    SentimentValue (..),

    -- ** Specialty
    Specialty (..),

    -- ** SubtitleFormat
    SubtitleFormat (..),

    -- ** TranscriptFilterType
    TranscriptFilterType (..),

    -- ** TranscriptionJobStatus
    TranscriptionJobStatus (..),

    -- ** Type
    Type (..),

    -- ** VocabularyFilterMethod
    VocabularyFilterMethod (..),

    -- ** VocabularyState
    VocabularyState (..),

    -- ** AbsoluteTimeRange
    AbsoluteTimeRange (AbsoluteTimeRange'),
    newAbsoluteTimeRange,

    -- ** CallAnalyticsJob
    CallAnalyticsJob (CallAnalyticsJob'),
    newCallAnalyticsJob,

    -- ** CallAnalyticsJobSettings
    CallAnalyticsJobSettings (CallAnalyticsJobSettings'),
    newCallAnalyticsJobSettings,

    -- ** CallAnalyticsJobSummary
    CallAnalyticsJobSummary (CallAnalyticsJobSummary'),
    newCallAnalyticsJobSummary,

    -- ** CategoryProperties
    CategoryProperties (CategoryProperties'),
    newCategoryProperties,

    -- ** ChannelDefinition
    ChannelDefinition (ChannelDefinition'),
    newChannelDefinition,

    -- ** ContentRedaction
    ContentRedaction (ContentRedaction'),
    newContentRedaction,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** InterruptionFilter
    InterruptionFilter (InterruptionFilter'),
    newInterruptionFilter,

    -- ** JobExecutionSettings
    JobExecutionSettings (JobExecutionSettings'),
    newJobExecutionSettings,

    -- ** LanguageCodeItem
    LanguageCodeItem (LanguageCodeItem'),
    newLanguageCodeItem,

    -- ** LanguageIdSettings
    LanguageIdSettings (LanguageIdSettings'),
    newLanguageIdSettings,

    -- ** LanguageModel
    LanguageModel (LanguageModel'),
    newLanguageModel,

    -- ** Media
    Media (Media'),
    newMedia,

    -- ** MedicalTranscript
    MedicalTranscript (MedicalTranscript'),
    newMedicalTranscript,

    -- ** MedicalTranscriptionJob
    MedicalTranscriptionJob (MedicalTranscriptionJob'),
    newMedicalTranscriptionJob,

    -- ** MedicalTranscriptionJobSummary
    MedicalTranscriptionJobSummary (MedicalTranscriptionJobSummary'),
    newMedicalTranscriptionJobSummary,

    -- ** MedicalTranscriptionSetting
    MedicalTranscriptionSetting (MedicalTranscriptionSetting'),
    newMedicalTranscriptionSetting,

    -- ** ModelSettings
    ModelSettings (ModelSettings'),
    newModelSettings,

    -- ** NonTalkTimeFilter
    NonTalkTimeFilter (NonTalkTimeFilter'),
    newNonTalkTimeFilter,

    -- ** RelativeTimeRange
    RelativeTimeRange (RelativeTimeRange'),
    newRelativeTimeRange,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** SentimentFilter
    SentimentFilter (SentimentFilter'),
    newSentimentFilter,

    -- ** Settings
    Settings (Settings'),
    newSettings,

    -- ** Subtitles
    Subtitles (Subtitles'),
    newSubtitles,

    -- ** SubtitlesOutput
    SubtitlesOutput (SubtitlesOutput'),
    newSubtitlesOutput,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Transcript
    Transcript (Transcript'),
    newTranscript,

    -- ** TranscriptFilter
    TranscriptFilter (TranscriptFilter'),
    newTranscriptFilter,

    -- ** TranscriptionJob
    TranscriptionJob (TranscriptionJob'),
    newTranscriptionJob,

    -- ** TranscriptionJobSummary
    TranscriptionJobSummary (TranscriptionJobSummary'),
    newTranscriptionJobSummary,

    -- ** VocabularyFilterInfo
    VocabularyFilterInfo (VocabularyFilterInfo'),
    newVocabularyFilterInfo,

    -- ** VocabularyInfo
    VocabularyInfo (VocabularyInfo'),
    newVocabularyInfo,
  )
where

import Amazonka.Transcribe.CreateCallAnalyticsCategory
import Amazonka.Transcribe.CreateLanguageModel
import Amazonka.Transcribe.CreateMedicalVocabulary
import Amazonka.Transcribe.CreateVocabulary
import Amazonka.Transcribe.CreateVocabularyFilter
import Amazonka.Transcribe.DeleteCallAnalyticsCategory
import Amazonka.Transcribe.DeleteCallAnalyticsJob
import Amazonka.Transcribe.DeleteLanguageModel
import Amazonka.Transcribe.DeleteMedicalTranscriptionJob
import Amazonka.Transcribe.DeleteMedicalVocabulary
import Amazonka.Transcribe.DeleteTranscriptionJob
import Amazonka.Transcribe.DeleteVocabulary
import Amazonka.Transcribe.DeleteVocabularyFilter
import Amazonka.Transcribe.DescribeLanguageModel
import Amazonka.Transcribe.GetCallAnalyticsCategory
import Amazonka.Transcribe.GetCallAnalyticsJob
import Amazonka.Transcribe.GetMedicalTranscriptionJob
import Amazonka.Transcribe.GetMedicalVocabulary
import Amazonka.Transcribe.GetTranscriptionJob
import Amazonka.Transcribe.GetVocabulary
import Amazonka.Transcribe.GetVocabularyFilter
import Amazonka.Transcribe.Lens
import Amazonka.Transcribe.ListCallAnalyticsCategories
import Amazonka.Transcribe.ListCallAnalyticsJobs
import Amazonka.Transcribe.ListLanguageModels
import Amazonka.Transcribe.ListMedicalTranscriptionJobs
import Amazonka.Transcribe.ListMedicalVocabularies
import Amazonka.Transcribe.ListTagsForResource
import Amazonka.Transcribe.ListTranscriptionJobs
import Amazonka.Transcribe.ListVocabularies
import Amazonka.Transcribe.ListVocabularyFilters
import Amazonka.Transcribe.StartCallAnalyticsJob
import Amazonka.Transcribe.StartMedicalTranscriptionJob
import Amazonka.Transcribe.StartTranscriptionJob
import Amazonka.Transcribe.TagResource
import Amazonka.Transcribe.Types
import Amazonka.Transcribe.UntagResource
import Amazonka.Transcribe.UpdateCallAnalyticsCategory
import Amazonka.Transcribe.UpdateMedicalVocabulary
import Amazonka.Transcribe.UpdateVocabulary
import Amazonka.Transcribe.UpdateVocabularyFilter
import Amazonka.Transcribe.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Transcribe'.

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
