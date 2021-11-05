{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Lens
  ( -- * Operations

    -- ** ListLanguageModels
    listLanguageModels_nameContains,
    listLanguageModels_nextToken,
    listLanguageModels_statusEquals,
    listLanguageModels_maxResults,
    listLanguageModelsResponse_nextToken,
    listLanguageModelsResponse_models,
    listLanguageModelsResponse_httpStatus,

    -- ** GetVocabulary
    getVocabulary_vocabularyName,
    getVocabularyResponse_failureReason,
    getVocabularyResponse_languageCode,
    getVocabularyResponse_downloadUri,
    getVocabularyResponse_vocabularyName,
    getVocabularyResponse_lastModifiedTime,
    getVocabularyResponse_vocabularyState,
    getVocabularyResponse_httpStatus,

    -- ** DeleteLanguageModel
    deleteLanguageModel_modelName,

    -- ** GetTranscriptionJob
    getTranscriptionJob_transcriptionJobName,
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,

    -- ** StartMedicalTranscriptionJob
    startMedicalTranscriptionJob_settings,
    startMedicalTranscriptionJob_mediaFormat,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
    startMedicalTranscriptionJob_kmsEncryptionContext,
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_contentIdentificationType,
    startMedicalTranscriptionJob_tags,
    startMedicalTranscriptionJob_mediaSampleRateHertz,
    startMedicalTranscriptionJob_medicalTranscriptionJobName,
    startMedicalTranscriptionJob_languageCode,
    startMedicalTranscriptionJob_media,
    startMedicalTranscriptionJob_outputBucketName,
    startMedicalTranscriptionJob_specialty,
    startMedicalTranscriptionJob_type,
    startMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    startMedicalTranscriptionJobResponse_httpStatus,

    -- ** ListCallAnalyticsJobs
    listCallAnalyticsJobs_status,
    listCallAnalyticsJobs_nextToken,
    listCallAnalyticsJobs_jobNameContains,
    listCallAnalyticsJobs_maxResults,
    listCallAnalyticsJobsResponse_status,
    listCallAnalyticsJobsResponse_callAnalyticsJobSummaries,
    listCallAnalyticsJobsResponse_nextToken,
    listCallAnalyticsJobsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetCallAnalyticsCategory
    getCallAnalyticsCategory_categoryName,
    getCallAnalyticsCategoryResponse_categoryProperties,
    getCallAnalyticsCategoryResponse_httpStatus,

    -- ** DeleteMedicalVocabulary
    deleteMedicalVocabulary_vocabularyName,

    -- ** UpdateMedicalVocabulary
    updateMedicalVocabulary_vocabularyFileUri,
    updateMedicalVocabulary_vocabularyName,
    updateMedicalVocabulary_languageCode,
    updateMedicalVocabularyResponse_languageCode,
    updateMedicalVocabularyResponse_vocabularyName,
    updateMedicalVocabularyResponse_lastModifiedTime,
    updateMedicalVocabularyResponse_vocabularyState,
    updateMedicalVocabularyResponse_httpStatus,

    -- ** CreateCallAnalyticsCategory
    createCallAnalyticsCategory_categoryName,
    createCallAnalyticsCategory_rules,
    createCallAnalyticsCategoryResponse_categoryProperties,
    createCallAnalyticsCategoryResponse_httpStatus,

    -- ** DeleteTranscriptionJob
    deleteTranscriptionJob_transcriptionJobName,

    -- ** DescribeLanguageModel
    describeLanguageModel_modelName,
    describeLanguageModelResponse_languageModel,
    describeLanguageModelResponse_httpStatus,

    -- ** ListCallAnalyticsCategories
    listCallAnalyticsCategories_nextToken,
    listCallAnalyticsCategories_maxResults,
    listCallAnalyticsCategoriesResponse_categories,
    listCallAnalyticsCategoriesResponse_nextToken,
    listCallAnalyticsCategoriesResponse_httpStatus,

    -- ** DeleteMedicalTranscriptionJob
    deleteMedicalTranscriptionJob_medicalTranscriptionJobName,

    -- ** DeleteVocabulary
    deleteVocabulary_vocabularyName,

    -- ** StartCallAnalyticsJob
    startCallAnalyticsJob_settings,
    startCallAnalyticsJob_outputEncryptionKMSKeyId,
    startCallAnalyticsJob_outputLocation,
    startCallAnalyticsJob_channelDefinitions,
    startCallAnalyticsJob_callAnalyticsJobName,
    startCallAnalyticsJob_media,
    startCallAnalyticsJob_dataAccessRoleArn,
    startCallAnalyticsJobResponse_callAnalyticsJob,
    startCallAnalyticsJobResponse_httpStatus,

    -- ** UpdateVocabulary
    updateVocabulary_vocabularyFileUri,
    updateVocabulary_phrases,
    updateVocabulary_vocabularyName,
    updateVocabulary_languageCode,
    updateVocabularyResponse_languageCode,
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_lastModifiedTime,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_httpStatus,

    -- ** CreateVocabularyFilter
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_words,
    createVocabularyFilter_tags,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_httpStatus,

    -- ** GetMedicalTranscriptionJob
    getMedicalTranscriptionJob_medicalTranscriptionJobName,
    getMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    getMedicalTranscriptionJobResponse_httpStatus,

    -- ** GetVocabularyFilter
    getVocabularyFilter_vocabularyFilterName,
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_httpStatus,

    -- ** GetMedicalVocabulary
    getMedicalVocabulary_vocabularyName,
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_downloadUri,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_httpStatus,

    -- ** DeleteCallAnalyticsJob
    deleteCallAnalyticsJob_callAnalyticsJobName,
    deleteCallAnalyticsJobResponse_httpStatus,

    -- ** CreateMedicalVocabulary
    createMedicalVocabulary_tags,
    createMedicalVocabulary_vocabularyName,
    createMedicalVocabulary_languageCode,
    createMedicalVocabulary_vocabularyFileUri,
    createMedicalVocabularyResponse_failureReason,
    createMedicalVocabularyResponse_languageCode,
    createMedicalVocabularyResponse_vocabularyName,
    createMedicalVocabularyResponse_lastModifiedTime,
    createMedicalVocabularyResponse_vocabularyState,
    createMedicalVocabularyResponse_httpStatus,

    -- ** ListMedicalVocabularies
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_stateEquals,
    listMedicalVocabularies_maxResults,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_httpStatus,

    -- ** DeleteCallAnalyticsCategory
    deleteCallAnalyticsCategory_categoryName,
    deleteCallAnalyticsCategoryResponse_httpStatus,

    -- ** UpdateCallAnalyticsCategory
    updateCallAnalyticsCategory_categoryName,
    updateCallAnalyticsCategory_rules,
    updateCallAnalyticsCategoryResponse_categoryProperties,
    updateCallAnalyticsCategoryResponse_httpStatus,

    -- ** GetCallAnalyticsJob
    getCallAnalyticsJob_callAnalyticsJobName,
    getCallAnalyticsJobResponse_callAnalyticsJob,
    getCallAnalyticsJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListTranscriptionJobs
    listTranscriptionJobs_status,
    listTranscriptionJobs_nextToken,
    listTranscriptionJobs_jobNameContains,
    listTranscriptionJobs_maxResults,
    listTranscriptionJobsResponse_status,
    listTranscriptionJobsResponse_nextToken,
    listTranscriptionJobsResponse_transcriptionJobSummaries,
    listTranscriptionJobsResponse_httpStatus,

    -- ** ListMedicalTranscriptionJobs
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_jobNameContains,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteVocabularyFilter
    deleteVocabularyFilter_vocabularyFilterName,

    -- ** ListVocabularyFilters
    listVocabularyFilters_nameContains,
    listVocabularyFilters_nextToken,
    listVocabularyFilters_maxResults,
    listVocabularyFiltersResponse_nextToken,
    listVocabularyFiltersResponse_vocabularyFilters,
    listVocabularyFiltersResponse_httpStatus,

    -- ** UpdateVocabularyFilter
    updateVocabularyFilter_vocabularyFilterFileUri,
    updateVocabularyFilter_words,
    updateVocabularyFilter_vocabularyFilterName,
    updateVocabularyFilterResponse_languageCode,
    updateVocabularyFilterResponse_lastModifiedTime,
    updateVocabularyFilterResponse_vocabularyFilterName,
    updateVocabularyFilterResponse_httpStatus,

    -- ** ListVocabularies
    listVocabularies_nameContains,
    listVocabularies_nextToken,
    listVocabularies_stateEquals,
    listVocabularies_maxResults,
    listVocabulariesResponse_vocabularies,
    listVocabulariesResponse_status,
    listVocabulariesResponse_nextToken,
    listVocabulariesResponse_httpStatus,

    -- ** CreateVocabulary
    createVocabulary_vocabularyFileUri,
    createVocabulary_phrases,
    createVocabulary_tags,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabularyResponse_failureReason,
    createVocabularyResponse_languageCode,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_httpStatus,

    -- ** CreateLanguageModel
    createLanguageModel_tags,
    createLanguageModel_languageCode,
    createLanguageModel_baseModelName,
    createLanguageModel_modelName,
    createLanguageModel_inputDataConfig,
    createLanguageModelResponse_languageCode,
    createLanguageModelResponse_modelName,
    createLanguageModelResponse_inputDataConfig,
    createLanguageModelResponse_baseModelName,
    createLanguageModelResponse_modelStatus,
    createLanguageModelResponse_httpStatus,

    -- ** StartTranscriptionJob
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_subtitles,
    startTranscriptionJob_languageCode,
    startTranscriptionJob_languageOptions,
    startTranscriptionJob_settings,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_kmsEncryptionContext,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_tags,
    startTranscriptionJob_mediaSampleRateHertz,
    startTranscriptionJob_transcriptionJobName,
    startTranscriptionJob_media,
    startTranscriptionJobResponse_transcriptionJob,
    startTranscriptionJobResponse_httpStatus,

    -- * Types

    -- ** AbsoluteTimeRange
    absoluteTimeRange_first,
    absoluteTimeRange_startTime,
    absoluteTimeRange_last,
    absoluteTimeRange_endTime,

    -- ** CallAnalyticsJob
    callAnalyticsJob_creationTime,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_settings,
    callAnalyticsJob_startTime,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_media,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_channelDefinitions,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_transcript,
    callAnalyticsJob_mediaSampleRateHertz,

    -- ** CallAnalyticsJobSettings
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_languageOptions,
    callAnalyticsJobSettings_vocabularyName,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_vocabularyFilterMethod,

    -- ** CallAnalyticsJobSummary
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_failureReason,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_startTime,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_callAnalyticsJobName,

    -- ** CategoryProperties
    categoryProperties_rules,
    categoryProperties_categoryName,
    categoryProperties_lastUpdateTime,
    categoryProperties_createTime,

    -- ** ChannelDefinition
    channelDefinition_participantRole,
    channelDefinition_channelId,

    -- ** ContentRedaction
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- ** InputDataConfig
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- ** InterruptionFilter
    interruptionFilter_participantRole,
    interruptionFilter_relativeTimeRange,
    interruptionFilter_negate,
    interruptionFilter_threshold,
    interruptionFilter_absoluteTimeRange,

    -- ** JobExecutionSettings
    jobExecutionSettings_dataAccessRoleArn,
    jobExecutionSettings_allowDeferredExecution,

    -- ** LanguageModel
    languageModel_failureReason,
    languageModel_languageCode,
    languageModel_modelName,
    languageModel_lastModifiedTime,
    languageModel_upgradeAvailability,
    languageModel_inputDataConfig,
    languageModel_baseModelName,
    languageModel_modelStatus,
    languageModel_createTime,

    -- ** Media
    media_mediaFileUri,
    media_redactedMediaFileUri,

    -- ** MedicalTranscript
    medicalTranscript_transcriptFileUri,

    -- ** MedicalTranscriptionJob
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_settings,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_mediaSampleRateHertz,

    -- ** MedicalTranscriptionJobSummary
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_failureReason,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_type,
    medicalTranscriptionJobSummary_contentIdentificationType,

    -- ** MedicalTranscriptionSetting
    medicalTranscriptionSetting_vocabularyName,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_showAlternatives,
    medicalTranscriptionSetting_maxSpeakerLabels,
    medicalTranscriptionSetting_showSpeakerLabels,

    -- ** ModelSettings
    modelSettings_languageModelName,

    -- ** NonTalkTimeFilter
    nonTalkTimeFilter_relativeTimeRange,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_threshold,
    nonTalkTimeFilter_absoluteTimeRange,

    -- ** RelativeTimeRange
    relativeTimeRange_endPercentage,
    relativeTimeRange_first,
    relativeTimeRange_last,
    relativeTimeRange_startPercentage,

    -- ** Rule
    rule_nonTalkTimeFilter,
    rule_transcriptFilter,
    rule_sentimentFilter,
    rule_interruptionFilter,

    -- ** SentimentFilter
    sentimentFilter_participantRole,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_negate,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_sentiments,

    -- ** Settings
    settings_vocabularyName,
    settings_maxAlternatives,
    settings_channelIdentification,
    settings_showAlternatives,
    settings_maxSpeakerLabels,
    settings_vocabularyFilterName,
    settings_showSpeakerLabels,
    settings_vocabularyFilterMethod,

    -- ** Subtitles
    subtitles_formats,

    -- ** SubtitlesOutput
    subtitlesOutput_formats,
    subtitlesOutput_subtitleFileUris,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Transcript
    transcript_redactedTranscriptFileUri,
    transcript_transcriptFileUri,

    -- ** TranscriptFilter
    transcriptFilter_participantRole,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_negate,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- ** TranscriptionJob
    transcriptionJob_creationTime,
    transcriptionJob_failureReason,
    transcriptionJob_contentRedaction,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_subtitles,
    transcriptionJob_languageCode,
    transcriptionJob_languageOptions,
    transcriptionJob_settings,
    transcriptionJob_startTime,
    transcriptionJob_completionTime,
    transcriptionJob_media,
    transcriptionJob_mediaFormat,
    transcriptionJob_modelSettings,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_identifyLanguage,
    transcriptionJob_transcript,
    transcriptionJob_tags,
    transcriptionJob_mediaSampleRateHertz,

    -- ** TranscriptionJobSummary
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_failureReason,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_identifyLanguage,

    -- ** VocabularyFilterInfo
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_lastModifiedTime,
    vocabularyFilterInfo_vocabularyFilterName,

    -- ** VocabularyInfo
    vocabularyInfo_languageCode,
    vocabularyInfo_vocabularyName,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_vocabularyState,
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
import Amazonka.Transcribe.Types.AbsoluteTimeRange
import Amazonka.Transcribe.Types.CallAnalyticsJob
import Amazonka.Transcribe.Types.CallAnalyticsJobSettings
import Amazonka.Transcribe.Types.CallAnalyticsJobSummary
import Amazonka.Transcribe.Types.CategoryProperties
import Amazonka.Transcribe.Types.ChannelDefinition
import Amazonka.Transcribe.Types.ContentRedaction
import Amazonka.Transcribe.Types.InputDataConfig
import Amazonka.Transcribe.Types.InterruptionFilter
import Amazonka.Transcribe.Types.JobExecutionSettings
import Amazonka.Transcribe.Types.LanguageModel
import Amazonka.Transcribe.Types.Media
import Amazonka.Transcribe.Types.MedicalTranscript
import Amazonka.Transcribe.Types.MedicalTranscriptionJob
import Amazonka.Transcribe.Types.MedicalTranscriptionJobSummary
import Amazonka.Transcribe.Types.MedicalTranscriptionSetting
import Amazonka.Transcribe.Types.ModelSettings
import Amazonka.Transcribe.Types.NonTalkTimeFilter
import Amazonka.Transcribe.Types.RelativeTimeRange
import Amazonka.Transcribe.Types.Rule
import Amazonka.Transcribe.Types.SentimentFilter
import Amazonka.Transcribe.Types.Settings
import Amazonka.Transcribe.Types.Subtitles
import Amazonka.Transcribe.Types.SubtitlesOutput
import Amazonka.Transcribe.Types.Tag
import Amazonka.Transcribe.Types.Transcript
import Amazonka.Transcribe.Types.TranscriptFilter
import Amazonka.Transcribe.Types.TranscriptionJob
import Amazonka.Transcribe.Types.TranscriptionJobSummary
import Amazonka.Transcribe.Types.VocabularyFilterInfo
import Amazonka.Transcribe.Types.VocabularyInfo
import Amazonka.Transcribe.UntagResource
import Amazonka.Transcribe.UpdateCallAnalyticsCategory
import Amazonka.Transcribe.UpdateMedicalVocabulary
import Amazonka.Transcribe.UpdateVocabulary
import Amazonka.Transcribe.UpdateVocabularyFilter
