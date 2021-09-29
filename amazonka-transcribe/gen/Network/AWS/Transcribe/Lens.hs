{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Lens
  ( -- * Operations

    -- ** ListLanguageModels
    listLanguageModels_nextToken,
    listLanguageModels_nameContains,
    listLanguageModels_maxResults,
    listLanguageModels_statusEquals,
    listLanguageModelsResponse_nextToken,
    listLanguageModelsResponse_models,
    listLanguageModelsResponse_httpStatus,

    -- ** GetVocabularyFilter
    getVocabularyFilter_vocabularyFilterName,
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_httpStatus,

    -- ** StartTranscriptionJob
    startTranscriptionJob_languageCode,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_kmsEncryptionContext,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_subtitles,
    startTranscriptionJob_tags,
    startTranscriptionJob_mediaSampleRateHertz,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_settings,
    startTranscriptionJob_languageOptions,
    startTranscriptionJob_transcriptionJobName,
    startTranscriptionJob_media,
    startTranscriptionJobResponse_transcriptionJob,
    startTranscriptionJobResponse_httpStatus,

    -- ** CreateLanguageModel
    createLanguageModel_tags,
    createLanguageModel_languageCode,
    createLanguageModel_baseModelName,
    createLanguageModel_modelName,
    createLanguageModel_inputDataConfig,
    createLanguageModelResponse_languageCode,
    createLanguageModelResponse_inputDataConfig,
    createLanguageModelResponse_modelStatus,
    createLanguageModelResponse_modelName,
    createLanguageModelResponse_baseModelName,
    createLanguageModelResponse_httpStatus,

    -- ** ListVocabularies
    listVocabularies_nextToken,
    listVocabularies_nameContains,
    listVocabularies_maxResults,
    listVocabularies_stateEquals,
    listVocabulariesResponse_status,
    listVocabulariesResponse_nextToken,
    listVocabulariesResponse_vocabularies,
    listVocabulariesResponse_httpStatus,

    -- ** CreateVocabulary
    createVocabulary_phrases,
    createVocabulary_tags,
    createVocabulary_vocabularyFileUri,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabularyResponse_languageCode,
    createVocabularyResponse_failureReason,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_httpStatus,

    -- ** DeleteVocabulary
    deleteVocabulary_vocabularyName,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateVocabulary
    updateVocabulary_phrases,
    updateVocabulary_vocabularyFileUri,
    updateVocabulary_vocabularyName,
    updateVocabulary_languageCode,
    updateVocabularyResponse_languageCode,
    updateVocabularyResponse_lastModifiedTime,
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_httpStatus,

    -- ** ListVocabularyFilters
    listVocabularyFilters_nextToken,
    listVocabularyFilters_nameContains,
    listVocabularyFilters_maxResults,
    listVocabularyFiltersResponse_nextToken,
    listVocabularyFiltersResponse_vocabularyFilters,
    listVocabularyFiltersResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListTranscriptionJobs
    listTranscriptionJobs_status,
    listTranscriptionJobs_nextToken,
    listTranscriptionJobs_maxResults,
    listTranscriptionJobs_jobNameContains,
    listTranscriptionJobsResponse_status,
    listTranscriptionJobsResponse_nextToken,
    listTranscriptionJobsResponse_transcriptionJobSummaries,
    listTranscriptionJobsResponse_httpStatus,

    -- ** DeleteTranscriptionJob
    deleteTranscriptionJob_transcriptionJobName,

    -- ** StartMedicalTranscriptionJob
    startMedicalTranscriptionJob_mediaFormat,
    startMedicalTranscriptionJob_contentIdentificationType,
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_kmsEncryptionContext,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
    startMedicalTranscriptionJob_tags,
    startMedicalTranscriptionJob_mediaSampleRateHertz,
    startMedicalTranscriptionJob_settings,
    startMedicalTranscriptionJob_medicalTranscriptionJobName,
    startMedicalTranscriptionJob_languageCode,
    startMedicalTranscriptionJob_media,
    startMedicalTranscriptionJob_outputBucketName,
    startMedicalTranscriptionJob_specialty,
    startMedicalTranscriptionJob_type,
    startMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    startMedicalTranscriptionJobResponse_httpStatus,

    -- ** GetCallAnalyticsCategory
    getCallAnalyticsCategory_categoryName,
    getCallAnalyticsCategoryResponse_categoryProperties,
    getCallAnalyticsCategoryResponse_httpStatus,

    -- ** GetMedicalVocabulary
    getMedicalVocabulary_vocabularyName,
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_downloadUri,
    getMedicalVocabularyResponse_httpStatus,

    -- ** GetTranscriptionJob
    getTranscriptionJob_transcriptionJobName,
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,

    -- ** GetVocabulary
    getVocabulary_vocabularyName,
    getVocabularyResponse_languageCode,
    getVocabularyResponse_failureReason,
    getVocabularyResponse_lastModifiedTime,
    getVocabularyResponse_vocabularyName,
    getVocabularyResponse_vocabularyState,
    getVocabularyResponse_downloadUri,
    getVocabularyResponse_httpStatus,

    -- ** GetMedicalTranscriptionJob
    getMedicalTranscriptionJob_medicalTranscriptionJobName,
    getMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    getMedicalTranscriptionJobResponse_httpStatus,

    -- ** DeleteLanguageModel
    deleteLanguageModel_modelName,

    -- ** CreateVocabularyFilter
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_tags,
    createVocabularyFilter_words,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_httpStatus,

    -- ** ListMedicalTranscriptionJobs
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_jobNameContains,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_httpStatus,

    -- ** StartCallAnalyticsJob
    startCallAnalyticsJob_channelDefinitions,
    startCallAnalyticsJob_outputEncryptionKMSKeyId,
    startCallAnalyticsJob_settings,
    startCallAnalyticsJob_outputLocation,
    startCallAnalyticsJob_callAnalyticsJobName,
    startCallAnalyticsJob_media,
    startCallAnalyticsJob_dataAccessRoleArn,
    startCallAnalyticsJobResponse_callAnalyticsJob,
    startCallAnalyticsJobResponse_httpStatus,

    -- ** DeleteMedicalTranscriptionJob
    deleteMedicalTranscriptionJob_medicalTranscriptionJobName,

    -- ** UpdateVocabularyFilter
    updateVocabularyFilter_vocabularyFilterFileUri,
    updateVocabularyFilter_words,
    updateVocabularyFilter_vocabularyFilterName,
    updateVocabularyFilterResponse_languageCode,
    updateVocabularyFilterResponse_vocabularyFilterName,
    updateVocabularyFilterResponse_lastModifiedTime,
    updateVocabularyFilterResponse_httpStatus,

    -- ** DeleteVocabularyFilter
    deleteVocabularyFilter_vocabularyFilterName,

    -- ** UpdateMedicalVocabulary
    updateMedicalVocabulary_vocabularyFileUri,
    updateMedicalVocabulary_vocabularyName,
    updateMedicalVocabulary_languageCode,
    updateMedicalVocabularyResponse_languageCode,
    updateMedicalVocabularyResponse_lastModifiedTime,
    updateMedicalVocabularyResponse_vocabularyName,
    updateMedicalVocabularyResponse_vocabularyState,
    updateMedicalVocabularyResponse_httpStatus,

    -- ** DescribeLanguageModel
    describeLanguageModel_modelName,
    describeLanguageModelResponse_languageModel,
    describeLanguageModelResponse_httpStatus,

    -- ** CreateCallAnalyticsCategory
    createCallAnalyticsCategory_categoryName,
    createCallAnalyticsCategory_rules,
    createCallAnalyticsCategoryResponse_categoryProperties,
    createCallAnalyticsCategoryResponse_httpStatus,

    -- ** ListCallAnalyticsCategories
    listCallAnalyticsCategories_nextToken,
    listCallAnalyticsCategories_maxResults,
    listCallAnalyticsCategoriesResponse_nextToken,
    listCallAnalyticsCategoriesResponse_categories,
    listCallAnalyticsCategoriesResponse_httpStatus,

    -- ** DeleteMedicalVocabulary
    deleteMedicalVocabulary_vocabularyName,

    -- ** ListMedicalVocabularies
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_maxResults,
    listMedicalVocabularies_stateEquals,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_httpStatus,

    -- ** GetCallAnalyticsJob
    getCallAnalyticsJob_callAnalyticsJobName,
    getCallAnalyticsJobResponse_callAnalyticsJob,
    getCallAnalyticsJobResponse_httpStatus,

    -- ** UpdateCallAnalyticsCategory
    updateCallAnalyticsCategory_categoryName,
    updateCallAnalyticsCategory_rules,
    updateCallAnalyticsCategoryResponse_categoryProperties,
    updateCallAnalyticsCategoryResponse_httpStatus,

    -- ** CreateMedicalVocabulary
    createMedicalVocabulary_tags,
    createMedicalVocabulary_vocabularyName,
    createMedicalVocabulary_languageCode,
    createMedicalVocabulary_vocabularyFileUri,
    createMedicalVocabularyResponse_languageCode,
    createMedicalVocabularyResponse_failureReason,
    createMedicalVocabularyResponse_lastModifiedTime,
    createMedicalVocabularyResponse_vocabularyName,
    createMedicalVocabularyResponse_vocabularyState,
    createMedicalVocabularyResponse_httpStatus,

    -- ** DeleteCallAnalyticsCategory
    deleteCallAnalyticsCategory_categoryName,
    deleteCallAnalyticsCategoryResponse_httpStatus,

    -- ** ListCallAnalyticsJobs
    listCallAnalyticsJobs_status,
    listCallAnalyticsJobs_nextToken,
    listCallAnalyticsJobs_maxResults,
    listCallAnalyticsJobs_jobNameContains,
    listCallAnalyticsJobsResponse_status,
    listCallAnalyticsJobsResponse_nextToken,
    listCallAnalyticsJobsResponse_callAnalyticsJobSummaries,
    listCallAnalyticsJobsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteCallAnalyticsJob
    deleteCallAnalyticsJob_callAnalyticsJobName,
    deleteCallAnalyticsJobResponse_httpStatus,

    -- * Types

    -- ** AbsoluteTimeRange
    absoluteTimeRange_startTime,
    absoluteTimeRange_endTime,
    absoluteTimeRange_last,
    absoluteTimeRange_first,

    -- ** CallAnalyticsJob
    callAnalyticsJob_languageCode,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_media,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_transcript,
    callAnalyticsJob_startTime,
    callAnalyticsJob_channelDefinitions,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_mediaSampleRateHertz,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_settings,

    -- ** CallAnalyticsJobSettings
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_vocabularyFilterMethod,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_vocabularyName,
    callAnalyticsJobSettings_languageOptions,

    -- ** CallAnalyticsJobSummary
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_startTime,
    callAnalyticsJobSummary_callAnalyticsJobName,
    callAnalyticsJobSummary_failureReason,

    -- ** CategoryProperties
    categoryProperties_lastUpdateTime,
    categoryProperties_rules,
    categoryProperties_categoryName,
    categoryProperties_createTime,

    -- ** ChannelDefinition
    channelDefinition_channelId,
    channelDefinition_participantRole,

    -- ** ContentRedaction
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- ** InputDataConfig
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- ** InterruptionFilter
    interruptionFilter_threshold,
    interruptionFilter_relativeTimeRange,
    interruptionFilter_negate,
    interruptionFilter_participantRole,
    interruptionFilter_absoluteTimeRange,

    -- ** JobExecutionSettings
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- ** LanguageModel
    languageModel_languageCode,
    languageModel_inputDataConfig,
    languageModel_modelStatus,
    languageModel_failureReason,
    languageModel_createTime,
    languageModel_upgradeAvailability,
    languageModel_lastModifiedTime,
    languageModel_modelName,
    languageModel_baseModelName,

    -- ** Media
    media_mediaFileUri,
    media_redactedMediaFileUri,

    -- ** MedicalTranscript
    medicalTranscript_transcriptFileUri,

    -- ** MedicalTranscriptionJob
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_settings,

    -- ** MedicalTranscriptionJobSummary
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_contentIdentificationType,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_failureReason,
    medicalTranscriptionJobSummary_type,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,

    -- ** MedicalTranscriptionSetting
    medicalTranscriptionSetting_showAlternatives,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_showSpeakerLabels,
    medicalTranscriptionSetting_vocabularyName,
    medicalTranscriptionSetting_maxSpeakerLabels,

    -- ** ModelSettings
    modelSettings_languageModelName,

    -- ** NonTalkTimeFilter
    nonTalkTimeFilter_threshold,
    nonTalkTimeFilter_relativeTimeRange,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_absoluteTimeRange,

    -- ** RelativeTimeRange
    relativeTimeRange_startPercentage,
    relativeTimeRange_endPercentage,
    relativeTimeRange_last,
    relativeTimeRange_first,

    -- ** Rule
    rule_sentimentFilter,
    rule_interruptionFilter,
    rule_transcriptFilter,
    rule_nonTalkTimeFilter,

    -- ** SentimentFilter
    sentimentFilter_relativeTimeRange,
    sentimentFilter_negate,
    sentimentFilter_participantRole,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_sentiments,

    -- ** Settings
    settings_vocabularyFilterName,
    settings_vocabularyFilterMethod,
    settings_showAlternatives,
    settings_channelIdentification,
    settings_maxAlternatives,
    settings_showSpeakerLabels,
    settings_vocabularyName,
    settings_maxSpeakerLabels,

    -- ** Subtitles
    subtitles_formats,

    -- ** SubtitlesOutput
    subtitlesOutput_subtitleFileUris,
    subtitlesOutput_formats,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Transcript
    transcript_transcriptFileUri,
    transcript_redactedTranscriptFileUri,

    -- ** TranscriptFilter
    transcriptFilter_relativeTimeRange,
    transcriptFilter_negate,
    transcriptFilter_participantRole,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- ** TranscriptionJob
    transcriptionJob_languageCode,
    transcriptionJob_mediaFormat,
    transcriptionJob_contentRedaction,
    transcriptionJob_creationTime,
    transcriptionJob_media,
    transcriptionJob_completionTime,
    transcriptionJob_transcript,
    transcriptionJob_identifyLanguage,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_startTime,
    transcriptionJob_modelSettings,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_subtitles,
    transcriptionJob_failureReason,
    transcriptionJob_tags,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_settings,
    transcriptionJob_languageOptions,

    -- ** TranscriptionJobSummary
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_failureReason,

    -- ** VocabularyFilterInfo
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_vocabularyFilterName,
    vocabularyFilterInfo_lastModifiedTime,

    -- ** VocabularyInfo
    vocabularyInfo_languageCode,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_vocabularyName,
    vocabularyInfo_vocabularyState,
  )
where

import Network.AWS.Transcribe.CreateCallAnalyticsCategory
import Network.AWS.Transcribe.CreateLanguageModel
import Network.AWS.Transcribe.CreateMedicalVocabulary
import Network.AWS.Transcribe.CreateVocabulary
import Network.AWS.Transcribe.CreateVocabularyFilter
import Network.AWS.Transcribe.DeleteCallAnalyticsCategory
import Network.AWS.Transcribe.DeleteCallAnalyticsJob
import Network.AWS.Transcribe.DeleteLanguageModel
import Network.AWS.Transcribe.DeleteMedicalTranscriptionJob
import Network.AWS.Transcribe.DeleteMedicalVocabulary
import Network.AWS.Transcribe.DeleteTranscriptionJob
import Network.AWS.Transcribe.DeleteVocabulary
import Network.AWS.Transcribe.DeleteVocabularyFilter
import Network.AWS.Transcribe.DescribeLanguageModel
import Network.AWS.Transcribe.GetCallAnalyticsCategory
import Network.AWS.Transcribe.GetCallAnalyticsJob
import Network.AWS.Transcribe.GetMedicalTranscriptionJob
import Network.AWS.Transcribe.GetMedicalVocabulary
import Network.AWS.Transcribe.GetTranscriptionJob
import Network.AWS.Transcribe.GetVocabulary
import Network.AWS.Transcribe.GetVocabularyFilter
import Network.AWS.Transcribe.ListCallAnalyticsCategories
import Network.AWS.Transcribe.ListCallAnalyticsJobs
import Network.AWS.Transcribe.ListLanguageModels
import Network.AWS.Transcribe.ListMedicalTranscriptionJobs
import Network.AWS.Transcribe.ListMedicalVocabularies
import Network.AWS.Transcribe.ListTagsForResource
import Network.AWS.Transcribe.ListTranscriptionJobs
import Network.AWS.Transcribe.ListVocabularies
import Network.AWS.Transcribe.ListVocabularyFilters
import Network.AWS.Transcribe.StartCallAnalyticsJob
import Network.AWS.Transcribe.StartMedicalTranscriptionJob
import Network.AWS.Transcribe.StartTranscriptionJob
import Network.AWS.Transcribe.TagResource
import Network.AWS.Transcribe.Types.AbsoluteTimeRange
import Network.AWS.Transcribe.Types.CallAnalyticsJob
import Network.AWS.Transcribe.Types.CallAnalyticsJobSettings
import Network.AWS.Transcribe.Types.CallAnalyticsJobSummary
import Network.AWS.Transcribe.Types.CategoryProperties
import Network.AWS.Transcribe.Types.ChannelDefinition
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.InterruptionFilter
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageModel
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionJob
import Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.NonTalkTimeFilter
import Network.AWS.Transcribe.Types.RelativeTimeRange
import Network.AWS.Transcribe.Types.Rule
import Network.AWS.Transcribe.Types.SentimentFilter
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Subtitles
import Network.AWS.Transcribe.Types.SubtitlesOutput
import Network.AWS.Transcribe.Types.Tag
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptFilter
import Network.AWS.Transcribe.Types.TranscriptionJob
import Network.AWS.Transcribe.Types.TranscriptionJobSummary
import Network.AWS.Transcribe.Types.VocabularyFilterInfo
import Network.AWS.Transcribe.Types.VocabularyInfo
import Network.AWS.Transcribe.UntagResource
import Network.AWS.Transcribe.UpdateCallAnalyticsCategory
import Network.AWS.Transcribe.UpdateMedicalVocabulary
import Network.AWS.Transcribe.UpdateVocabulary
import Network.AWS.Transcribe.UpdateVocabularyFilter
