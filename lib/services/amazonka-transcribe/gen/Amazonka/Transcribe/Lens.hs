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

    -- ** CreateCallAnalyticsCategory
    createCallAnalyticsCategory_categoryName,
    createCallAnalyticsCategory_rules,
    createCallAnalyticsCategoryResponse_categoryProperties,
    createCallAnalyticsCategoryResponse_httpStatus,

    -- ** CreateLanguageModel
    createLanguageModel_tags,
    createLanguageModel_languageCode,
    createLanguageModel_baseModelName,
    createLanguageModel_modelName,
    createLanguageModel_inputDataConfig,
    createLanguageModelResponse_modelStatus,
    createLanguageModelResponse_languageCode,
    createLanguageModelResponse_modelName,
    createLanguageModelResponse_baseModelName,
    createLanguageModelResponse_inputDataConfig,
    createLanguageModelResponse_httpStatus,

    -- ** CreateMedicalVocabulary
    createMedicalVocabulary_tags,
    createMedicalVocabulary_vocabularyName,
    createMedicalVocabulary_languageCode,
    createMedicalVocabulary_vocabularyFileUri,
    createMedicalVocabularyResponse_vocabularyName,
    createMedicalVocabularyResponse_vocabularyState,
    createMedicalVocabularyResponse_lastModifiedTime,
    createMedicalVocabularyResponse_languageCode,
    createMedicalVocabularyResponse_failureReason,
    createMedicalVocabularyResponse_httpStatus,

    -- ** CreateVocabulary
    createVocabulary_tags,
    createVocabulary_phrases,
    createVocabulary_vocabularyFileUri,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_languageCode,
    createVocabularyResponse_failureReason,
    createVocabularyResponse_httpStatus,

    -- ** CreateVocabularyFilter
    createVocabularyFilter_tags,
    createVocabularyFilter_words,
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_httpStatus,

    -- ** DeleteCallAnalyticsCategory
    deleteCallAnalyticsCategory_categoryName,
    deleteCallAnalyticsCategoryResponse_httpStatus,

    -- ** DeleteCallAnalyticsJob
    deleteCallAnalyticsJob_callAnalyticsJobName,
    deleteCallAnalyticsJobResponse_httpStatus,

    -- ** DeleteLanguageModel
    deleteLanguageModel_modelName,

    -- ** DeleteMedicalTranscriptionJob
    deleteMedicalTranscriptionJob_medicalTranscriptionJobName,

    -- ** DeleteMedicalVocabulary
    deleteMedicalVocabulary_vocabularyName,

    -- ** DeleteTranscriptionJob
    deleteTranscriptionJob_transcriptionJobName,

    -- ** DeleteVocabulary
    deleteVocabulary_vocabularyName,

    -- ** DeleteVocabularyFilter
    deleteVocabularyFilter_vocabularyFilterName,

    -- ** DescribeLanguageModel
    describeLanguageModel_modelName,
    describeLanguageModelResponse_languageModel,
    describeLanguageModelResponse_httpStatus,

    -- ** GetCallAnalyticsCategory
    getCallAnalyticsCategory_categoryName,
    getCallAnalyticsCategoryResponse_categoryProperties,
    getCallAnalyticsCategoryResponse_httpStatus,

    -- ** GetCallAnalyticsJob
    getCallAnalyticsJob_callAnalyticsJobName,
    getCallAnalyticsJobResponse_callAnalyticsJob,
    getCallAnalyticsJobResponse_httpStatus,

    -- ** GetMedicalTranscriptionJob
    getMedicalTranscriptionJob_medicalTranscriptionJobName,
    getMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    getMedicalTranscriptionJobResponse_httpStatus,

    -- ** GetMedicalVocabulary
    getMedicalVocabulary_vocabularyName,
    getMedicalVocabularyResponse_downloadUri,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_httpStatus,

    -- ** GetTranscriptionJob
    getTranscriptionJob_transcriptionJobName,
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,

    -- ** GetVocabulary
    getVocabulary_vocabularyName,
    getVocabularyResponse_downloadUri,
    getVocabularyResponse_vocabularyName,
    getVocabularyResponse_vocabularyState,
    getVocabularyResponse_lastModifiedTime,
    getVocabularyResponse_languageCode,
    getVocabularyResponse_failureReason,
    getVocabularyResponse_httpStatus,

    -- ** GetVocabularyFilter
    getVocabularyFilter_vocabularyFilterName,
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_httpStatus,

    -- ** ListCallAnalyticsCategories
    listCallAnalyticsCategories_nextToken,
    listCallAnalyticsCategories_maxResults,
    listCallAnalyticsCategoriesResponse_nextToken,
    listCallAnalyticsCategoriesResponse_categories,
    listCallAnalyticsCategoriesResponse_httpStatus,

    -- ** ListCallAnalyticsJobs
    listCallAnalyticsJobs_nextToken,
    listCallAnalyticsJobs_status,
    listCallAnalyticsJobs_maxResults,
    listCallAnalyticsJobs_jobNameContains,
    listCallAnalyticsJobsResponse_nextToken,
    listCallAnalyticsJobsResponse_status,
    listCallAnalyticsJobsResponse_callAnalyticsJobSummaries,
    listCallAnalyticsJobsResponse_httpStatus,

    -- ** ListLanguageModels
    listLanguageModels_nextToken,
    listLanguageModels_nameContains,
    listLanguageModels_maxResults,
    listLanguageModels_statusEquals,
    listLanguageModelsResponse_nextToken,
    listLanguageModelsResponse_models,
    listLanguageModelsResponse_httpStatus,

    -- ** ListMedicalTranscriptionJobs
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_jobNameContains,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_httpStatus,

    -- ** ListMedicalVocabularies
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_maxResults,
    listMedicalVocabularies_stateEquals,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTranscriptionJobs
    listTranscriptionJobs_nextToken,
    listTranscriptionJobs_status,
    listTranscriptionJobs_maxResults,
    listTranscriptionJobs_jobNameContains,
    listTranscriptionJobsResponse_nextToken,
    listTranscriptionJobsResponse_transcriptionJobSummaries,
    listTranscriptionJobsResponse_status,
    listTranscriptionJobsResponse_httpStatus,

    -- ** ListVocabularies
    listVocabularies_nextToken,
    listVocabularies_nameContains,
    listVocabularies_maxResults,
    listVocabularies_stateEquals,
    listVocabulariesResponse_nextToken,
    listVocabulariesResponse_status,
    listVocabulariesResponse_vocabularies,
    listVocabulariesResponse_httpStatus,

    -- ** ListVocabularyFilters
    listVocabularyFilters_nextToken,
    listVocabularyFilters_nameContains,
    listVocabularyFilters_maxResults,
    listVocabularyFiltersResponse_nextToken,
    listVocabularyFiltersResponse_vocabularyFilters,
    listVocabularyFiltersResponse_httpStatus,

    -- ** StartCallAnalyticsJob
    startCallAnalyticsJob_outputLocation,
    startCallAnalyticsJob_dataAccessRoleArn,
    startCallAnalyticsJob_settings,
    startCallAnalyticsJob_outputEncryptionKMSKeyId,
    startCallAnalyticsJob_channelDefinitions,
    startCallAnalyticsJob_callAnalyticsJobName,
    startCallAnalyticsJob_media,
    startCallAnalyticsJobResponse_callAnalyticsJob,
    startCallAnalyticsJobResponse_httpStatus,

    -- ** StartMedicalTranscriptionJob
    startMedicalTranscriptionJob_tags,
    startMedicalTranscriptionJob_contentIdentificationType,
    startMedicalTranscriptionJob_kmsEncryptionContext,
    startMedicalTranscriptionJob_mediaFormat,
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_settings,
    startMedicalTranscriptionJob_mediaSampleRateHertz,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
    startMedicalTranscriptionJob_medicalTranscriptionJobName,
    startMedicalTranscriptionJob_languageCode,
    startMedicalTranscriptionJob_media,
    startMedicalTranscriptionJob_outputBucketName,
    startMedicalTranscriptionJob_specialty,
    startMedicalTranscriptionJob_type,
    startMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    startMedicalTranscriptionJobResponse_httpStatus,

    -- ** StartTranscriptionJob
    startTranscriptionJob_tags,
    startTranscriptionJob_kmsEncryptionContext,
    startTranscriptionJob_identifyMultipleLanguages,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_subtitles,
    startTranscriptionJob_languageIdSettings,
    startTranscriptionJob_settings,
    startTranscriptionJob_mediaSampleRateHertz,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_languageCode,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_languageOptions,
    startTranscriptionJob_transcriptionJobName,
    startTranscriptionJob_media,
    startTranscriptionJobResponse_transcriptionJob,
    startTranscriptionJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCallAnalyticsCategory
    updateCallAnalyticsCategory_categoryName,
    updateCallAnalyticsCategory_rules,
    updateCallAnalyticsCategoryResponse_categoryProperties,
    updateCallAnalyticsCategoryResponse_httpStatus,

    -- ** UpdateMedicalVocabulary
    updateMedicalVocabulary_vocabularyName,
    updateMedicalVocabulary_languageCode,
    updateMedicalVocabulary_vocabularyFileUri,
    updateMedicalVocabularyResponse_vocabularyName,
    updateMedicalVocabularyResponse_vocabularyState,
    updateMedicalVocabularyResponse_lastModifiedTime,
    updateMedicalVocabularyResponse_languageCode,
    updateMedicalVocabularyResponse_httpStatus,

    -- ** UpdateVocabulary
    updateVocabulary_phrases,
    updateVocabulary_vocabularyFileUri,
    updateVocabulary_vocabularyName,
    updateVocabulary_languageCode,
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_lastModifiedTime,
    updateVocabularyResponse_languageCode,
    updateVocabularyResponse_httpStatus,

    -- ** UpdateVocabularyFilter
    updateVocabularyFilter_words,
    updateVocabularyFilter_vocabularyFilterFileUri,
    updateVocabularyFilter_vocabularyFilterName,
    updateVocabularyFilterResponse_lastModifiedTime,
    updateVocabularyFilterResponse_languageCode,
    updateVocabularyFilterResponse_vocabularyFilterName,
    updateVocabularyFilterResponse_httpStatus,

    -- * Types

    -- ** AbsoluteTimeRange
    absoluteTimeRange_endTime,
    absoluteTimeRange_last,
    absoluteTimeRange_first,
    absoluteTimeRange_startTime,

    -- ** CallAnalyticsJob
    callAnalyticsJob_transcript,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_settings,
    callAnalyticsJob_mediaSampleRateHertz,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_startTime,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_media,
    callAnalyticsJob_channelDefinitions,

    -- ** CallAnalyticsJobSettings
    callAnalyticsJobSettings_vocabularyFilterMethod,
    callAnalyticsJobSettings_vocabularyName,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_languageIdSettings,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_languageOptions,

    -- ** CallAnalyticsJobSummary
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_callAnalyticsJobName,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_startTime,
    callAnalyticsJobSummary_failureReason,

    -- ** CategoryProperties
    categoryProperties_rules,
    categoryProperties_lastUpdateTime,
    categoryProperties_createTime,
    categoryProperties_categoryName,

    -- ** ChannelDefinition
    channelDefinition_participantRole,
    channelDefinition_channelId,

    -- ** ContentRedaction
    contentRedaction_piiEntityTypes,
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- ** InputDataConfig
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- ** InterruptionFilter
    interruptionFilter_negate,
    interruptionFilter_absoluteTimeRange,
    interruptionFilter_participantRole,
    interruptionFilter_threshold,
    interruptionFilter_relativeTimeRange,

    -- ** JobExecutionSettings
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- ** LanguageCodeItem
    languageCodeItem_languageCode,
    languageCodeItem_durationInSeconds,

    -- ** LanguageIdSettings
    languageIdSettings_vocabularyName,
    languageIdSettings_languageModelName,
    languageIdSettings_vocabularyFilterName,

    -- ** LanguageModel
    languageModel_modelStatus,
    languageModel_upgradeAvailability,
    languageModel_lastModifiedTime,
    languageModel_languageCode,
    languageModel_modelName,
    languageModel_baseModelName,
    languageModel_createTime,
    languageModel_inputDataConfig,
    languageModel_failureReason,

    -- ** Media
    media_redactedMediaFileUri,
    media_mediaFileUri,

    -- ** MedicalTranscript
    medicalTranscript_transcriptFileUri,

    -- ** MedicalTranscriptionJob
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_settings,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_media,

    -- ** MedicalTranscriptionJobSummary
    medicalTranscriptionJobSummary_type,
    medicalTranscriptionJobSummary_contentIdentificationType,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_failureReason,

    -- ** MedicalTranscriptionSetting
    medicalTranscriptionSetting_vocabularyName,
    medicalTranscriptionSetting_maxSpeakerLabels,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_showSpeakerLabels,
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_showAlternatives,

    -- ** ModelSettings
    modelSettings_languageModelName,

    -- ** NonTalkTimeFilter
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_absoluteTimeRange,
    nonTalkTimeFilter_threshold,
    nonTalkTimeFilter_relativeTimeRange,

    -- ** RelativeTimeRange
    relativeTimeRange_last,
    relativeTimeRange_endPercentage,
    relativeTimeRange_startPercentage,
    relativeTimeRange_first,

    -- ** Rule
    rule_transcriptFilter,
    rule_interruptionFilter,
    rule_sentimentFilter,
    rule_nonTalkTimeFilter,

    -- ** SentimentFilter
    sentimentFilter_negate,
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_participantRole,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_sentiments,

    -- ** Settings
    settings_vocabularyFilterMethod,
    settings_vocabularyName,
    settings_maxSpeakerLabels,
    settings_maxAlternatives,
    settings_vocabularyFilterName,
    settings_showSpeakerLabels,
    settings_channelIdentification,
    settings_showAlternatives,

    -- ** Subtitles
    subtitles_outputStartIndex,
    subtitles_formats,

    -- ** SubtitlesOutput
    subtitlesOutput_outputStartIndex,
    subtitlesOutput_formats,
    subtitlesOutput_subtitleFileUris,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Transcript
    transcript_redactedTranscriptFileUri,
    transcript_transcriptFileUri,

    -- ** TranscriptFilter
    transcriptFilter_negate,
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_participantRole,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- ** TranscriptionJob
    transcriptionJob_tags,
    transcriptionJob_transcript,
    transcriptionJob_identifyMultipleLanguages,
    transcriptionJob_mediaFormat,
    transcriptionJob_identifyLanguage,
    transcriptionJob_contentRedaction,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_completionTime,
    transcriptionJob_subtitles,
    transcriptionJob_languageIdSettings,
    transcriptionJob_settings,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_languageCode,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_creationTime,
    transcriptionJob_modelSettings,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_startTime,
    transcriptionJob_failureReason,
    transcriptionJob_languageOptions,
    transcriptionJob_languageCodes,
    transcriptionJob_media,

    -- ** TranscriptionJobSummary
    transcriptionJobSummary_identifyMultipleLanguages,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_failureReason,
    transcriptionJobSummary_languageCodes,

    -- ** VocabularyFilterInfo
    vocabularyFilterInfo_lastModifiedTime,
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_vocabularyFilterName,

    -- ** VocabularyInfo
    vocabularyInfo_vocabularyName,
    vocabularyInfo_vocabularyState,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_languageCode,
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
import Amazonka.Transcribe.Types.LanguageCodeItem
import Amazonka.Transcribe.Types.LanguageIdSettings
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
