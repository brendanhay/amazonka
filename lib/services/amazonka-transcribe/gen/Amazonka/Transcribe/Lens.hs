{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Lens
  ( -- * Operations

    -- ** CreateCallAnalyticsCategory
    createCallAnalyticsCategory_inputType,
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
    createLanguageModelResponse_baseModelName,
    createLanguageModelResponse_inputDataConfig,
    createLanguageModelResponse_languageCode,
    createLanguageModelResponse_modelName,
    createLanguageModelResponse_modelStatus,
    createLanguageModelResponse_httpStatus,

    -- ** CreateMedicalVocabulary
    createMedicalVocabulary_tags,
    createMedicalVocabulary_vocabularyName,
    createMedicalVocabulary_languageCode,
    createMedicalVocabulary_vocabularyFileUri,
    createMedicalVocabularyResponse_failureReason,
    createMedicalVocabularyResponse_languageCode,
    createMedicalVocabularyResponse_lastModifiedTime,
    createMedicalVocabularyResponse_vocabularyName,
    createMedicalVocabularyResponse_vocabularyState,
    createMedicalVocabularyResponse_httpStatus,

    -- ** CreateVocabulary
    createVocabulary_phrases,
    createVocabulary_tags,
    createVocabulary_vocabularyFileUri,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabularyResponse_failureReason,
    createVocabularyResponse_languageCode,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_httpStatus,

    -- ** CreateVocabularyFilter
    createVocabularyFilter_tags,
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_words,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_lastModifiedTime,
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
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_httpStatus,

    -- ** GetTranscriptionJob
    getTranscriptionJob_transcriptionJobName,
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,

    -- ** GetVocabulary
    getVocabulary_vocabularyName,
    getVocabularyResponse_downloadUri,
    getVocabularyResponse_failureReason,
    getVocabularyResponse_languageCode,
    getVocabularyResponse_lastModifiedTime,
    getVocabularyResponse_vocabularyName,
    getVocabularyResponse_vocabularyState,
    getVocabularyResponse_httpStatus,

    -- ** GetVocabularyFilter
    getVocabularyFilter_vocabularyFilterName,
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_httpStatus,

    -- ** ListCallAnalyticsCategories
    listCallAnalyticsCategories_maxResults,
    listCallAnalyticsCategories_nextToken,
    listCallAnalyticsCategoriesResponse_categories,
    listCallAnalyticsCategoriesResponse_nextToken,
    listCallAnalyticsCategoriesResponse_httpStatus,

    -- ** ListCallAnalyticsJobs
    listCallAnalyticsJobs_jobNameContains,
    listCallAnalyticsJobs_maxResults,
    listCallAnalyticsJobs_nextToken,
    listCallAnalyticsJobs_status,
    listCallAnalyticsJobsResponse_callAnalyticsJobSummaries,
    listCallAnalyticsJobsResponse_nextToken,
    listCallAnalyticsJobsResponse_status,
    listCallAnalyticsJobsResponse_httpStatus,

    -- ** ListLanguageModels
    listLanguageModels_maxResults,
    listLanguageModels_nameContains,
    listLanguageModels_nextToken,
    listLanguageModels_statusEquals,
    listLanguageModelsResponse_models,
    listLanguageModelsResponse_nextToken,
    listLanguageModelsResponse_httpStatus,

    -- ** ListMedicalTranscriptionJobs
    listMedicalTranscriptionJobs_jobNameContains,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_httpStatus,

    -- ** ListMedicalVocabularies
    listMedicalVocabularies_maxResults,
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_stateEquals,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTranscriptionJobs
    listTranscriptionJobs_jobNameContains,
    listTranscriptionJobs_maxResults,
    listTranscriptionJobs_nextToken,
    listTranscriptionJobs_status,
    listTranscriptionJobsResponse_nextToken,
    listTranscriptionJobsResponse_status,
    listTranscriptionJobsResponse_transcriptionJobSummaries,
    listTranscriptionJobsResponse_httpStatus,

    -- ** ListVocabularies
    listVocabularies_maxResults,
    listVocabularies_nameContains,
    listVocabularies_nextToken,
    listVocabularies_stateEquals,
    listVocabulariesResponse_nextToken,
    listVocabulariesResponse_status,
    listVocabulariesResponse_vocabularies,
    listVocabulariesResponse_httpStatus,

    -- ** ListVocabularyFilters
    listVocabularyFilters_maxResults,
    listVocabularyFilters_nameContains,
    listVocabularyFilters_nextToken,
    listVocabularyFiltersResponse_nextToken,
    listVocabularyFiltersResponse_vocabularyFilters,
    listVocabularyFiltersResponse_httpStatus,

    -- ** StartCallAnalyticsJob
    startCallAnalyticsJob_channelDefinitions,
    startCallAnalyticsJob_dataAccessRoleArn,
    startCallAnalyticsJob_outputEncryptionKMSKeyId,
    startCallAnalyticsJob_outputLocation,
    startCallAnalyticsJob_settings,
    startCallAnalyticsJob_callAnalyticsJobName,
    startCallAnalyticsJob_media,
    startCallAnalyticsJobResponse_callAnalyticsJob,
    startCallAnalyticsJobResponse_httpStatus,

    -- ** StartMedicalTranscriptionJob
    startMedicalTranscriptionJob_contentIdentificationType,
    startMedicalTranscriptionJob_kmsEncryptionContext,
    startMedicalTranscriptionJob_mediaFormat,
    startMedicalTranscriptionJob_mediaSampleRateHertz,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_settings,
    startMedicalTranscriptionJob_tags,
    startMedicalTranscriptionJob_medicalTranscriptionJobName,
    startMedicalTranscriptionJob_languageCode,
    startMedicalTranscriptionJob_media,
    startMedicalTranscriptionJob_outputBucketName,
    startMedicalTranscriptionJob_specialty,
    startMedicalTranscriptionJob_type,
    startMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    startMedicalTranscriptionJobResponse_httpStatus,

    -- ** StartTranscriptionJob
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_identifyMultipleLanguages,
    startTranscriptionJob_jobExecutionSettings,
    startTranscriptionJob_kmsEncryptionContext,
    startTranscriptionJob_languageCode,
    startTranscriptionJob_languageIdSettings,
    startTranscriptionJob_languageOptions,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_mediaSampleRateHertz,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_outputBucketName,
    startTranscriptionJob_outputEncryptionKMSKeyId,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_settings,
    startTranscriptionJob_subtitles,
    startTranscriptionJob_tags,
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
    updateCallAnalyticsCategory_inputType,
    updateCallAnalyticsCategory_categoryName,
    updateCallAnalyticsCategory_rules,
    updateCallAnalyticsCategoryResponse_categoryProperties,
    updateCallAnalyticsCategoryResponse_httpStatus,

    -- ** UpdateMedicalVocabulary
    updateMedicalVocabulary_vocabularyName,
    updateMedicalVocabulary_languageCode,
    updateMedicalVocabulary_vocabularyFileUri,
    updateMedicalVocabularyResponse_languageCode,
    updateMedicalVocabularyResponse_lastModifiedTime,
    updateMedicalVocabularyResponse_vocabularyName,
    updateMedicalVocabularyResponse_vocabularyState,
    updateMedicalVocabularyResponse_httpStatus,

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

    -- ** UpdateVocabularyFilter
    updateVocabularyFilter_vocabularyFilterFileUri,
    updateVocabularyFilter_words,
    updateVocabularyFilter_vocabularyFilterName,
    updateVocabularyFilterResponse_languageCode,
    updateVocabularyFilterResponse_lastModifiedTime,
    updateVocabularyFilterResponse_vocabularyFilterName,
    updateVocabularyFilterResponse_httpStatus,

    -- * Types

    -- ** AbsoluteTimeRange
    absoluteTimeRange_endTime,
    absoluteTimeRange_first,
    absoluteTimeRange_last,
    absoluteTimeRange_startTime,

    -- ** CallAnalyticsJob
    callAnalyticsJob_callAnalyticsJobName,
    callAnalyticsJob_callAnalyticsJobStatus,
    callAnalyticsJob_channelDefinitions,
    callAnalyticsJob_completionTime,
    callAnalyticsJob_creationTime,
    callAnalyticsJob_dataAccessRoleArn,
    callAnalyticsJob_failureReason,
    callAnalyticsJob_identifiedLanguageScore,
    callAnalyticsJob_languageCode,
    callAnalyticsJob_media,
    callAnalyticsJob_mediaFormat,
    callAnalyticsJob_mediaSampleRateHertz,
    callAnalyticsJob_settings,
    callAnalyticsJob_startTime,
    callAnalyticsJob_transcript,

    -- ** CallAnalyticsJobSettings
    callAnalyticsJobSettings_contentRedaction,
    callAnalyticsJobSettings_languageIdSettings,
    callAnalyticsJobSettings_languageModelName,
    callAnalyticsJobSettings_languageOptions,
    callAnalyticsJobSettings_vocabularyFilterMethod,
    callAnalyticsJobSettings_vocabularyFilterName,
    callAnalyticsJobSettings_vocabularyName,

    -- ** CallAnalyticsJobSummary
    callAnalyticsJobSummary_callAnalyticsJobName,
    callAnalyticsJobSummary_callAnalyticsJobStatus,
    callAnalyticsJobSummary_completionTime,
    callAnalyticsJobSummary_creationTime,
    callAnalyticsJobSummary_failureReason,
    callAnalyticsJobSummary_languageCode,
    callAnalyticsJobSummary_startTime,

    -- ** CategoryProperties
    categoryProperties_categoryName,
    categoryProperties_createTime,
    categoryProperties_inputType,
    categoryProperties_lastUpdateTime,
    categoryProperties_rules,

    -- ** ChannelDefinition
    channelDefinition_channelId,
    channelDefinition_participantRole,

    -- ** ContentRedaction
    contentRedaction_piiEntityTypes,
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- ** InputDataConfig
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- ** InterruptionFilter
    interruptionFilter_absoluteTimeRange,
    interruptionFilter_negate,
    interruptionFilter_participantRole,
    interruptionFilter_relativeTimeRange,
    interruptionFilter_threshold,

    -- ** JobExecutionSettings
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- ** LanguageCodeItem
    languageCodeItem_durationInSeconds,
    languageCodeItem_languageCode,

    -- ** LanguageIdSettings
    languageIdSettings_languageModelName,
    languageIdSettings_vocabularyFilterName,
    languageIdSettings_vocabularyName,

    -- ** LanguageModel
    languageModel_baseModelName,
    languageModel_createTime,
    languageModel_failureReason,
    languageModel_inputDataConfig,
    languageModel_languageCode,
    languageModel_lastModifiedTime,
    languageModel_modelName,
    languageModel_modelStatus,
    languageModel_upgradeAvailability,

    -- ** Media
    media_mediaFileUri,
    media_redactedMediaFileUri,

    -- ** MedicalTranscript
    medicalTranscript_transcriptFileUri,

    -- ** MedicalTranscriptionJob
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_contentIdentificationType,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_settings,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_tags,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_type,

    -- ** MedicalTranscriptionJobSummary
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_contentIdentificationType,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_failureReason,
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_medicalTranscriptionJobName,
    medicalTranscriptionJobSummary_outputLocationType,
    medicalTranscriptionJobSummary_specialty,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
    medicalTranscriptionJobSummary_type,

    -- ** MedicalTranscriptionSetting
    medicalTranscriptionSetting_channelIdentification,
    medicalTranscriptionSetting_maxAlternatives,
    medicalTranscriptionSetting_maxSpeakerLabels,
    medicalTranscriptionSetting_showAlternatives,
    medicalTranscriptionSetting_showSpeakerLabels,
    medicalTranscriptionSetting_vocabularyName,

    -- ** ModelSettings
    modelSettings_languageModelName,

    -- ** NonTalkTimeFilter
    nonTalkTimeFilter_absoluteTimeRange,
    nonTalkTimeFilter_negate,
    nonTalkTimeFilter_relativeTimeRange,
    nonTalkTimeFilter_threshold,

    -- ** RelativeTimeRange
    relativeTimeRange_endPercentage,
    relativeTimeRange_first,
    relativeTimeRange_last,
    relativeTimeRange_startPercentage,

    -- ** Rule
    rule_interruptionFilter,
    rule_nonTalkTimeFilter,
    rule_sentimentFilter,
    rule_transcriptFilter,

    -- ** SentimentFilter
    sentimentFilter_absoluteTimeRange,
    sentimentFilter_negate,
    sentimentFilter_participantRole,
    sentimentFilter_relativeTimeRange,
    sentimentFilter_sentiments,

    -- ** Settings
    settings_channelIdentification,
    settings_maxAlternatives,
    settings_maxSpeakerLabels,
    settings_showAlternatives,
    settings_showSpeakerLabels,
    settings_vocabularyFilterMethod,
    settings_vocabularyFilterName,
    settings_vocabularyName,

    -- ** Subtitles
    subtitles_formats,
    subtitles_outputStartIndex,

    -- ** SubtitlesOutput
    subtitlesOutput_formats,
    subtitlesOutput_outputStartIndex,
    subtitlesOutput_subtitleFileUris,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Transcript
    transcript_redactedTranscriptFileUri,
    transcript_transcriptFileUri,

    -- ** TranscriptFilter
    transcriptFilter_absoluteTimeRange,
    transcriptFilter_negate,
    transcriptFilter_participantRole,
    transcriptFilter_relativeTimeRange,
    transcriptFilter_transcriptFilterType,
    transcriptFilter_targets,

    -- ** TranscriptionJob
    transcriptionJob_completionTime,
    transcriptionJob_contentRedaction,
    transcriptionJob_creationTime,
    transcriptionJob_failureReason,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_identifyLanguage,
    transcriptionJob_identifyMultipleLanguages,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_languageCode,
    transcriptionJob_languageCodes,
    transcriptionJob_languageIdSettings,
    transcriptionJob_languageOptions,
    transcriptionJob_media,
    transcriptionJob_mediaFormat,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_modelSettings,
    transcriptionJob_settings,
    transcriptionJob_startTime,
    transcriptionJob_subtitles,
    transcriptionJob_tags,
    transcriptionJob_transcript,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_transcriptionJobStatus,

    -- ** TranscriptionJobSummary
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_failureReason,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_identifyMultipleLanguages,
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_languageCodes,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_transcriptionJobStatus,

    -- ** VocabularyFilterInfo
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_lastModifiedTime,
    vocabularyFilterInfo_vocabularyFilterName,

    -- ** VocabularyInfo
    vocabularyInfo_languageCode,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_vocabularyName,
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
