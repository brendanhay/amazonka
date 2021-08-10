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

    -- ** GetVocabularyFilter
    getVocabularyFilter_vocabularyFilterName,
    getVocabularyFilterResponse_languageCode,
    getVocabularyFilterResponse_vocabularyFilterName,
    getVocabularyFilterResponse_lastModifiedTime,
    getVocabularyFilterResponse_downloadUri,
    getVocabularyFilterResponse_httpStatus,

    -- ** ListLanguageModels
    listLanguageModels_nextToken,
    listLanguageModels_nameContains,
    listLanguageModels_maxResults,
    listLanguageModels_statusEquals,
    listLanguageModelsResponse_nextToken,
    listLanguageModelsResponse_models,
    listLanguageModelsResponse_httpStatus,

    -- ** StartTranscriptionJob
    startTranscriptionJob_languageCode,
    startTranscriptionJob_mediaFormat,
    startTranscriptionJob_contentRedaction,
    startTranscriptionJob_identifyLanguage,
    startTranscriptionJob_outputKey,
    startTranscriptionJob_modelSettings,
    startTranscriptionJob_outputEncryptionKMSKeyId,
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
    createVocabulary_vocabularyFileUri,
    createVocabulary_vocabularyName,
    createVocabulary_languageCode,
    createVocabularyResponse_languageCode,
    createVocabularyResponse_failureReason,
    createVocabularyResponse_lastModifiedTime,
    createVocabularyResponse_vocabularyState,
    createVocabularyResponse_vocabularyName,
    createVocabularyResponse_httpStatus,

    -- ** UpdateVocabulary
    updateVocabulary_phrases,
    updateVocabulary_vocabularyFileUri,
    updateVocabulary_vocabularyName,
    updateVocabulary_languageCode,
    updateVocabularyResponse_languageCode,
    updateVocabularyResponse_lastModifiedTime,
    updateVocabularyResponse_vocabularyState,
    updateVocabularyResponse_vocabularyName,
    updateVocabularyResponse_httpStatus,

    -- ** DeleteVocabulary
    deleteVocabulary_vocabularyName,

    -- ** ListVocabularyFilters
    listVocabularyFilters_nextToken,
    listVocabularyFilters_nameContains,
    listVocabularyFilters_maxResults,
    listVocabularyFiltersResponse_nextToken,
    listVocabularyFiltersResponse_vocabularyFilters,
    listVocabularyFiltersResponse_httpStatus,

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
    startMedicalTranscriptionJob_outputKey,
    startMedicalTranscriptionJob_outputEncryptionKMSKeyId,
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

    -- ** GetMedicalVocabulary
    getMedicalVocabulary_vocabularyName,
    getMedicalVocabularyResponse_languageCode,
    getMedicalVocabularyResponse_failureReason,
    getMedicalVocabularyResponse_lastModifiedTime,
    getMedicalVocabularyResponse_vocabularyState,
    getMedicalVocabularyResponse_vocabularyName,
    getMedicalVocabularyResponse_downloadUri,
    getMedicalVocabularyResponse_httpStatus,

    -- ** GetTranscriptionJob
    getTranscriptionJob_transcriptionJobName,
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,

    -- ** DeleteLanguageModel
    deleteLanguageModel_modelName,

    -- ** GetVocabulary
    getVocabulary_vocabularyName,
    getVocabularyResponse_languageCode,
    getVocabularyResponse_failureReason,
    getVocabularyResponse_lastModifiedTime,
    getVocabularyResponse_vocabularyState,
    getVocabularyResponse_vocabularyName,
    getVocabularyResponse_downloadUri,
    getVocabularyResponse_httpStatus,

    -- ** GetMedicalTranscriptionJob
    getMedicalTranscriptionJob_medicalTranscriptionJobName,
    getMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    getMedicalTranscriptionJobResponse_httpStatus,

    -- ** CreateVocabularyFilter
    createVocabularyFilter_vocabularyFilterFileUri,
    createVocabularyFilter_words,
    createVocabularyFilter_vocabularyFilterName,
    createVocabularyFilter_languageCode,
    createVocabularyFilterResponse_languageCode,
    createVocabularyFilterResponse_vocabularyFilterName,
    createVocabularyFilterResponse_lastModifiedTime,
    createVocabularyFilterResponse_httpStatus,

    -- ** DeleteVocabularyFilter
    deleteVocabularyFilter_vocabularyFilterName,

    -- ** ListMedicalTranscriptionJobs
    listMedicalTranscriptionJobs_status,
    listMedicalTranscriptionJobs_nextToken,
    listMedicalTranscriptionJobs_maxResults,
    listMedicalTranscriptionJobs_jobNameContains,
    listMedicalTranscriptionJobsResponse_status,
    listMedicalTranscriptionJobsResponse_nextToken,
    listMedicalTranscriptionJobsResponse_medicalTranscriptionJobSummaries,
    listMedicalTranscriptionJobsResponse_httpStatus,

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

    -- ** DeleteMedicalVocabulary
    deleteMedicalVocabulary_vocabularyName,

    -- ** UpdateMedicalVocabulary
    updateMedicalVocabulary_vocabularyFileUri,
    updateMedicalVocabulary_vocabularyName,
    updateMedicalVocabulary_languageCode,
    updateMedicalVocabularyResponse_languageCode,
    updateMedicalVocabularyResponse_lastModifiedTime,
    updateMedicalVocabularyResponse_vocabularyState,
    updateMedicalVocabularyResponse_vocabularyName,
    updateMedicalVocabularyResponse_httpStatus,

    -- ** DescribeLanguageModel
    describeLanguageModel_modelName,
    describeLanguageModelResponse_languageModel,
    describeLanguageModelResponse_httpStatus,

    -- ** CreateMedicalVocabulary
    createMedicalVocabulary_vocabularyName,
    createMedicalVocabulary_languageCode,
    createMedicalVocabulary_vocabularyFileUri,
    createMedicalVocabularyResponse_languageCode,
    createMedicalVocabularyResponse_failureReason,
    createMedicalVocabularyResponse_lastModifiedTime,
    createMedicalVocabularyResponse_vocabularyState,
    createMedicalVocabularyResponse_vocabularyName,
    createMedicalVocabularyResponse_httpStatus,

    -- ** ListMedicalVocabularies
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_maxResults,
    listMedicalVocabularies_stateEquals,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_httpStatus,

    -- * Types

    -- ** ContentRedaction
    contentRedaction_redactionType,
    contentRedaction_redactionOutput,

    -- ** InputDataConfig
    inputDataConfig_tuningDataS3Uri,
    inputDataConfig_s3Uri,
    inputDataConfig_dataAccessRoleArn,

    -- ** JobExecutionSettings
    jobExecutionSettings_allowDeferredExecution,
    jobExecutionSettings_dataAccessRoleArn,

    -- ** LanguageModel
    languageModel_languageCode,
    languageModel_inputDataConfig,
    languageModel_modelStatus,
    languageModel_failureReason,
    languageModel_upgradeAvailability,
    languageModel_createTime,
    languageModel_lastModifiedTime,
    languageModel_modelName,
    languageModel_baseModelName,

    -- ** Media
    media_mediaFileUri,

    -- ** MedicalTranscript
    medicalTranscript_transcriptFileUri,

    -- ** MedicalTranscriptionJob
    medicalTranscriptionJob_languageCode,
    medicalTranscriptionJob_mediaFormat,
    medicalTranscriptionJob_media,
    medicalTranscriptionJob_creationTime,
    medicalTranscriptionJob_completionTime,
    medicalTranscriptionJob_transcript,
    medicalTranscriptionJob_startTime,
    medicalTranscriptionJob_transcriptionJobStatus,
    medicalTranscriptionJob_specialty,
    medicalTranscriptionJob_failureReason,
    medicalTranscriptionJob_mediaSampleRateHertz,
    medicalTranscriptionJob_type,
    medicalTranscriptionJob_medicalTranscriptionJobName,
    medicalTranscriptionJob_settings,

    -- ** MedicalTranscriptionJobSummary
    medicalTranscriptionJobSummary_languageCode,
    medicalTranscriptionJobSummary_creationTime,
    medicalTranscriptionJobSummary_completionTime,
    medicalTranscriptionJobSummary_startTime,
    medicalTranscriptionJobSummary_transcriptionJobStatus,
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

    -- ** Settings
    settings_vocabularyFilterMethod,
    settings_vocabularyFilterName,
    settings_showAlternatives,
    settings_channelIdentification,
    settings_maxAlternatives,
    settings_showSpeakerLabels,
    settings_vocabularyName,
    settings_maxSpeakerLabels,

    -- ** Transcript
    transcript_transcriptFileUri,
    transcript_redactedTranscriptFileUri,

    -- ** TranscriptionJob
    transcriptionJob_languageCode,
    transcriptionJob_mediaFormat,
    transcriptionJob_contentRedaction,
    transcriptionJob_media,
    transcriptionJob_creationTime,
    transcriptionJob_completionTime,
    transcriptionJob_transcriptionJobName,
    transcriptionJob_transcript,
    transcriptionJob_identifyLanguage,
    transcriptionJob_startTime,
    transcriptionJob_transcriptionJobStatus,
    transcriptionJob_modelSettings,
    transcriptionJob_identifiedLanguageScore,
    transcriptionJob_failureReason,
    transcriptionJob_mediaSampleRateHertz,
    transcriptionJob_jobExecutionSettings,
    transcriptionJob_settings,
    transcriptionJob_languageOptions,

    -- ** TranscriptionJobSummary
    transcriptionJobSummary_languageCode,
    transcriptionJobSummary_contentRedaction,
    transcriptionJobSummary_creationTime,
    transcriptionJobSummary_completionTime,
    transcriptionJobSummary_transcriptionJobName,
    transcriptionJobSummary_identifyLanguage,
    transcriptionJobSummary_startTime,
    transcriptionJobSummary_transcriptionJobStatus,
    transcriptionJobSummary_modelSettings,
    transcriptionJobSummary_outputLocationType,
    transcriptionJobSummary_identifiedLanguageScore,
    transcriptionJobSummary_failureReason,

    -- ** VocabularyFilterInfo
    vocabularyFilterInfo_languageCode,
    vocabularyFilterInfo_vocabularyFilterName,
    vocabularyFilterInfo_lastModifiedTime,

    -- ** VocabularyInfo
    vocabularyInfo_languageCode,
    vocabularyInfo_lastModifiedTime,
    vocabularyInfo_vocabularyState,
    vocabularyInfo_vocabularyName,
  )
where

import Network.AWS.Transcribe.CreateLanguageModel
import Network.AWS.Transcribe.CreateMedicalVocabulary
import Network.AWS.Transcribe.CreateVocabulary
import Network.AWS.Transcribe.CreateVocabularyFilter
import Network.AWS.Transcribe.DeleteLanguageModel
import Network.AWS.Transcribe.DeleteMedicalTranscriptionJob
import Network.AWS.Transcribe.DeleteMedicalVocabulary
import Network.AWS.Transcribe.DeleteTranscriptionJob
import Network.AWS.Transcribe.DeleteVocabulary
import Network.AWS.Transcribe.DeleteVocabularyFilter
import Network.AWS.Transcribe.DescribeLanguageModel
import Network.AWS.Transcribe.GetMedicalTranscriptionJob
import Network.AWS.Transcribe.GetMedicalVocabulary
import Network.AWS.Transcribe.GetTranscriptionJob
import Network.AWS.Transcribe.GetVocabulary
import Network.AWS.Transcribe.GetVocabularyFilter
import Network.AWS.Transcribe.ListLanguageModels
import Network.AWS.Transcribe.ListMedicalTranscriptionJobs
import Network.AWS.Transcribe.ListMedicalVocabularies
import Network.AWS.Transcribe.ListTranscriptionJobs
import Network.AWS.Transcribe.ListVocabularies
import Network.AWS.Transcribe.ListVocabularyFilters
import Network.AWS.Transcribe.StartMedicalTranscriptionJob
import Network.AWS.Transcribe.StartTranscriptionJob
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.JobExecutionSettings
import Network.AWS.Transcribe.Types.LanguageModel
import Network.AWS.Transcribe.Types.Media
import Network.AWS.Transcribe.Types.MedicalTranscript
import Network.AWS.Transcribe.Types.MedicalTranscriptionJob
import Network.AWS.Transcribe.Types.MedicalTranscriptionJobSummary
import Network.AWS.Transcribe.Types.MedicalTranscriptionSetting
import Network.AWS.Transcribe.Types.ModelSettings
import Network.AWS.Transcribe.Types.Settings
import Network.AWS.Transcribe.Types.Transcript
import Network.AWS.Transcribe.Types.TranscriptionJob
import Network.AWS.Transcribe.Types.TranscriptionJobSummary
import Network.AWS.Transcribe.Types.VocabularyFilterInfo
import Network.AWS.Transcribe.Types.VocabularyInfo
import Network.AWS.Transcribe.UpdateMedicalVocabulary
import Network.AWS.Transcribe.UpdateVocabulary
import Network.AWS.Transcribe.UpdateVocabularyFilter
