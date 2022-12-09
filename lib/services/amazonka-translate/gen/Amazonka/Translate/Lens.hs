{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Translate.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Lens
  ( -- * Operations

    -- ** CreateParallelData
    createParallelData_description,
    createParallelData_encryptionKey,
    createParallelData_tags,
    createParallelData_name,
    createParallelData_parallelDataConfig,
    createParallelData_clientToken,
    createParallelDataResponse_name,
    createParallelDataResponse_status,
    createParallelDataResponse_httpStatus,

    -- ** DeleteParallelData
    deleteParallelData_name,
    deleteParallelDataResponse_name,
    deleteParallelDataResponse_status,
    deleteParallelDataResponse_httpStatus,

    -- ** DeleteTerminology
    deleteTerminology_name,

    -- ** DescribeTextTranslationJob
    describeTextTranslationJob_jobId,
    describeTextTranslationJobResponse_textTranslationJobProperties,
    describeTextTranslationJobResponse_httpStatus,

    -- ** GetParallelData
    getParallelData_name,
    getParallelDataResponse_auxiliaryDataLocation,
    getParallelDataResponse_dataLocation,
    getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation,
    getParallelDataResponse_parallelDataProperties,
    getParallelDataResponse_httpStatus,

    -- ** GetTerminology
    getTerminology_terminologyDataFormat,
    getTerminology_name,
    getTerminologyResponse_auxiliaryDataLocation,
    getTerminologyResponse_terminologyDataLocation,
    getTerminologyResponse_terminologyProperties,
    getTerminologyResponse_httpStatus,

    -- ** ImportTerminology
    importTerminology_description,
    importTerminology_encryptionKey,
    importTerminology_tags,
    importTerminology_name,
    importTerminology_mergeStrategy,
    importTerminology_terminologyData,
    importTerminologyResponse_auxiliaryDataLocation,
    importTerminologyResponse_terminologyProperties,
    importTerminologyResponse_httpStatus,

    -- ** ListLanguages
    listLanguages_displayLanguageCode,
    listLanguages_maxResults,
    listLanguages_nextToken,
    listLanguagesResponse_displayLanguageCode,
    listLanguagesResponse_languages,
    listLanguagesResponse_nextToken,
    listLanguagesResponse_httpStatus,

    -- ** ListParallelData
    listParallelData_maxResults,
    listParallelData_nextToken,
    listParallelDataResponse_nextToken,
    listParallelDataResponse_parallelDataPropertiesList,
    listParallelDataResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTerminologies
    listTerminologies_maxResults,
    listTerminologies_nextToken,
    listTerminologiesResponse_nextToken,
    listTerminologiesResponse_terminologyPropertiesList,
    listTerminologiesResponse_httpStatus,

    -- ** ListTextTranslationJobs
    listTextTranslationJobs_filter,
    listTextTranslationJobs_maxResults,
    listTextTranslationJobs_nextToken,
    listTextTranslationJobsResponse_nextToken,
    listTextTranslationJobsResponse_textTranslationJobPropertiesList,
    listTextTranslationJobsResponse_httpStatus,

    -- ** StartTextTranslationJob
    startTextTranslationJob_jobName,
    startTextTranslationJob_parallelDataNames,
    startTextTranslationJob_settings,
    startTextTranslationJob_terminologyNames,
    startTextTranslationJob_inputDataConfig,
    startTextTranslationJob_outputDataConfig,
    startTextTranslationJob_dataAccessRoleArn,
    startTextTranslationJob_sourceLanguageCode,
    startTextTranslationJob_targetLanguageCodes,
    startTextTranslationJob_clientToken,
    startTextTranslationJobResponse_jobId,
    startTextTranslationJobResponse_jobStatus,
    startTextTranslationJobResponse_httpStatus,

    -- ** StopTextTranslationJob
    stopTextTranslationJob_jobId,
    stopTextTranslationJobResponse_jobId,
    stopTextTranslationJobResponse_jobStatus,
    stopTextTranslationJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TranslateText
    translateText_settings,
    translateText_terminologyNames,
    translateText_text,
    translateText_sourceLanguageCode,
    translateText_targetLanguageCode,
    translateTextResponse_appliedSettings,
    translateTextResponse_appliedTerminologies,
    translateTextResponse_httpStatus,
    translateTextResponse_translatedText,
    translateTextResponse_sourceLanguageCode,
    translateTextResponse_targetLanguageCode,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateParallelData
    updateParallelData_description,
    updateParallelData_name,
    updateParallelData_parallelDataConfig,
    updateParallelData_clientToken,
    updateParallelDataResponse_latestUpdateAttemptAt,
    updateParallelDataResponse_latestUpdateAttemptStatus,
    updateParallelDataResponse_name,
    updateParallelDataResponse_status,
    updateParallelDataResponse_httpStatus,

    -- * Types

    -- ** AppliedTerminology
    appliedTerminology_name,
    appliedTerminology_terms,

    -- ** EncryptionKey
    encryptionKey_type,
    encryptionKey_id,

    -- ** InputDataConfig
    inputDataConfig_s3Uri,
    inputDataConfig_contentType,

    -- ** JobDetails
    jobDetails_documentsWithErrorsCount,
    jobDetails_inputDocumentsCount,
    jobDetails_translatedDocumentsCount,

    -- ** Language
    language_languageName,
    language_languageCode,

    -- ** OutputDataConfig
    outputDataConfig_encryptionKey,
    outputDataConfig_s3Uri,

    -- ** ParallelDataConfig
    parallelDataConfig_s3Uri,
    parallelDataConfig_format,

    -- ** ParallelDataDataLocation
    parallelDataDataLocation_repositoryType,
    parallelDataDataLocation_location,

    -- ** ParallelDataProperties
    parallelDataProperties_arn,
    parallelDataProperties_createdAt,
    parallelDataProperties_description,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_importedDataSize,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_lastUpdatedAt,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_message,
    parallelDataProperties_name,
    parallelDataProperties_parallelDataConfig,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_status,
    parallelDataProperties_targetLanguageCodes,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Term
    term_sourceText,
    term_targetText,

    -- ** TerminologyData
    terminologyData_directionality,
    terminologyData_file,
    terminologyData_format,

    -- ** TerminologyDataLocation
    terminologyDataLocation_repositoryType,
    terminologyDataLocation_location,

    -- ** TerminologyProperties
    terminologyProperties_arn,
    terminologyProperties_createdAt,
    terminologyProperties_description,
    terminologyProperties_directionality,
    terminologyProperties_encryptionKey,
    terminologyProperties_format,
    terminologyProperties_lastUpdatedAt,
    terminologyProperties_message,
    terminologyProperties_name,
    terminologyProperties_sizeBytes,
    terminologyProperties_skippedTermCount,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_termCount,

    -- ** TextTranslationJobFilter
    textTranslationJobFilter_jobName,
    textTranslationJobFilter_jobStatus,
    textTranslationJobFilter_submittedAfterTime,
    textTranslationJobFilter_submittedBeforeTime,

    -- ** TextTranslationJobProperties
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_message,
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_parallelDataNames,
    textTranslationJobProperties_settings,
    textTranslationJobProperties_sourceLanguageCode,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_terminologyNames,

    -- ** TranslationSettings
    translationSettings_formality,
    translationSettings_profanity,
  )
where

import Amazonka.Translate.CreateParallelData
import Amazonka.Translate.DeleteParallelData
import Amazonka.Translate.DeleteTerminology
import Amazonka.Translate.DescribeTextTranslationJob
import Amazonka.Translate.GetParallelData
import Amazonka.Translate.GetTerminology
import Amazonka.Translate.ImportTerminology
import Amazonka.Translate.ListLanguages
import Amazonka.Translate.ListParallelData
import Amazonka.Translate.ListTagsForResource
import Amazonka.Translate.ListTerminologies
import Amazonka.Translate.ListTextTranslationJobs
import Amazonka.Translate.StartTextTranslationJob
import Amazonka.Translate.StopTextTranslationJob
import Amazonka.Translate.TagResource
import Amazonka.Translate.TranslateText
import Amazonka.Translate.Types.AppliedTerminology
import Amazonka.Translate.Types.EncryptionKey
import Amazonka.Translate.Types.InputDataConfig
import Amazonka.Translate.Types.JobDetails
import Amazonka.Translate.Types.Language
import Amazonka.Translate.Types.OutputDataConfig
import Amazonka.Translate.Types.ParallelDataConfig
import Amazonka.Translate.Types.ParallelDataDataLocation
import Amazonka.Translate.Types.ParallelDataProperties
import Amazonka.Translate.Types.Tag
import Amazonka.Translate.Types.Term
import Amazonka.Translate.Types.TerminologyData
import Amazonka.Translate.Types.TerminologyDataLocation
import Amazonka.Translate.Types.TerminologyProperties
import Amazonka.Translate.Types.TextTranslationJobFilter
import Amazonka.Translate.Types.TextTranslationJobProperties
import Amazonka.Translate.Types.TranslationSettings
import Amazonka.Translate.UntagResource
import Amazonka.Translate.UpdateParallelData
