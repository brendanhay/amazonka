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
    createParallelData_tags,
    createParallelData_description,
    createParallelData_encryptionKey,
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
    getParallelDataResponse_parallelDataProperties,
    getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation,
    getParallelDataResponse_httpStatus,

    -- ** GetTerminology
    getTerminology_terminologyDataFormat,
    getTerminology_name,
    getTerminologyResponse_terminologyDataLocation,
    getTerminologyResponse_auxiliaryDataLocation,
    getTerminologyResponse_terminologyProperties,
    getTerminologyResponse_httpStatus,

    -- ** ImportTerminology
    importTerminology_tags,
    importTerminology_description,
    importTerminology_encryptionKey,
    importTerminology_name,
    importTerminology_mergeStrategy,
    importTerminology_terminologyData,
    importTerminologyResponse_auxiliaryDataLocation,
    importTerminologyResponse_terminologyProperties,
    importTerminologyResponse_httpStatus,

    -- ** ListLanguages
    listLanguages_nextToken,
    listLanguages_maxResults,
    listLanguages_displayLanguageCode,
    listLanguagesResponse_nextToken,
    listLanguagesResponse_languages,
    listLanguagesResponse_displayLanguageCode,
    listLanguagesResponse_httpStatus,

    -- ** ListParallelData
    listParallelData_nextToken,
    listParallelData_maxResults,
    listParallelDataResponse_nextToken,
    listParallelDataResponse_parallelDataPropertiesList,
    listParallelDataResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTerminologies
    listTerminologies_nextToken,
    listTerminologies_maxResults,
    listTerminologiesResponse_nextToken,
    listTerminologiesResponse_terminologyPropertiesList,
    listTerminologiesResponse_httpStatus,

    -- ** ListTextTranslationJobs
    listTextTranslationJobs_nextToken,
    listTextTranslationJobs_filter,
    listTextTranslationJobs_maxResults,
    listTextTranslationJobsResponse_nextToken,
    listTextTranslationJobsResponse_textTranslationJobPropertiesList,
    listTextTranslationJobsResponse_httpStatus,

    -- ** StartTextTranslationJob
    startTextTranslationJob_jobName,
    startTextTranslationJob_terminologyNames,
    startTextTranslationJob_settings,
    startTextTranslationJob_parallelDataNames,
    startTextTranslationJob_inputDataConfig,
    startTextTranslationJob_outputDataConfig,
    startTextTranslationJob_dataAccessRoleArn,
    startTextTranslationJob_sourceLanguageCode,
    startTextTranslationJob_targetLanguageCodes,
    startTextTranslationJob_clientToken,
    startTextTranslationJobResponse_jobStatus,
    startTextTranslationJobResponse_jobId,
    startTextTranslationJobResponse_httpStatus,

    -- ** StopTextTranslationJob
    stopTextTranslationJob_jobId,
    stopTextTranslationJobResponse_jobStatus,
    stopTextTranslationJobResponse_jobId,
    stopTextTranslationJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TranslateText
    translateText_terminologyNames,
    translateText_settings,
    translateText_text,
    translateText_sourceLanguageCode,
    translateText_targetLanguageCode,
    translateTextResponse_appliedTerminologies,
    translateTextResponse_appliedSettings,
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
    updateParallelDataResponse_name,
    updateParallelDataResponse_latestUpdateAttemptStatus,
    updateParallelDataResponse_status,
    updateParallelDataResponse_latestUpdateAttemptAt,
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
    jobDetails_translatedDocumentsCount,
    jobDetails_inputDocumentsCount,

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
    parallelDataProperties_message,
    parallelDataProperties_name,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_lastUpdatedAt,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_arn,
    parallelDataProperties_targetLanguageCodes,
    parallelDataProperties_status,
    parallelDataProperties_description,
    parallelDataProperties_parallelDataConfig,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_importedDataSize,
    parallelDataProperties_createdAt,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Term
    term_targetText,
    term_sourceText,

    -- ** TerminologyData
    terminologyData_directionality,
    terminologyData_file,
    terminologyData_format,

    -- ** TerminologyDataLocation
    terminologyDataLocation_repositoryType,
    terminologyDataLocation_location,

    -- ** TerminologyProperties
    terminologyProperties_skippedTermCount,
    terminologyProperties_message,
    terminologyProperties_name,
    terminologyProperties_lastUpdatedAt,
    terminologyProperties_directionality,
    terminologyProperties_termCount,
    terminologyProperties_format,
    terminologyProperties_sizeBytes,
    terminologyProperties_arn,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_description,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_encryptionKey,
    terminologyProperties_createdAt,

    -- ** TextTranslationJobFilter
    textTranslationJobFilter_jobStatus,
    textTranslationJobFilter_jobName,
    textTranslationJobFilter_submittedBeforeTime,
    textTranslationJobFilter_submittedAfterTime,

    -- ** TextTranslationJobProperties
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_message,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_terminologyNames,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_settings,
    textTranslationJobProperties_sourceLanguageCode,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_parallelDataNames,

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
