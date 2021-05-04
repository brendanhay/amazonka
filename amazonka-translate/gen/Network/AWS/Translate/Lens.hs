{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Lens
  ( -- * Operations

    -- ** CreateParallelData
    createParallelData_encryptionKey,
    createParallelData_description,
    createParallelData_name,
    createParallelData_parallelDataConfig,
    createParallelData_clientToken,
    createParallelDataResponse_status,
    createParallelDataResponse_name,
    createParallelDataResponse_httpStatus,

    -- ** DescribeTextTranslationJob
    describeTextTranslationJob_jobId,
    describeTextTranslationJobResponse_textTranslationJobProperties,
    describeTextTranslationJobResponse_httpStatus,

    -- ** StopTextTranslationJob
    stopTextTranslationJob_jobId,
    stopTextTranslationJobResponse_jobStatus,
    stopTextTranslationJobResponse_jobId,
    stopTextTranslationJobResponse_httpStatus,

    -- ** StartTextTranslationJob
    startTextTranslationJob_parallelDataNames,
    startTextTranslationJob_terminologyNames,
    startTextTranslationJob_jobName,
    startTextTranslationJob_inputDataConfig,
    startTextTranslationJob_outputDataConfig,
    startTextTranslationJob_dataAccessRoleArn,
    startTextTranslationJob_sourceLanguageCode,
    startTextTranslationJob_targetLanguageCodes,
    startTextTranslationJob_clientToken,
    startTextTranslationJobResponse_jobStatus,
    startTextTranslationJobResponse_jobId,
    startTextTranslationJobResponse_httpStatus,

    -- ** ImportTerminology
    importTerminology_encryptionKey,
    importTerminology_description,
    importTerminology_name,
    importTerminology_mergeStrategy,
    importTerminology_terminologyData,
    importTerminologyResponse_terminologyProperties,
    importTerminologyResponse_httpStatus,

    -- ** ListTextTranslationJobs
    listTextTranslationJobs_nextToken,
    listTextTranslationJobs_maxResults,
    listTextTranslationJobs_filter,
    listTextTranslationJobsResponse_nextToken,
    listTextTranslationJobsResponse_textTranslationJobPropertiesList,
    listTextTranslationJobsResponse_httpStatus,

    -- ** GetParallelData
    getParallelData_name,
    getParallelDataResponse_auxiliaryDataLocation,
    getParallelDataResponse_parallelDataProperties,
    getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation,
    getParallelDataResponse_dataLocation,
    getParallelDataResponse_httpStatus,

    -- ** DeleteParallelData
    deleteParallelData_name,
    deleteParallelDataResponse_status,
    deleteParallelDataResponse_name,
    deleteParallelDataResponse_httpStatus,

    -- ** UpdateParallelData
    updateParallelData_description,
    updateParallelData_name,
    updateParallelData_parallelDataConfig,
    updateParallelData_clientToken,
    updateParallelDataResponse_status,
    updateParallelDataResponse_latestUpdateAttemptStatus,
    updateParallelDataResponse_latestUpdateAttemptAt,
    updateParallelDataResponse_name,
    updateParallelDataResponse_httpStatus,

    -- ** DeleteTerminology
    deleteTerminology_name,

    -- ** ListTerminologies
    listTerminologies_nextToken,
    listTerminologies_maxResults,
    listTerminologiesResponse_nextToken,
    listTerminologiesResponse_terminologyPropertiesList,
    listTerminologiesResponse_httpStatus,

    -- ** ListParallelData
    listParallelData_nextToken,
    listParallelData_maxResults,
    listParallelDataResponse_nextToken,
    listParallelDataResponse_parallelDataPropertiesList,
    listParallelDataResponse_httpStatus,

    -- ** TranslateText
    translateText_terminologyNames,
    translateText_text,
    translateText_sourceLanguageCode,
    translateText_targetLanguageCode,
    translateTextResponse_appliedTerminologies,
    translateTextResponse_httpStatus,
    translateTextResponse_translatedText,
    translateTextResponse_sourceLanguageCode,
    translateTextResponse_targetLanguageCode,

    -- ** GetTerminology
    getTerminology_name,
    getTerminology_terminologyDataFormat,
    getTerminologyResponse_terminologyDataLocation,
    getTerminologyResponse_terminologyProperties,
    getTerminologyResponse_httpStatus,

    -- * Types

    -- ** AppliedTerminology
    appliedTerminology_terms,
    appliedTerminology_name,

    -- ** EncryptionKey
    encryptionKey_type,
    encryptionKey_id,

    -- ** InputDataConfig
    inputDataConfig_s3Uri,
    inputDataConfig_contentType,

    -- ** JobDetails
    jobDetails_inputDocumentsCount,
    jobDetails_documentsWithErrorsCount,
    jobDetails_translatedDocumentsCount,

    -- ** OutputDataConfig
    outputDataConfig_s3Uri,

    -- ** ParallelDataConfig
    parallelDataConfig_s3Uri,
    parallelDataConfig_format,

    -- ** ParallelDataDataLocation
    parallelDataDataLocation_repositoryType,
    parallelDataDataLocation_location,

    -- ** ParallelDataProperties
    parallelDataProperties_status,
    parallelDataProperties_importedDataSize,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_message,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_arn,
    parallelDataProperties_targetLanguageCodes,
    parallelDataProperties_createdAt,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_name,
    parallelDataProperties_parallelDataConfig,
    parallelDataProperties_description,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_lastUpdatedAt,

    -- ** Term
    term_targetText,
    term_sourceText,

    -- ** TerminologyData
    terminologyData_file,
    terminologyData_format,

    -- ** TerminologyDataLocation
    terminologyDataLocation_repositoryType,
    terminologyDataLocation_location,

    -- ** TerminologyProperties
    terminologyProperties_encryptionKey,
    terminologyProperties_arn,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_createdAt,
    terminologyProperties_name,
    terminologyProperties_sizeBytes,
    terminologyProperties_description,
    terminologyProperties_termCount,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_lastUpdatedAt,

    -- ** TextTranslationJobFilter
    textTranslationJobFilter_jobStatus,
    textTranslationJobFilter_submittedAfterTime,
    textTranslationJobFilter_submittedBeforeTime,
    textTranslationJobFilter_jobName,

    -- ** TextTranslationJobProperties
    textTranslationJobProperties_parallelDataNames,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_message,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_terminologyNames,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_sourceLanguageCode,
  )
where

import Network.AWS.Translate.CreateParallelData
import Network.AWS.Translate.DeleteParallelData
import Network.AWS.Translate.DeleteTerminology
import Network.AWS.Translate.DescribeTextTranslationJob
import Network.AWS.Translate.GetParallelData
import Network.AWS.Translate.GetTerminology
import Network.AWS.Translate.ImportTerminology
import Network.AWS.Translate.ListParallelData
import Network.AWS.Translate.ListTerminologies
import Network.AWS.Translate.ListTextTranslationJobs
import Network.AWS.Translate.StartTextTranslationJob
import Network.AWS.Translate.StopTextTranslationJob
import Network.AWS.Translate.TranslateText
import Network.AWS.Translate.Types.AppliedTerminology
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.InputDataConfig
import Network.AWS.Translate.Types.JobDetails
import Network.AWS.Translate.Types.OutputDataConfig
import Network.AWS.Translate.Types.ParallelDataConfig
import Network.AWS.Translate.Types.ParallelDataDataLocation
import Network.AWS.Translate.Types.ParallelDataProperties
import Network.AWS.Translate.Types.Term
import Network.AWS.Translate.Types.TerminologyData
import Network.AWS.Translate.Types.TerminologyDataLocation
import Network.AWS.Translate.Types.TerminologyProperties
import Network.AWS.Translate.Types.TextTranslationJobFilter
import Network.AWS.Translate.Types.TextTranslationJobProperties
import Network.AWS.Translate.UpdateParallelData
