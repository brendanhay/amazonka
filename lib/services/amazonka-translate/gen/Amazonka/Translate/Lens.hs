{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Translate.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Lens
  ( -- * Operations

    -- ** DescribeTextTranslationJob
    describeTextTranslationJob_jobId,
    describeTextTranslationJobResponse_textTranslationJobProperties,
    describeTextTranslationJobResponse_httpStatus,

    -- ** ListTerminologies
    listTerminologies_nextToken,
    listTerminologies_maxResults,
    listTerminologiesResponse_terminologyPropertiesList,
    listTerminologiesResponse_nextToken,
    listTerminologiesResponse_httpStatus,

    -- ** CreateParallelData
    createParallelData_encryptionKey,
    createParallelData_description,
    createParallelData_name,
    createParallelData_parallelDataConfig,
    createParallelData_clientToken,
    createParallelDataResponse_status,
    createParallelDataResponse_name,
    createParallelDataResponse_httpStatus,

    -- ** UpdateParallelData
    updateParallelData_description,
    updateParallelData_name,
    updateParallelData_parallelDataConfig,
    updateParallelData_clientToken,
    updateParallelDataResponse_status,
    updateParallelDataResponse_name,
    updateParallelDataResponse_latestUpdateAttemptAt,
    updateParallelDataResponse_latestUpdateAttemptStatus,
    updateParallelDataResponse_httpStatus,

    -- ** DeleteParallelData
    deleteParallelData_name,
    deleteParallelDataResponse_status,
    deleteParallelDataResponse_name,
    deleteParallelDataResponse_httpStatus,

    -- ** GetParallelData
    getParallelData_name,
    getParallelDataResponse_parallelDataProperties,
    getParallelDataResponse_dataLocation,
    getParallelDataResponse_auxiliaryDataLocation,
    getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation,
    getParallelDataResponse_httpStatus,

    -- ** GetTerminology
    getTerminology_name,
    getTerminology_terminologyDataFormat,
    getTerminologyResponse_terminologyProperties,
    getTerminologyResponse_terminologyDataLocation,
    getTerminologyResponse_httpStatus,

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

    -- ** ImportTerminology
    importTerminology_encryptionKey,
    importTerminology_description,
    importTerminology_name,
    importTerminology_mergeStrategy,
    importTerminology_terminologyData,
    importTerminologyResponse_terminologyProperties,
    importTerminologyResponse_httpStatus,

    -- ** StopTextTranslationJob
    stopTextTranslationJob_jobId,
    stopTextTranslationJobResponse_jobId,
    stopTextTranslationJobResponse_jobStatus,
    stopTextTranslationJobResponse_httpStatus,

    -- ** DeleteTerminology
    deleteTerminology_name,

    -- ** ListTextTranslationJobs
    listTextTranslationJobs_nextToken,
    listTextTranslationJobs_filter,
    listTextTranslationJobs_maxResults,
    listTextTranslationJobsResponse_textTranslationJobPropertiesList,
    listTextTranslationJobsResponse_nextToken,
    listTextTranslationJobsResponse_httpStatus,

    -- ** StartTextTranslationJob
    startTextTranslationJob_jobName,
    startTextTranslationJob_parallelDataNames,
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

    -- ** ListParallelData
    listParallelData_nextToken,
    listParallelData_maxResults,
    listParallelDataResponse_parallelDataPropertiesList,
    listParallelDataResponse_nextToken,
    listParallelDataResponse_httpStatus,

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
    jobDetails_translatedDocumentsCount,
    jobDetails_documentsWithErrorsCount,
    jobDetails_inputDocumentsCount,

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
    parallelDataProperties_lastUpdatedAt,
    parallelDataProperties_importedRecordCount,
    parallelDataProperties_arn,
    parallelDataProperties_targetLanguageCodes,
    parallelDataProperties_createdAt,
    parallelDataProperties_failedRecordCount,
    parallelDataProperties_importedDataSize,
    parallelDataProperties_name,
    parallelDataProperties_sourceLanguageCode,
    parallelDataProperties_latestUpdateAttemptAt,
    parallelDataProperties_encryptionKey,
    parallelDataProperties_latestUpdateAttemptStatus,
    parallelDataProperties_message,
    parallelDataProperties_description,
    parallelDataProperties_skippedRecordCount,
    parallelDataProperties_parallelDataConfig,

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
    terminologyProperties_sizeBytes,
    terminologyProperties_lastUpdatedAt,
    terminologyProperties_arn,
    terminologyProperties_targetLanguageCodes,
    terminologyProperties_createdAt,
    terminologyProperties_name,
    terminologyProperties_sourceLanguageCode,
    terminologyProperties_termCount,
    terminologyProperties_encryptionKey,
    terminologyProperties_description,

    -- ** TextTranslationJobFilter
    textTranslationJobFilter_submittedBeforeTime,
    textTranslationJobFilter_submittedAfterTime,
    textTranslationJobFilter_jobName,
    textTranslationJobFilter_jobStatus,

    -- ** TextTranslationJobProperties
    textTranslationJobProperties_jobId,
    textTranslationJobProperties_targetLanguageCodes,
    textTranslationJobProperties_jobName,
    textTranslationJobProperties_submittedTime,
    textTranslationJobProperties_inputDataConfig,
    textTranslationJobProperties_parallelDataNames,
    textTranslationJobProperties_terminologyNames,
    textTranslationJobProperties_sourceLanguageCode,
    textTranslationJobProperties_endTime,
    textTranslationJobProperties_outputDataConfig,
    textTranslationJobProperties_jobDetails,
    textTranslationJobProperties_dataAccessRoleArn,
    textTranslationJobProperties_jobStatus,
    textTranslationJobProperties_message,
  )
where

import Amazonka.Translate.CreateParallelData
import Amazonka.Translate.DeleteParallelData
import Amazonka.Translate.DeleteTerminology
import Amazonka.Translate.DescribeTextTranslationJob
import Amazonka.Translate.GetParallelData
import Amazonka.Translate.GetTerminology
import Amazonka.Translate.ImportTerminology
import Amazonka.Translate.ListParallelData
import Amazonka.Translate.ListTerminologies
import Amazonka.Translate.ListTextTranslationJobs
import Amazonka.Translate.StartTextTranslationJob
import Amazonka.Translate.StopTextTranslationJob
import Amazonka.Translate.TranslateText
import Amazonka.Translate.Types.AppliedTerminology
import Amazonka.Translate.Types.EncryptionKey
import Amazonka.Translate.Types.InputDataConfig
import Amazonka.Translate.Types.JobDetails
import Amazonka.Translate.Types.OutputDataConfig
import Amazonka.Translate.Types.ParallelDataConfig
import Amazonka.Translate.Types.ParallelDataDataLocation
import Amazonka.Translate.Types.ParallelDataProperties
import Amazonka.Translate.Types.Term
import Amazonka.Translate.Types.TerminologyData
import Amazonka.Translate.Types.TerminologyDataLocation
import Amazonka.Translate.Types.TerminologyProperties
import Amazonka.Translate.Types.TextTranslationJobFilter
import Amazonka.Translate.Types.TextTranslationJobProperties
import Amazonka.Translate.UpdateParallelData
