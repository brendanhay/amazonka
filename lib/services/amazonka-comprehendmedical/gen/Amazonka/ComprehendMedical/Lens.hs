{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComprehendMedical.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Lens
  ( -- * Operations

    -- ** DescribeEntitiesDetectionV2Job
    describeEntitiesDetectionV2Job_jobId,
    describeEntitiesDetectionV2JobResponse_comprehendMedicalAsyncJobProperties,
    describeEntitiesDetectionV2JobResponse_httpStatus,

    -- ** DescribeICD10CMInferenceJob
    describeICD10CMInferenceJob_jobId,
    describeICD10CMInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeICD10CMInferenceJobResponse_httpStatus,

    -- ** DescribePHIDetectionJob
    describePHIDetectionJob_jobId,
    describePHIDetectionJobResponse_comprehendMedicalAsyncJobProperties,
    describePHIDetectionJobResponse_httpStatus,

    -- ** DescribeRxNormInferenceJob
    describeRxNormInferenceJob_jobId,
    describeRxNormInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeRxNormInferenceJobResponse_httpStatus,

    -- ** DescribeSNOMEDCTInferenceJob
    describeSNOMEDCTInferenceJob_jobId,
    describeSNOMEDCTInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeSNOMEDCTInferenceJobResponse_httpStatus,

    -- ** DetectEntitiesV2
    detectEntitiesV2_text,
    detectEntitiesV2Response_paginationToken,
    detectEntitiesV2Response_unmappedAttributes,
    detectEntitiesV2Response_httpStatus,
    detectEntitiesV2Response_entities,
    detectEntitiesV2Response_modelVersion,

    -- ** DetectPHI
    detectPHI_text,
    detectPHIResponse_paginationToken,
    detectPHIResponse_httpStatus,
    detectPHIResponse_entities,
    detectPHIResponse_modelVersion,

    -- ** InferICD10CM
    inferICD10CM_text,
    inferICD10CMResponse_modelVersion,
    inferICD10CMResponse_paginationToken,
    inferICD10CMResponse_httpStatus,
    inferICD10CMResponse_entities,

    -- ** InferRxNorm
    inferRxNorm_text,
    inferRxNormResponse_modelVersion,
    inferRxNormResponse_paginationToken,
    inferRxNormResponse_httpStatus,
    inferRxNormResponse_entities,

    -- ** InferSNOMEDCT
    inferSNOMEDCT_text,
    inferSNOMEDCTResponse_characters,
    inferSNOMEDCTResponse_modelVersion,
    inferSNOMEDCTResponse_paginationToken,
    inferSNOMEDCTResponse_sNOMEDCTDetails,
    inferSNOMEDCTResponse_httpStatus,
    inferSNOMEDCTResponse_entities,

    -- ** ListEntitiesDetectionV2Jobs
    listEntitiesDetectionV2Jobs_filter,
    listEntitiesDetectionV2Jobs_maxResults,
    listEntitiesDetectionV2Jobs_nextToken,
    listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listEntitiesDetectionV2JobsResponse_nextToken,
    listEntitiesDetectionV2JobsResponse_httpStatus,

    -- ** ListICD10CMInferenceJobs
    listICD10CMInferenceJobs_filter,
    listICD10CMInferenceJobs_maxResults,
    listICD10CMInferenceJobs_nextToken,
    listICD10CMInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listICD10CMInferenceJobsResponse_nextToken,
    listICD10CMInferenceJobsResponse_httpStatus,

    -- ** ListPHIDetectionJobs
    listPHIDetectionJobs_filter,
    listPHIDetectionJobs_maxResults,
    listPHIDetectionJobs_nextToken,
    listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listPHIDetectionJobsResponse_nextToken,
    listPHIDetectionJobsResponse_httpStatus,

    -- ** ListRxNormInferenceJobs
    listRxNormInferenceJobs_filter,
    listRxNormInferenceJobs_maxResults,
    listRxNormInferenceJobs_nextToken,
    listRxNormInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listRxNormInferenceJobsResponse_nextToken,
    listRxNormInferenceJobsResponse_httpStatus,

    -- ** ListSNOMEDCTInferenceJobs
    listSNOMEDCTInferenceJobs_filter,
    listSNOMEDCTInferenceJobs_maxResults,
    listSNOMEDCTInferenceJobs_nextToken,
    listSNOMEDCTInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listSNOMEDCTInferenceJobsResponse_nextToken,
    listSNOMEDCTInferenceJobsResponse_httpStatus,

    -- ** StartEntitiesDetectionV2Job
    startEntitiesDetectionV2Job_clientRequestToken,
    startEntitiesDetectionV2Job_jobName,
    startEntitiesDetectionV2Job_kmsKey,
    startEntitiesDetectionV2Job_inputDataConfig,
    startEntitiesDetectionV2Job_outputDataConfig,
    startEntitiesDetectionV2Job_dataAccessRoleArn,
    startEntitiesDetectionV2Job_languageCode,
    startEntitiesDetectionV2JobResponse_jobId,
    startEntitiesDetectionV2JobResponse_httpStatus,

    -- ** StartICD10CMInferenceJob
    startICD10CMInferenceJob_clientRequestToken,
    startICD10CMInferenceJob_jobName,
    startICD10CMInferenceJob_kmsKey,
    startICD10CMInferenceJob_inputDataConfig,
    startICD10CMInferenceJob_outputDataConfig,
    startICD10CMInferenceJob_dataAccessRoleArn,
    startICD10CMInferenceJob_languageCode,
    startICD10CMInferenceJobResponse_jobId,
    startICD10CMInferenceJobResponse_httpStatus,

    -- ** StartPHIDetectionJob
    startPHIDetectionJob_clientRequestToken,
    startPHIDetectionJob_jobName,
    startPHIDetectionJob_kmsKey,
    startPHIDetectionJob_inputDataConfig,
    startPHIDetectionJob_outputDataConfig,
    startPHIDetectionJob_dataAccessRoleArn,
    startPHIDetectionJob_languageCode,
    startPHIDetectionJobResponse_jobId,
    startPHIDetectionJobResponse_httpStatus,

    -- ** StartRxNormInferenceJob
    startRxNormInferenceJob_clientRequestToken,
    startRxNormInferenceJob_jobName,
    startRxNormInferenceJob_kmsKey,
    startRxNormInferenceJob_inputDataConfig,
    startRxNormInferenceJob_outputDataConfig,
    startRxNormInferenceJob_dataAccessRoleArn,
    startRxNormInferenceJob_languageCode,
    startRxNormInferenceJobResponse_jobId,
    startRxNormInferenceJobResponse_httpStatus,

    -- ** StartSNOMEDCTInferenceJob
    startSNOMEDCTInferenceJob_clientRequestToken,
    startSNOMEDCTInferenceJob_jobName,
    startSNOMEDCTInferenceJob_kmsKey,
    startSNOMEDCTInferenceJob_inputDataConfig,
    startSNOMEDCTInferenceJob_outputDataConfig,
    startSNOMEDCTInferenceJob_dataAccessRoleArn,
    startSNOMEDCTInferenceJob_languageCode,
    startSNOMEDCTInferenceJobResponse_jobId,
    startSNOMEDCTInferenceJobResponse_httpStatus,

    -- ** StopEntitiesDetectionV2Job
    stopEntitiesDetectionV2Job_jobId,
    stopEntitiesDetectionV2JobResponse_jobId,
    stopEntitiesDetectionV2JobResponse_httpStatus,

    -- ** StopICD10CMInferenceJob
    stopICD10CMInferenceJob_jobId,
    stopICD10CMInferenceJobResponse_jobId,
    stopICD10CMInferenceJobResponse_httpStatus,

    -- ** StopPHIDetectionJob
    stopPHIDetectionJob_jobId,
    stopPHIDetectionJobResponse_jobId,
    stopPHIDetectionJobResponse_httpStatus,

    -- ** StopRxNormInferenceJob
    stopRxNormInferenceJob_jobId,
    stopRxNormInferenceJobResponse_jobId,
    stopRxNormInferenceJobResponse_httpStatus,

    -- ** StopSNOMEDCTInferenceJob
    stopSNOMEDCTInferenceJob_jobId,
    stopSNOMEDCTInferenceJobResponse_jobId,
    stopSNOMEDCTInferenceJobResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_beginOffset,
    attribute_category,
    attribute_endOffset,
    attribute_id,
    attribute_relationshipScore,
    attribute_relationshipType,
    attribute_score,
    attribute_text,
    attribute_traits,
    attribute_type,

    -- ** Characters
    characters_originalTextCharacters,

    -- ** ComprehendMedicalAsyncJobFilter
    comprehendMedicalAsyncJobFilter_jobName,
    comprehendMedicalAsyncJobFilter_jobStatus,
    comprehendMedicalAsyncJobFilter_submitTimeAfter,
    comprehendMedicalAsyncJobFilter_submitTimeBefore,

    -- ** ComprehendMedicalAsyncJobProperties
    comprehendMedicalAsyncJobProperties_dataAccessRoleArn,
    comprehendMedicalAsyncJobProperties_endTime,
    comprehendMedicalAsyncJobProperties_expirationTime,
    comprehendMedicalAsyncJobProperties_inputDataConfig,
    comprehendMedicalAsyncJobProperties_jobId,
    comprehendMedicalAsyncJobProperties_jobName,
    comprehendMedicalAsyncJobProperties_jobStatus,
    comprehendMedicalAsyncJobProperties_kmsKey,
    comprehendMedicalAsyncJobProperties_languageCode,
    comprehendMedicalAsyncJobProperties_manifestFilePath,
    comprehendMedicalAsyncJobProperties_message,
    comprehendMedicalAsyncJobProperties_modelVersion,
    comprehendMedicalAsyncJobProperties_outputDataConfig,
    comprehendMedicalAsyncJobProperties_submitTime,

    -- ** Entity
    entity_attributes,
    entity_beginOffset,
    entity_category,
    entity_endOffset,
    entity_id,
    entity_score,
    entity_text,
    entity_traits,
    entity_type,

    -- ** ICD10CMAttribute
    iCD10CMAttribute_beginOffset,
    iCD10CMAttribute_category,
    iCD10CMAttribute_endOffset,
    iCD10CMAttribute_id,
    iCD10CMAttribute_relationshipScore,
    iCD10CMAttribute_relationshipType,
    iCD10CMAttribute_score,
    iCD10CMAttribute_text,
    iCD10CMAttribute_traits,
    iCD10CMAttribute_type,

    -- ** ICD10CMConcept
    iCD10CMConcept_code,
    iCD10CMConcept_description,
    iCD10CMConcept_score,

    -- ** ICD10CMEntity
    iCD10CMEntity_attributes,
    iCD10CMEntity_beginOffset,
    iCD10CMEntity_category,
    iCD10CMEntity_endOffset,
    iCD10CMEntity_iCD10CMConcepts,
    iCD10CMEntity_id,
    iCD10CMEntity_score,
    iCD10CMEntity_text,
    iCD10CMEntity_traits,
    iCD10CMEntity_type,

    -- ** ICD10CMTrait
    iCD10CMTrait_name,
    iCD10CMTrait_score,

    -- ** InputDataConfig
    inputDataConfig_s3Key,
    inputDataConfig_s3Bucket,

    -- ** OutputDataConfig
    outputDataConfig_s3Key,
    outputDataConfig_s3Bucket,

    -- ** RxNormAttribute
    rxNormAttribute_beginOffset,
    rxNormAttribute_endOffset,
    rxNormAttribute_id,
    rxNormAttribute_relationshipScore,
    rxNormAttribute_score,
    rxNormAttribute_text,
    rxNormAttribute_traits,
    rxNormAttribute_type,

    -- ** RxNormConcept
    rxNormConcept_code,
    rxNormConcept_description,
    rxNormConcept_score,

    -- ** RxNormEntity
    rxNormEntity_attributes,
    rxNormEntity_beginOffset,
    rxNormEntity_category,
    rxNormEntity_endOffset,
    rxNormEntity_id,
    rxNormEntity_rxNormConcepts,
    rxNormEntity_score,
    rxNormEntity_text,
    rxNormEntity_traits,
    rxNormEntity_type,

    -- ** RxNormTrait
    rxNormTrait_name,
    rxNormTrait_score,

    -- ** SNOMEDCTAttribute
    sNOMEDCTAttribute_beginOffset,
    sNOMEDCTAttribute_category,
    sNOMEDCTAttribute_endOffset,
    sNOMEDCTAttribute_id,
    sNOMEDCTAttribute_relationshipScore,
    sNOMEDCTAttribute_relationshipType,
    sNOMEDCTAttribute_sNOMEDCTConcepts,
    sNOMEDCTAttribute_score,
    sNOMEDCTAttribute_text,
    sNOMEDCTAttribute_traits,
    sNOMEDCTAttribute_type,

    -- ** SNOMEDCTConcept
    sNOMEDCTConcept_code,
    sNOMEDCTConcept_description,
    sNOMEDCTConcept_score,

    -- ** SNOMEDCTDetails
    sNOMEDCTDetails_edition,
    sNOMEDCTDetails_language,
    sNOMEDCTDetails_versionDate,

    -- ** SNOMEDCTEntity
    sNOMEDCTEntity_attributes,
    sNOMEDCTEntity_beginOffset,
    sNOMEDCTEntity_category,
    sNOMEDCTEntity_endOffset,
    sNOMEDCTEntity_id,
    sNOMEDCTEntity_sNOMEDCTConcepts,
    sNOMEDCTEntity_score,
    sNOMEDCTEntity_text,
    sNOMEDCTEntity_traits,
    sNOMEDCTEntity_type,

    -- ** SNOMEDCTTrait
    sNOMEDCTTrait_name,
    sNOMEDCTTrait_score,

    -- ** Trait
    trait_name,
    trait_score,

    -- ** UnmappedAttribute
    unmappedAttribute_attribute,
    unmappedAttribute_type,
  )
where

import Amazonka.ComprehendMedical.DescribeEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.DescribeICD10CMInferenceJob
import Amazonka.ComprehendMedical.DescribePHIDetectionJob
import Amazonka.ComprehendMedical.DescribeRxNormInferenceJob
import Amazonka.ComprehendMedical.DescribeSNOMEDCTInferenceJob
import Amazonka.ComprehendMedical.DetectEntitiesV2
import Amazonka.ComprehendMedical.DetectPHI
import Amazonka.ComprehendMedical.InferICD10CM
import Amazonka.ComprehendMedical.InferRxNorm
import Amazonka.ComprehendMedical.InferSNOMEDCT
import Amazonka.ComprehendMedical.ListEntitiesDetectionV2Jobs
import Amazonka.ComprehendMedical.ListICD10CMInferenceJobs
import Amazonka.ComprehendMedical.ListPHIDetectionJobs
import Amazonka.ComprehendMedical.ListRxNormInferenceJobs
import Amazonka.ComprehendMedical.ListSNOMEDCTInferenceJobs
import Amazonka.ComprehendMedical.StartEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.StartICD10CMInferenceJob
import Amazonka.ComprehendMedical.StartPHIDetectionJob
import Amazonka.ComprehendMedical.StartRxNormInferenceJob
import Amazonka.ComprehendMedical.StartSNOMEDCTInferenceJob
import Amazonka.ComprehendMedical.StopEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.StopICD10CMInferenceJob
import Amazonka.ComprehendMedical.StopPHIDetectionJob
import Amazonka.ComprehendMedical.StopRxNormInferenceJob
import Amazonka.ComprehendMedical.StopSNOMEDCTInferenceJob
import Amazonka.ComprehendMedical.Types.Attribute
import Amazonka.ComprehendMedical.Types.Characters
import Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobFilter
import Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobProperties
import Amazonka.ComprehendMedical.Types.Entity
import Amazonka.ComprehendMedical.Types.ICD10CMAttribute
import Amazonka.ComprehendMedical.Types.ICD10CMConcept
import Amazonka.ComprehendMedical.Types.ICD10CMEntity
import Amazonka.ComprehendMedical.Types.ICD10CMTrait
import Amazonka.ComprehendMedical.Types.InputDataConfig
import Amazonka.ComprehendMedical.Types.OutputDataConfig
import Amazonka.ComprehendMedical.Types.RxNormAttribute
import Amazonka.ComprehendMedical.Types.RxNormConcept
import Amazonka.ComprehendMedical.Types.RxNormEntity
import Amazonka.ComprehendMedical.Types.RxNormTrait
import Amazonka.ComprehendMedical.Types.SNOMEDCTAttribute
import Amazonka.ComprehendMedical.Types.SNOMEDCTConcept
import Amazonka.ComprehendMedical.Types.SNOMEDCTDetails
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntity
import Amazonka.ComprehendMedical.Types.SNOMEDCTTrait
import Amazonka.ComprehendMedical.Types.Trait
import Amazonka.ComprehendMedical.Types.UnmappedAttribute
