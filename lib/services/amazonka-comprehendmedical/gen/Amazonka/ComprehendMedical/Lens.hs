{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComprehendMedical.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Lens
  ( -- * Operations

    -- ** DescribeEntitiesDetectionV2Job
    describeEntitiesDetectionV2Job_jobId,
    describeEntitiesDetectionV2JobResponse_comprehendMedicalAsyncJobProperties,
    describeEntitiesDetectionV2JobResponse_httpStatus,

    -- ** DescribePHIDetectionJob
    describePHIDetectionJob_jobId,
    describePHIDetectionJobResponse_comprehendMedicalAsyncJobProperties,
    describePHIDetectionJobResponse_httpStatus,

    -- ** ListICD10CMInferenceJobs
    listICD10CMInferenceJobs_nextToken,
    listICD10CMInferenceJobs_filter,
    listICD10CMInferenceJobs_maxResults,
    listICD10CMInferenceJobsResponse_nextToken,
    listICD10CMInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listICD10CMInferenceJobsResponse_httpStatus,

    -- ** InferICD10CM
    inferICD10CM_text,
    inferICD10CMResponse_paginationToken,
    inferICD10CMResponse_modelVersion,
    inferICD10CMResponse_httpStatus,
    inferICD10CMResponse_entities,

    -- ** InferRxNorm
    inferRxNorm_text,
    inferRxNormResponse_paginationToken,
    inferRxNormResponse_modelVersion,
    inferRxNormResponse_httpStatus,
    inferRxNormResponse_entities,

    -- ** StartICD10CMInferenceJob
    startICD10CMInferenceJob_kmsKey,
    startICD10CMInferenceJob_jobName,
    startICD10CMInferenceJob_clientRequestToken,
    startICD10CMInferenceJob_inputDataConfig,
    startICD10CMInferenceJob_outputDataConfig,
    startICD10CMInferenceJob_dataAccessRoleArn,
    startICD10CMInferenceJob_languageCode,
    startICD10CMInferenceJobResponse_jobId,
    startICD10CMInferenceJobResponse_httpStatus,

    -- ** StartRxNormInferenceJob
    startRxNormInferenceJob_kmsKey,
    startRxNormInferenceJob_jobName,
    startRxNormInferenceJob_clientRequestToken,
    startRxNormInferenceJob_inputDataConfig,
    startRxNormInferenceJob_outputDataConfig,
    startRxNormInferenceJob_dataAccessRoleArn,
    startRxNormInferenceJob_languageCode,
    startRxNormInferenceJobResponse_jobId,
    startRxNormInferenceJobResponse_httpStatus,

    -- ** ListPHIDetectionJobs
    listPHIDetectionJobs_nextToken,
    listPHIDetectionJobs_filter,
    listPHIDetectionJobs_maxResults,
    listPHIDetectionJobsResponse_nextToken,
    listPHIDetectionJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listPHIDetectionJobsResponse_httpStatus,

    -- ** DescribeICD10CMInferenceJob
    describeICD10CMInferenceJob_jobId,
    describeICD10CMInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeICD10CMInferenceJobResponse_httpStatus,

    -- ** StartPHIDetectionJob
    startPHIDetectionJob_kmsKey,
    startPHIDetectionJob_jobName,
    startPHIDetectionJob_clientRequestToken,
    startPHIDetectionJob_inputDataConfig,
    startPHIDetectionJob_outputDataConfig,
    startPHIDetectionJob_dataAccessRoleArn,
    startPHIDetectionJob_languageCode,
    startPHIDetectionJobResponse_jobId,
    startPHIDetectionJobResponse_httpStatus,

    -- ** StopEntitiesDetectionV2Job
    stopEntitiesDetectionV2Job_jobId,
    stopEntitiesDetectionV2JobResponse_jobId,
    stopEntitiesDetectionV2JobResponse_httpStatus,

    -- ** DescribeRxNormInferenceJob
    describeRxNormInferenceJob_jobId,
    describeRxNormInferenceJobResponse_comprehendMedicalAsyncJobProperties,
    describeRxNormInferenceJobResponse_httpStatus,

    -- ** StopICD10CMInferenceJob
    stopICD10CMInferenceJob_jobId,
    stopICD10CMInferenceJobResponse_jobId,
    stopICD10CMInferenceJobResponse_httpStatus,

    -- ** ListEntitiesDetectionV2Jobs
    listEntitiesDetectionV2Jobs_nextToken,
    listEntitiesDetectionV2Jobs_filter,
    listEntitiesDetectionV2Jobs_maxResults,
    listEntitiesDetectionV2JobsResponse_nextToken,
    listEntitiesDetectionV2JobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listEntitiesDetectionV2JobsResponse_httpStatus,

    -- ** StopRxNormInferenceJob
    stopRxNormInferenceJob_jobId,
    stopRxNormInferenceJobResponse_jobId,
    stopRxNormInferenceJobResponse_httpStatus,

    -- ** DetectPHI
    detectPHI_text,
    detectPHIResponse_paginationToken,
    detectPHIResponse_httpStatus,
    detectPHIResponse_entities,
    detectPHIResponse_modelVersion,

    -- ** DetectEntitiesV2
    detectEntitiesV2_text,
    detectEntitiesV2Response_paginationToken,
    detectEntitiesV2Response_unmappedAttributes,
    detectEntitiesV2Response_httpStatus,
    detectEntitiesV2Response_entities,
    detectEntitiesV2Response_modelVersion,

    -- ** StopPHIDetectionJob
    stopPHIDetectionJob_jobId,
    stopPHIDetectionJobResponse_jobId,
    stopPHIDetectionJobResponse_httpStatus,

    -- ** StartEntitiesDetectionV2Job
    startEntitiesDetectionV2Job_kmsKey,
    startEntitiesDetectionV2Job_jobName,
    startEntitiesDetectionV2Job_clientRequestToken,
    startEntitiesDetectionV2Job_inputDataConfig,
    startEntitiesDetectionV2Job_outputDataConfig,
    startEntitiesDetectionV2Job_dataAccessRoleArn,
    startEntitiesDetectionV2Job_languageCode,
    startEntitiesDetectionV2JobResponse_jobId,
    startEntitiesDetectionV2JobResponse_httpStatus,

    -- ** ListRxNormInferenceJobs
    listRxNormInferenceJobs_nextToken,
    listRxNormInferenceJobs_filter,
    listRxNormInferenceJobs_maxResults,
    listRxNormInferenceJobsResponse_nextToken,
    listRxNormInferenceJobsResponse_comprehendMedicalAsyncJobPropertiesList,
    listRxNormInferenceJobsResponse_httpStatus,

    -- * Types

    -- ** Attribute
    attribute_relationshipScore,
    attribute_beginOffset,
    attribute_text,
    attribute_category,
    attribute_score,
    attribute_traits,
    attribute_relationshipType,
    attribute_endOffset,
    attribute_id,
    attribute_type,

    -- ** ComprehendMedicalAsyncJobFilter
    comprehendMedicalAsyncJobFilter_submitTimeAfter,
    comprehendMedicalAsyncJobFilter_submitTimeBefore,
    comprehendMedicalAsyncJobFilter_jobName,
    comprehendMedicalAsyncJobFilter_jobStatus,

    -- ** ComprehendMedicalAsyncJobProperties
    comprehendMedicalAsyncJobProperties_languageCode,
    comprehendMedicalAsyncJobProperties_kmsKey,
    comprehendMedicalAsyncJobProperties_jobId,
    comprehendMedicalAsyncJobProperties_jobName,
    comprehendMedicalAsyncJobProperties_manifestFilePath,
    comprehendMedicalAsyncJobProperties_inputDataConfig,
    comprehendMedicalAsyncJobProperties_endTime,
    comprehendMedicalAsyncJobProperties_outputDataConfig,
    comprehendMedicalAsyncJobProperties_modelVersion,
    comprehendMedicalAsyncJobProperties_dataAccessRoleArn,
    comprehendMedicalAsyncJobProperties_jobStatus,
    comprehendMedicalAsyncJobProperties_message,
    comprehendMedicalAsyncJobProperties_expirationTime,
    comprehendMedicalAsyncJobProperties_submitTime,

    -- ** Entity
    entity_beginOffset,
    entity_text,
    entity_category,
    entity_score,
    entity_traits,
    entity_attributes,
    entity_endOffset,
    entity_id,
    entity_type,

    -- ** ICD10CMAttribute
    iCD10CMAttribute_relationshipScore,
    iCD10CMAttribute_beginOffset,
    iCD10CMAttribute_text,
    iCD10CMAttribute_category,
    iCD10CMAttribute_score,
    iCD10CMAttribute_traits,
    iCD10CMAttribute_relationshipType,
    iCD10CMAttribute_endOffset,
    iCD10CMAttribute_id,
    iCD10CMAttribute_type,

    -- ** ICD10CMConcept
    iCD10CMConcept_score,
    iCD10CMConcept_code,
    iCD10CMConcept_description,

    -- ** ICD10CMEntity
    iCD10CMEntity_iCD10CMConcepts,
    iCD10CMEntity_beginOffset,
    iCD10CMEntity_text,
    iCD10CMEntity_category,
    iCD10CMEntity_score,
    iCD10CMEntity_traits,
    iCD10CMEntity_attributes,
    iCD10CMEntity_endOffset,
    iCD10CMEntity_id,
    iCD10CMEntity_type,

    -- ** ICD10CMTrait
    iCD10CMTrait_score,
    iCD10CMTrait_name,

    -- ** InputDataConfig
    inputDataConfig_s3Key,
    inputDataConfig_s3Bucket,

    -- ** OutputDataConfig
    outputDataConfig_s3Key,
    outputDataConfig_s3Bucket,

    -- ** RxNormAttribute
    rxNormAttribute_relationshipScore,
    rxNormAttribute_beginOffset,
    rxNormAttribute_text,
    rxNormAttribute_score,
    rxNormAttribute_traits,
    rxNormAttribute_endOffset,
    rxNormAttribute_id,
    rxNormAttribute_type,

    -- ** RxNormConcept
    rxNormConcept_score,
    rxNormConcept_code,
    rxNormConcept_description,

    -- ** RxNormEntity
    rxNormEntity_rxNormConcepts,
    rxNormEntity_beginOffset,
    rxNormEntity_text,
    rxNormEntity_category,
    rxNormEntity_score,
    rxNormEntity_traits,
    rxNormEntity_attributes,
    rxNormEntity_endOffset,
    rxNormEntity_id,
    rxNormEntity_type,

    -- ** RxNormTrait
    rxNormTrait_score,
    rxNormTrait_name,

    -- ** Trait
    trait_score,
    trait_name,

    -- ** UnmappedAttribute
    unmappedAttribute_attribute,
    unmappedAttribute_type,
  )
where

import Amazonka.ComprehendMedical.DescribeEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.DescribeICD10CMInferenceJob
import Amazonka.ComprehendMedical.DescribePHIDetectionJob
import Amazonka.ComprehendMedical.DescribeRxNormInferenceJob
import Amazonka.ComprehendMedical.DetectEntitiesV2
import Amazonka.ComprehendMedical.DetectPHI
import Amazonka.ComprehendMedical.InferICD10CM
import Amazonka.ComprehendMedical.InferRxNorm
import Amazonka.ComprehendMedical.ListEntitiesDetectionV2Jobs
import Amazonka.ComprehendMedical.ListICD10CMInferenceJobs
import Amazonka.ComprehendMedical.ListPHIDetectionJobs
import Amazonka.ComprehendMedical.ListRxNormInferenceJobs
import Amazonka.ComprehendMedical.StartEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.StartICD10CMInferenceJob
import Amazonka.ComprehendMedical.StartPHIDetectionJob
import Amazonka.ComprehendMedical.StartRxNormInferenceJob
import Amazonka.ComprehendMedical.StopEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.StopICD10CMInferenceJob
import Amazonka.ComprehendMedical.StopPHIDetectionJob
import Amazonka.ComprehendMedical.StopRxNormInferenceJob
import Amazonka.ComprehendMedical.Types.Attribute
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
import Amazonka.ComprehendMedical.Types.Trait
import Amazonka.ComprehendMedical.Types.UnmappedAttribute
