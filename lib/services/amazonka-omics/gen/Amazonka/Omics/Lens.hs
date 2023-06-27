{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Omics.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Lens
  ( -- * Operations

    -- ** AbortMultipartReadSetUpload
    abortMultipartReadSetUpload_sequenceStoreId,
    abortMultipartReadSetUpload_uploadId,
    abortMultipartReadSetUploadResponse_httpStatus,

    -- ** BatchDeleteReadSet
    batchDeleteReadSet_ids,
    batchDeleteReadSet_sequenceStoreId,
    batchDeleteReadSetResponse_errors,
    batchDeleteReadSetResponse_httpStatus,

    -- ** CancelAnnotationImportJob
    cancelAnnotationImportJob_jobId,
    cancelAnnotationImportJobResponse_httpStatus,

    -- ** CancelRun
    cancelRun_id,

    -- ** CancelVariantImportJob
    cancelVariantImportJob_jobId,
    cancelVariantImportJobResponse_httpStatus,

    -- ** CompleteMultipartReadSetUpload
    completeMultipartReadSetUpload_sequenceStoreId,
    completeMultipartReadSetUpload_uploadId,
    completeMultipartReadSetUpload_parts,
    completeMultipartReadSetUploadResponse_httpStatus,
    completeMultipartReadSetUploadResponse_readSetId,

    -- ** CreateAnnotationStore
    createAnnotationStore_description,
    createAnnotationStore_name,
    createAnnotationStore_reference,
    createAnnotationStore_sseConfig,
    createAnnotationStore_storeOptions,
    createAnnotationStore_tags,
    createAnnotationStore_storeFormat,
    createAnnotationStoreResponse_reference,
    createAnnotationStoreResponse_storeFormat,
    createAnnotationStoreResponse_storeOptions,
    createAnnotationStoreResponse_httpStatus,
    createAnnotationStoreResponse_id,
    createAnnotationStoreResponse_status,
    createAnnotationStoreResponse_name,
    createAnnotationStoreResponse_creationTime,

    -- ** CreateMultipartReadSetUpload
    createMultipartReadSetUpload_clientToken,
    createMultipartReadSetUpload_description,
    createMultipartReadSetUpload_generatedFrom,
    createMultipartReadSetUpload_tags,
    createMultipartReadSetUpload_sequenceStoreId,
    createMultipartReadSetUpload_sourceFileType,
    createMultipartReadSetUpload_subjectId,
    createMultipartReadSetUpload_sampleId,
    createMultipartReadSetUpload_referenceArn,
    createMultipartReadSetUpload_name,
    createMultipartReadSetUploadResponse_description,
    createMultipartReadSetUploadResponse_generatedFrom,
    createMultipartReadSetUploadResponse_name,
    createMultipartReadSetUploadResponse_tags,
    createMultipartReadSetUploadResponse_httpStatus,
    createMultipartReadSetUploadResponse_sequenceStoreId,
    createMultipartReadSetUploadResponse_uploadId,
    createMultipartReadSetUploadResponse_sourceFileType,
    createMultipartReadSetUploadResponse_subjectId,
    createMultipartReadSetUploadResponse_sampleId,
    createMultipartReadSetUploadResponse_referenceArn,
    createMultipartReadSetUploadResponse_creationTime,

    -- ** CreateReferenceStore
    createReferenceStore_clientToken,
    createReferenceStore_description,
    createReferenceStore_sseConfig,
    createReferenceStore_tags,
    createReferenceStore_name,
    createReferenceStoreResponse_description,
    createReferenceStoreResponse_name,
    createReferenceStoreResponse_sseConfig,
    createReferenceStoreResponse_httpStatus,
    createReferenceStoreResponse_id,
    createReferenceStoreResponse_arn,
    createReferenceStoreResponse_creationTime,

    -- ** CreateRunGroup
    createRunGroup_maxCpus,
    createRunGroup_maxDuration,
    createRunGroup_maxGpus,
    createRunGroup_maxRuns,
    createRunGroup_name,
    createRunGroup_tags,
    createRunGroup_requestId,
    createRunGroupResponse_arn,
    createRunGroupResponse_id,
    createRunGroupResponse_tags,
    createRunGroupResponse_httpStatus,

    -- ** CreateSequenceStore
    createSequenceStore_clientToken,
    createSequenceStore_description,
    createSequenceStore_fallbackLocation,
    createSequenceStore_sseConfig,
    createSequenceStore_tags,
    createSequenceStore_name,
    createSequenceStoreResponse_description,
    createSequenceStoreResponse_fallbackLocation,
    createSequenceStoreResponse_name,
    createSequenceStoreResponse_sseConfig,
    createSequenceStoreResponse_httpStatus,
    createSequenceStoreResponse_id,
    createSequenceStoreResponse_arn,
    createSequenceStoreResponse_creationTime,

    -- ** CreateVariantStore
    createVariantStore_description,
    createVariantStore_name,
    createVariantStore_sseConfig,
    createVariantStore_tags,
    createVariantStore_reference,
    createVariantStoreResponse_reference,
    createVariantStoreResponse_httpStatus,
    createVariantStoreResponse_id,
    createVariantStoreResponse_status,
    createVariantStoreResponse_name,
    createVariantStoreResponse_creationTime,

    -- ** CreateWorkflow
    createWorkflow_accelerators,
    createWorkflow_definitionUri,
    createWorkflow_definitionZip,
    createWorkflow_description,
    createWorkflow_engine,
    createWorkflow_main,
    createWorkflow_name,
    createWorkflow_parameterTemplate,
    createWorkflow_storageCapacity,
    createWorkflow_tags,
    createWorkflow_requestId,
    createWorkflowResponse_arn,
    createWorkflowResponse_id,
    createWorkflowResponse_status,
    createWorkflowResponse_tags,
    createWorkflowResponse_httpStatus,

    -- ** DeleteAnnotationStore
    deleteAnnotationStore_force,
    deleteAnnotationStore_name,
    deleteAnnotationStoreResponse_httpStatus,
    deleteAnnotationStoreResponse_status,

    -- ** DeleteReference
    deleteReference_id,
    deleteReference_referenceStoreId,
    deleteReferenceResponse_httpStatus,

    -- ** DeleteReferenceStore
    deleteReferenceStore_id,
    deleteReferenceStoreResponse_httpStatus,

    -- ** DeleteRun
    deleteRun_id,

    -- ** DeleteRunGroup
    deleteRunGroup_id,

    -- ** DeleteSequenceStore
    deleteSequenceStore_id,
    deleteSequenceStoreResponse_httpStatus,

    -- ** DeleteVariantStore
    deleteVariantStore_force,
    deleteVariantStore_name,
    deleteVariantStoreResponse_httpStatus,
    deleteVariantStoreResponse_status,

    -- ** DeleteWorkflow
    deleteWorkflow_id,

    -- ** GetAnnotationImportJob
    getAnnotationImportJob_jobId,
    getAnnotationImportJobResponse_annotationFields,
    getAnnotationImportJobResponse_httpStatus,
    getAnnotationImportJobResponse_id,
    getAnnotationImportJobResponse_destinationName,
    getAnnotationImportJobResponse_roleArn,
    getAnnotationImportJobResponse_status,
    getAnnotationImportJobResponse_statusMessage,
    getAnnotationImportJobResponse_creationTime,
    getAnnotationImportJobResponse_updateTime,
    getAnnotationImportJobResponse_completionTime,
    getAnnotationImportJobResponse_items,
    getAnnotationImportJobResponse_runLeftNormalization,
    getAnnotationImportJobResponse_formatOptions,

    -- ** GetAnnotationStore
    getAnnotationStore_name,
    getAnnotationStoreResponse_storeFormat,
    getAnnotationStoreResponse_storeOptions,
    getAnnotationStoreResponse_httpStatus,
    getAnnotationStoreResponse_id,
    getAnnotationStoreResponse_reference,
    getAnnotationStoreResponse_status,
    getAnnotationStoreResponse_storeArn,
    getAnnotationStoreResponse_name,
    getAnnotationStoreResponse_description,
    getAnnotationStoreResponse_sseConfig,
    getAnnotationStoreResponse_creationTime,
    getAnnotationStoreResponse_updateTime,
    getAnnotationStoreResponse_tags,
    getAnnotationStoreResponse_statusMessage,
    getAnnotationStoreResponse_storeSizeBytes,

    -- ** GetReadSet
    getReadSet_file,
    getReadSet_id,
    getReadSet_sequenceStoreId,
    getReadSet_partNumber,
    getReadSetResponse_httpStatus,
    getReadSetResponse_payload,

    -- ** GetReadSetActivationJob
    getReadSetActivationJob_id,
    getReadSetActivationJob_sequenceStoreId,
    getReadSetActivationJobResponse_completionTime,
    getReadSetActivationJobResponse_sources,
    getReadSetActivationJobResponse_statusMessage,
    getReadSetActivationJobResponse_httpStatus,
    getReadSetActivationJobResponse_id,
    getReadSetActivationJobResponse_sequenceStoreId,
    getReadSetActivationJobResponse_status,
    getReadSetActivationJobResponse_creationTime,

    -- ** GetReadSetExportJob
    getReadSetExportJob_sequenceStoreId,
    getReadSetExportJob_id,
    getReadSetExportJobResponse_completionTime,
    getReadSetExportJobResponse_readSets,
    getReadSetExportJobResponse_statusMessage,
    getReadSetExportJobResponse_httpStatus,
    getReadSetExportJobResponse_id,
    getReadSetExportJobResponse_sequenceStoreId,
    getReadSetExportJobResponse_destination,
    getReadSetExportJobResponse_status,
    getReadSetExportJobResponse_creationTime,

    -- ** GetReadSetImportJob
    getReadSetImportJob_id,
    getReadSetImportJob_sequenceStoreId,
    getReadSetImportJobResponse_completionTime,
    getReadSetImportJobResponse_statusMessage,
    getReadSetImportJobResponse_httpStatus,
    getReadSetImportJobResponse_id,
    getReadSetImportJobResponse_sequenceStoreId,
    getReadSetImportJobResponse_roleArn,
    getReadSetImportJobResponse_status,
    getReadSetImportJobResponse_creationTime,
    getReadSetImportJobResponse_sources,

    -- ** GetReadSetMetadata
    getReadSetMetadata_id,
    getReadSetMetadata_sequenceStoreId,
    getReadSetMetadataResponse_description,
    getReadSetMetadataResponse_files,
    getReadSetMetadataResponse_name,
    getReadSetMetadataResponse_referenceArn,
    getReadSetMetadataResponse_sampleId,
    getReadSetMetadataResponse_sequenceInformation,
    getReadSetMetadataResponse_statusMessage,
    getReadSetMetadataResponse_subjectId,
    getReadSetMetadataResponse_httpStatus,
    getReadSetMetadataResponse_id,
    getReadSetMetadataResponse_arn,
    getReadSetMetadataResponse_sequenceStoreId,
    getReadSetMetadataResponse_status,
    getReadSetMetadataResponse_fileType,
    getReadSetMetadataResponse_creationTime,

    -- ** GetReference
    getReference_file,
    getReference_range,
    getReference_id,
    getReference_referenceStoreId,
    getReference_partNumber,
    getReferenceResponse_httpStatus,
    getReferenceResponse_payload,

    -- ** GetReferenceImportJob
    getReferenceImportJob_id,
    getReferenceImportJob_referenceStoreId,
    getReferenceImportJobResponse_completionTime,
    getReferenceImportJobResponse_statusMessage,
    getReferenceImportJobResponse_httpStatus,
    getReferenceImportJobResponse_id,
    getReferenceImportJobResponse_referenceStoreId,
    getReferenceImportJobResponse_roleArn,
    getReferenceImportJobResponse_status,
    getReferenceImportJobResponse_creationTime,
    getReferenceImportJobResponse_sources,

    -- ** GetReferenceMetadata
    getReferenceMetadata_id,
    getReferenceMetadata_referenceStoreId,
    getReferenceMetadataResponse_description,
    getReferenceMetadataResponse_files,
    getReferenceMetadataResponse_name,
    getReferenceMetadataResponse_status,
    getReferenceMetadataResponse_httpStatus,
    getReferenceMetadataResponse_id,
    getReferenceMetadataResponse_arn,
    getReferenceMetadataResponse_referenceStoreId,
    getReferenceMetadataResponse_md5,
    getReferenceMetadataResponse_creationTime,
    getReferenceMetadataResponse_updateTime,

    -- ** GetReferenceStore
    getReferenceStore_id,
    getReferenceStoreResponse_description,
    getReferenceStoreResponse_name,
    getReferenceStoreResponse_sseConfig,
    getReferenceStoreResponse_httpStatus,
    getReferenceStoreResponse_id,
    getReferenceStoreResponse_arn,
    getReferenceStoreResponse_creationTime,

    -- ** GetRun
    getRun_export,
    getRun_id,
    getRunResponse_accelerators,
    getRunResponse_arn,
    getRunResponse_creationTime,
    getRunResponse_definition,
    getRunResponse_digest,
    getRunResponse_id,
    getRunResponse_logLevel,
    getRunResponse_name,
    getRunResponse_outputUri,
    getRunResponse_parameters,
    getRunResponse_priority,
    getRunResponse_resourceDigests,
    getRunResponse_roleArn,
    getRunResponse_runGroupId,
    getRunResponse_runId,
    getRunResponse_startTime,
    getRunResponse_startedBy,
    getRunResponse_status,
    getRunResponse_statusMessage,
    getRunResponse_stopTime,
    getRunResponse_storageCapacity,
    getRunResponse_tags,
    getRunResponse_workflowId,
    getRunResponse_workflowType,
    getRunResponse_httpStatus,

    -- ** GetRunGroup
    getRunGroup_id,
    getRunGroupResponse_arn,
    getRunGroupResponse_creationTime,
    getRunGroupResponse_id,
    getRunGroupResponse_maxCpus,
    getRunGroupResponse_maxDuration,
    getRunGroupResponse_maxGpus,
    getRunGroupResponse_maxRuns,
    getRunGroupResponse_name,
    getRunGroupResponse_tags,
    getRunGroupResponse_httpStatus,

    -- ** GetRunTask
    getRunTask_id,
    getRunTask_taskId,
    getRunTaskResponse_cpus,
    getRunTaskResponse_creationTime,
    getRunTaskResponse_gpus,
    getRunTaskResponse_logStream,
    getRunTaskResponse_memory,
    getRunTaskResponse_name,
    getRunTaskResponse_startTime,
    getRunTaskResponse_status,
    getRunTaskResponse_statusMessage,
    getRunTaskResponse_stopTime,
    getRunTaskResponse_taskId,
    getRunTaskResponse_httpStatus,

    -- ** GetSequenceStore
    getSequenceStore_id,
    getSequenceStoreResponse_description,
    getSequenceStoreResponse_fallbackLocation,
    getSequenceStoreResponse_name,
    getSequenceStoreResponse_sseConfig,
    getSequenceStoreResponse_httpStatus,
    getSequenceStoreResponse_id,
    getSequenceStoreResponse_arn,
    getSequenceStoreResponse_creationTime,

    -- ** GetVariantImportJob
    getVariantImportJob_jobId,
    getVariantImportJobResponse_annotationFields,
    getVariantImportJobResponse_completionTime,
    getVariantImportJobResponse_httpStatus,
    getVariantImportJobResponse_id,
    getVariantImportJobResponse_destinationName,
    getVariantImportJobResponse_roleArn,
    getVariantImportJobResponse_status,
    getVariantImportJobResponse_statusMessage,
    getVariantImportJobResponse_creationTime,
    getVariantImportJobResponse_updateTime,
    getVariantImportJobResponse_items,
    getVariantImportJobResponse_runLeftNormalization,

    -- ** GetVariantStore
    getVariantStore_name,
    getVariantStoreResponse_httpStatus,
    getVariantStoreResponse_id,
    getVariantStoreResponse_reference,
    getVariantStoreResponse_status,
    getVariantStoreResponse_storeArn,
    getVariantStoreResponse_name,
    getVariantStoreResponse_description,
    getVariantStoreResponse_sseConfig,
    getVariantStoreResponse_creationTime,
    getVariantStoreResponse_updateTime,
    getVariantStoreResponse_tags,
    getVariantStoreResponse_statusMessage,
    getVariantStoreResponse_storeSizeBytes,

    -- ** GetWorkflow
    getWorkflow_export,
    getWorkflow_type,
    getWorkflow_id,
    getWorkflowResponse_accelerators,
    getWorkflowResponse_arn,
    getWorkflowResponse_creationTime,
    getWorkflowResponse_definition,
    getWorkflowResponse_description,
    getWorkflowResponse_digest,
    getWorkflowResponse_engine,
    getWorkflowResponse_id,
    getWorkflowResponse_main,
    getWorkflowResponse_metadata,
    getWorkflowResponse_name,
    getWorkflowResponse_parameterTemplate,
    getWorkflowResponse_status,
    getWorkflowResponse_statusMessage,
    getWorkflowResponse_storageCapacity,
    getWorkflowResponse_tags,
    getWorkflowResponse_type,
    getWorkflowResponse_httpStatus,

    -- ** ListAnnotationImportJobs
    listAnnotationImportJobs_filter,
    listAnnotationImportJobs_ids,
    listAnnotationImportJobs_maxResults,
    listAnnotationImportJobs_nextToken,
    listAnnotationImportJobsResponse_annotationImportJobs,
    listAnnotationImportJobsResponse_nextToken,
    listAnnotationImportJobsResponse_httpStatus,

    -- ** ListAnnotationStores
    listAnnotationStores_filter,
    listAnnotationStores_ids,
    listAnnotationStores_maxResults,
    listAnnotationStores_nextToken,
    listAnnotationStoresResponse_annotationStores,
    listAnnotationStoresResponse_nextToken,
    listAnnotationStoresResponse_httpStatus,

    -- ** ListMultipartReadSetUploads
    listMultipartReadSetUploads_maxResults,
    listMultipartReadSetUploads_nextToken,
    listMultipartReadSetUploads_sequenceStoreId,
    listMultipartReadSetUploadsResponse_nextToken,
    listMultipartReadSetUploadsResponse_uploads,
    listMultipartReadSetUploadsResponse_httpStatus,

    -- ** ListReadSetActivationJobs
    listReadSetActivationJobs_filter,
    listReadSetActivationJobs_maxResults,
    listReadSetActivationJobs_nextToken,
    listReadSetActivationJobs_sequenceStoreId,
    listReadSetActivationJobsResponse_activationJobs,
    listReadSetActivationJobsResponse_nextToken,
    listReadSetActivationJobsResponse_httpStatus,

    -- ** ListReadSetExportJobs
    listReadSetExportJobs_filter,
    listReadSetExportJobs_maxResults,
    listReadSetExportJobs_nextToken,
    listReadSetExportJobs_sequenceStoreId,
    listReadSetExportJobsResponse_exportJobs,
    listReadSetExportJobsResponse_nextToken,
    listReadSetExportJobsResponse_httpStatus,

    -- ** ListReadSetImportJobs
    listReadSetImportJobs_filter,
    listReadSetImportJobs_maxResults,
    listReadSetImportJobs_nextToken,
    listReadSetImportJobs_sequenceStoreId,
    listReadSetImportJobsResponse_importJobs,
    listReadSetImportJobsResponse_nextToken,
    listReadSetImportJobsResponse_httpStatus,

    -- ** ListReadSetUploadParts
    listReadSetUploadParts_filter,
    listReadSetUploadParts_maxResults,
    listReadSetUploadParts_nextToken,
    listReadSetUploadParts_sequenceStoreId,
    listReadSetUploadParts_uploadId,
    listReadSetUploadParts_partSource,
    listReadSetUploadPartsResponse_nextToken,
    listReadSetUploadPartsResponse_parts,
    listReadSetUploadPartsResponse_httpStatus,

    -- ** ListReadSets
    listReadSets_filter,
    listReadSets_maxResults,
    listReadSets_nextToken,
    listReadSets_sequenceStoreId,
    listReadSetsResponse_nextToken,
    listReadSetsResponse_httpStatus,
    listReadSetsResponse_readSets,

    -- ** ListReferenceImportJobs
    listReferenceImportJobs_filter,
    listReferenceImportJobs_maxResults,
    listReferenceImportJobs_nextToken,
    listReferenceImportJobs_referenceStoreId,
    listReferenceImportJobsResponse_importJobs,
    listReferenceImportJobsResponse_nextToken,
    listReferenceImportJobsResponse_httpStatus,

    -- ** ListReferenceStores
    listReferenceStores_filter,
    listReferenceStores_maxResults,
    listReferenceStores_nextToken,
    listReferenceStoresResponse_nextToken,
    listReferenceStoresResponse_httpStatus,
    listReferenceStoresResponse_referenceStores,

    -- ** ListReferences
    listReferences_filter,
    listReferences_maxResults,
    listReferences_nextToken,
    listReferences_referenceStoreId,
    listReferencesResponse_nextToken,
    listReferencesResponse_httpStatus,
    listReferencesResponse_references,

    -- ** ListRunGroups
    listRunGroups_maxResults,
    listRunGroups_name,
    listRunGroups_startingToken,
    listRunGroupsResponse_items,
    listRunGroupsResponse_nextToken,
    listRunGroupsResponse_httpStatus,

    -- ** ListRunTasks
    listRunTasks_maxResults,
    listRunTasks_startingToken,
    listRunTasks_status,
    listRunTasks_id,
    listRunTasksResponse_items,
    listRunTasksResponse_nextToken,
    listRunTasksResponse_httpStatus,

    -- ** ListRuns
    listRuns_maxResults,
    listRuns_name,
    listRuns_runGroupId,
    listRuns_startingToken,
    listRuns_status,
    listRunsResponse_items,
    listRunsResponse_nextToken,
    listRunsResponse_httpStatus,

    -- ** ListSequenceStores
    listSequenceStores_filter,
    listSequenceStores_maxResults,
    listSequenceStores_nextToken,
    listSequenceStoresResponse_nextToken,
    listSequenceStoresResponse_httpStatus,
    listSequenceStoresResponse_sequenceStores,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** ListVariantImportJobs
    listVariantImportJobs_filter,
    listVariantImportJobs_ids,
    listVariantImportJobs_maxResults,
    listVariantImportJobs_nextToken,
    listVariantImportJobsResponse_nextToken,
    listVariantImportJobsResponse_variantImportJobs,
    listVariantImportJobsResponse_httpStatus,

    -- ** ListVariantStores
    listVariantStores_filter,
    listVariantStores_ids,
    listVariantStores_maxResults,
    listVariantStores_nextToken,
    listVariantStoresResponse_nextToken,
    listVariantStoresResponse_variantStores,
    listVariantStoresResponse_httpStatus,

    -- ** ListWorkflows
    listWorkflows_maxResults,
    listWorkflows_name,
    listWorkflows_startingToken,
    listWorkflows_type,
    listWorkflowsResponse_items,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,

    -- ** StartAnnotationImportJob
    startAnnotationImportJob_annotationFields,
    startAnnotationImportJob_formatOptions,
    startAnnotationImportJob_runLeftNormalization,
    startAnnotationImportJob_destinationName,
    startAnnotationImportJob_roleArn,
    startAnnotationImportJob_items,
    startAnnotationImportJobResponse_httpStatus,
    startAnnotationImportJobResponse_jobId,

    -- ** StartReadSetActivationJob
    startReadSetActivationJob_clientToken,
    startReadSetActivationJob_sequenceStoreId,
    startReadSetActivationJob_sources,
    startReadSetActivationJobResponse_httpStatus,
    startReadSetActivationJobResponse_id,
    startReadSetActivationJobResponse_sequenceStoreId,
    startReadSetActivationJobResponse_status,
    startReadSetActivationJobResponse_creationTime,

    -- ** StartReadSetExportJob
    startReadSetExportJob_clientToken,
    startReadSetExportJob_sequenceStoreId,
    startReadSetExportJob_destination,
    startReadSetExportJob_roleArn,
    startReadSetExportJob_sources,
    startReadSetExportJobResponse_httpStatus,
    startReadSetExportJobResponse_id,
    startReadSetExportJobResponse_sequenceStoreId,
    startReadSetExportJobResponse_destination,
    startReadSetExportJobResponse_status,
    startReadSetExportJobResponse_creationTime,

    -- ** StartReadSetImportJob
    startReadSetImportJob_clientToken,
    startReadSetImportJob_sequenceStoreId,
    startReadSetImportJob_roleArn,
    startReadSetImportJob_sources,
    startReadSetImportJobResponse_httpStatus,
    startReadSetImportJobResponse_id,
    startReadSetImportJobResponse_sequenceStoreId,
    startReadSetImportJobResponse_roleArn,
    startReadSetImportJobResponse_status,
    startReadSetImportJobResponse_creationTime,

    -- ** StartReferenceImportJob
    startReferenceImportJob_clientToken,
    startReferenceImportJob_referenceStoreId,
    startReferenceImportJob_roleArn,
    startReferenceImportJob_sources,
    startReferenceImportJobResponse_httpStatus,
    startReferenceImportJobResponse_id,
    startReferenceImportJobResponse_referenceStoreId,
    startReferenceImportJobResponse_roleArn,
    startReferenceImportJobResponse_status,
    startReferenceImportJobResponse_creationTime,

    -- ** StartRun
    startRun_logLevel,
    startRun_name,
    startRun_outputUri,
    startRun_parameters,
    startRun_priority,
    startRun_runGroupId,
    startRun_runId,
    startRun_storageCapacity,
    startRun_tags,
    startRun_workflowId,
    startRun_workflowType,
    startRun_roleArn,
    startRun_requestId,
    startRunResponse_arn,
    startRunResponse_id,
    startRunResponse_status,
    startRunResponse_tags,
    startRunResponse_httpStatus,

    -- ** StartVariantImportJob
    startVariantImportJob_annotationFields,
    startVariantImportJob_runLeftNormalization,
    startVariantImportJob_destinationName,
    startVariantImportJob_roleArn,
    startVariantImportJob_items,
    startVariantImportJobResponse_httpStatus,
    startVariantImportJobResponse_jobId,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAnnotationStore
    updateAnnotationStore_description,
    updateAnnotationStore_name,
    updateAnnotationStoreResponse_storeFormat,
    updateAnnotationStoreResponse_storeOptions,
    updateAnnotationStoreResponse_httpStatus,
    updateAnnotationStoreResponse_id,
    updateAnnotationStoreResponse_reference,
    updateAnnotationStoreResponse_status,
    updateAnnotationStoreResponse_name,
    updateAnnotationStoreResponse_description,
    updateAnnotationStoreResponse_creationTime,
    updateAnnotationStoreResponse_updateTime,

    -- ** UpdateRunGroup
    updateRunGroup_maxCpus,
    updateRunGroup_maxDuration,
    updateRunGroup_maxGpus,
    updateRunGroup_maxRuns,
    updateRunGroup_name,
    updateRunGroup_id,

    -- ** UpdateVariantStore
    updateVariantStore_description,
    updateVariantStore_name,
    updateVariantStoreResponse_httpStatus,
    updateVariantStoreResponse_id,
    updateVariantStoreResponse_reference,
    updateVariantStoreResponse_status,
    updateVariantStoreResponse_name,
    updateVariantStoreResponse_description,
    updateVariantStoreResponse_creationTime,
    updateVariantStoreResponse_updateTime,

    -- ** UpdateWorkflow
    updateWorkflow_description,
    updateWorkflow_name,
    updateWorkflow_id,

    -- ** UploadReadSetPart
    uploadReadSetPart_sequenceStoreId,
    uploadReadSetPart_uploadId,
    uploadReadSetPart_partSource,
    uploadReadSetPart_partNumber,
    uploadReadSetPart_payload,
    uploadReadSetPartResponse_httpStatus,
    uploadReadSetPartResponse_checksum,

    -- * Types

    -- ** ActivateReadSetFilter
    activateReadSetFilter_createdAfter,
    activateReadSetFilter_createdBefore,
    activateReadSetFilter_status,

    -- ** ActivateReadSetJobItem
    activateReadSetJobItem_completionTime,
    activateReadSetJobItem_id,
    activateReadSetJobItem_sequenceStoreId,
    activateReadSetJobItem_status,
    activateReadSetJobItem_creationTime,

    -- ** ActivateReadSetSourceItem
    activateReadSetSourceItem_statusMessage,
    activateReadSetSourceItem_readSetId,
    activateReadSetSourceItem_status,

    -- ** AnnotationImportItemDetail
    annotationImportItemDetail_source,
    annotationImportItemDetail_jobStatus,

    -- ** AnnotationImportItemSource
    annotationImportItemSource_source,

    -- ** AnnotationImportJobItem
    annotationImportJobItem_annotationFields,
    annotationImportJobItem_completionTime,
    annotationImportJobItem_runLeftNormalization,
    annotationImportJobItem_id,
    annotationImportJobItem_destinationName,
    annotationImportJobItem_roleArn,
    annotationImportJobItem_status,
    annotationImportJobItem_creationTime,
    annotationImportJobItem_updateTime,

    -- ** AnnotationStoreItem
    annotationStoreItem_id,
    annotationStoreItem_reference,
    annotationStoreItem_status,
    annotationStoreItem_storeArn,
    annotationStoreItem_name,
    annotationStoreItem_storeFormat,
    annotationStoreItem_description,
    annotationStoreItem_sseConfig,
    annotationStoreItem_creationTime,
    annotationStoreItem_updateTime,
    annotationStoreItem_statusMessage,
    annotationStoreItem_storeSizeBytes,

    -- ** CompleteReadSetUploadPartListItem
    completeReadSetUploadPartListItem_partNumber,
    completeReadSetUploadPartListItem_partSource,
    completeReadSetUploadPartListItem_checksum,

    -- ** ExportReadSet
    exportReadSet_readSetId,

    -- ** ExportReadSetDetail
    exportReadSetDetail_statusMessage,
    exportReadSetDetail_id,
    exportReadSetDetail_status,

    -- ** ExportReadSetFilter
    exportReadSetFilter_createdAfter,
    exportReadSetFilter_createdBefore,
    exportReadSetFilter_status,

    -- ** ExportReadSetJobDetail
    exportReadSetJobDetail_completionTime,
    exportReadSetJobDetail_id,
    exportReadSetJobDetail_sequenceStoreId,
    exportReadSetJobDetail_destination,
    exportReadSetJobDetail_status,
    exportReadSetJobDetail_creationTime,

    -- ** FileInformation
    fileInformation_contentLength,
    fileInformation_partSize,
    fileInformation_totalParts,

    -- ** FormatOptions
    formatOptions_tsvOptions,
    formatOptions_vcfOptions,

    -- ** ImportReadSetFilter
    importReadSetFilter_createdAfter,
    importReadSetFilter_createdBefore,
    importReadSetFilter_status,

    -- ** ImportReadSetJobItem
    importReadSetJobItem_completionTime,
    importReadSetJobItem_id,
    importReadSetJobItem_sequenceStoreId,
    importReadSetJobItem_roleArn,
    importReadSetJobItem_status,
    importReadSetJobItem_creationTime,

    -- ** ImportReadSetSourceItem
    importReadSetSourceItem_description,
    importReadSetSourceItem_generatedFrom,
    importReadSetSourceItem_name,
    importReadSetSourceItem_referenceArn,
    importReadSetSourceItem_statusMessage,
    importReadSetSourceItem_tags,
    importReadSetSourceItem_sourceFiles,
    importReadSetSourceItem_sourceFileType,
    importReadSetSourceItem_status,
    importReadSetSourceItem_subjectId,
    importReadSetSourceItem_sampleId,

    -- ** ImportReferenceFilter
    importReferenceFilter_createdAfter,
    importReferenceFilter_createdBefore,
    importReferenceFilter_status,

    -- ** ImportReferenceJobItem
    importReferenceJobItem_completionTime,
    importReferenceJobItem_id,
    importReferenceJobItem_referenceStoreId,
    importReferenceJobItem_roleArn,
    importReferenceJobItem_status,
    importReferenceJobItem_creationTime,

    -- ** ImportReferenceSourceItem
    importReferenceSourceItem_description,
    importReferenceSourceItem_name,
    importReferenceSourceItem_sourceFile,
    importReferenceSourceItem_statusMessage,
    importReferenceSourceItem_tags,
    importReferenceSourceItem_status,

    -- ** ListAnnotationImportJobsFilter
    listAnnotationImportJobsFilter_status,
    listAnnotationImportJobsFilter_storeName,

    -- ** ListAnnotationStoresFilter
    listAnnotationStoresFilter_status,

    -- ** ListVariantImportJobsFilter
    listVariantImportJobsFilter_status,
    listVariantImportJobsFilter_storeName,

    -- ** ListVariantStoresFilter
    listVariantStoresFilter_status,

    -- ** MultipartReadSetUploadListItem
    multipartReadSetUploadListItem_description,
    multipartReadSetUploadListItem_name,
    multipartReadSetUploadListItem_tags,
    multipartReadSetUploadListItem_sequenceStoreId,
    multipartReadSetUploadListItem_uploadId,
    multipartReadSetUploadListItem_sourceFileType,
    multipartReadSetUploadListItem_subjectId,
    multipartReadSetUploadListItem_sampleId,
    multipartReadSetUploadListItem_generatedFrom,
    multipartReadSetUploadListItem_referenceArn,
    multipartReadSetUploadListItem_creationTime,

    -- ** ReadOptions
    readOptions_comment,
    readOptions_encoding,
    readOptions_escape,
    readOptions_escapeQuotes,
    readOptions_header,
    readOptions_lineSep,
    readOptions_quote,
    readOptions_quoteAll,
    readOptions_sep,

    -- ** ReadSetBatchError
    readSetBatchError_id,
    readSetBatchError_code,
    readSetBatchError_message,

    -- ** ReadSetFiles
    readSetFiles_index,
    readSetFiles_source1,
    readSetFiles_source2,

    -- ** ReadSetFilter
    readSetFilter_createdAfter,
    readSetFilter_createdBefore,
    readSetFilter_generatedFrom,
    readSetFilter_name,
    readSetFilter_referenceArn,
    readSetFilter_sampleId,
    readSetFilter_status,
    readSetFilter_subjectId,

    -- ** ReadSetListItem
    readSetListItem_description,
    readSetListItem_name,
    readSetListItem_referenceArn,
    readSetListItem_sampleId,
    readSetListItem_sequenceInformation,
    readSetListItem_statusMessage,
    readSetListItem_subjectId,
    readSetListItem_id,
    readSetListItem_arn,
    readSetListItem_sequenceStoreId,
    readSetListItem_status,
    readSetListItem_fileType,
    readSetListItem_creationTime,

    -- ** ReadSetUploadPartListFilter
    readSetUploadPartListFilter_createdAfter,
    readSetUploadPartListFilter_createdBefore,

    -- ** ReadSetUploadPartListItem
    readSetUploadPartListItem_creationTime,
    readSetUploadPartListItem_lastUpdatedTime,
    readSetUploadPartListItem_partNumber,
    readSetUploadPartListItem_partSize,
    readSetUploadPartListItem_partSource,
    readSetUploadPartListItem_checksum,

    -- ** ReferenceFiles
    referenceFiles_index,
    referenceFiles_source,

    -- ** ReferenceFilter
    referenceFilter_createdAfter,
    referenceFilter_createdBefore,
    referenceFilter_md5,
    referenceFilter_name,

    -- ** ReferenceItem
    referenceItem_referenceArn,

    -- ** ReferenceListItem
    referenceListItem_description,
    referenceListItem_name,
    referenceListItem_status,
    referenceListItem_id,
    referenceListItem_arn,
    referenceListItem_referenceStoreId,
    referenceListItem_md5,
    referenceListItem_creationTime,
    referenceListItem_updateTime,

    -- ** ReferenceStoreDetail
    referenceStoreDetail_description,
    referenceStoreDetail_name,
    referenceStoreDetail_sseConfig,
    referenceStoreDetail_arn,
    referenceStoreDetail_id,
    referenceStoreDetail_creationTime,

    -- ** ReferenceStoreFilter
    referenceStoreFilter_createdAfter,
    referenceStoreFilter_createdBefore,
    referenceStoreFilter_name,

    -- ** RunGroupListItem
    runGroupListItem_arn,
    runGroupListItem_creationTime,
    runGroupListItem_id,
    runGroupListItem_maxCpus,
    runGroupListItem_maxDuration,
    runGroupListItem_maxGpus,
    runGroupListItem_maxRuns,
    runGroupListItem_name,

    -- ** RunListItem
    runListItem_arn,
    runListItem_creationTime,
    runListItem_id,
    runListItem_name,
    runListItem_priority,
    runListItem_startTime,
    runListItem_status,
    runListItem_stopTime,
    runListItem_storageCapacity,
    runListItem_workflowId,

    -- ** RunParameters

    -- ** SequenceInformation
    sequenceInformation_alignment,
    sequenceInformation_generatedFrom,
    sequenceInformation_totalBaseCount,
    sequenceInformation_totalReadCount,

    -- ** SequenceStoreDetail
    sequenceStoreDetail_description,
    sequenceStoreDetail_fallbackLocation,
    sequenceStoreDetail_name,
    sequenceStoreDetail_sseConfig,
    sequenceStoreDetail_arn,
    sequenceStoreDetail_id,
    sequenceStoreDetail_creationTime,

    -- ** SequenceStoreFilter
    sequenceStoreFilter_createdAfter,
    sequenceStoreFilter_createdBefore,
    sequenceStoreFilter_name,

    -- ** SourceFiles
    sourceFiles_source2,
    sourceFiles_source1,

    -- ** SseConfig
    sseConfig_keyArn,
    sseConfig_type,

    -- ** StartReadSetActivationJobSourceItem
    startReadSetActivationJobSourceItem_readSetId,

    -- ** StartReadSetImportJobSourceItem
    startReadSetImportJobSourceItem_description,
    startReadSetImportJobSourceItem_generatedFrom,
    startReadSetImportJobSourceItem_name,
    startReadSetImportJobSourceItem_tags,
    startReadSetImportJobSourceItem_sourceFiles,
    startReadSetImportJobSourceItem_sourceFileType,
    startReadSetImportJobSourceItem_subjectId,
    startReadSetImportJobSourceItem_sampleId,
    startReadSetImportJobSourceItem_referenceArn,

    -- ** StartReferenceImportJobSourceItem
    startReferenceImportJobSourceItem_description,
    startReferenceImportJobSourceItem_tags,
    startReferenceImportJobSourceItem_sourceFile,
    startReferenceImportJobSourceItem_name,

    -- ** StoreOptions
    storeOptions_tsvStoreOptions,

    -- ** TaskListItem
    taskListItem_cpus,
    taskListItem_creationTime,
    taskListItem_gpus,
    taskListItem_memory,
    taskListItem_name,
    taskListItem_startTime,
    taskListItem_status,
    taskListItem_stopTime,
    taskListItem_taskId,

    -- ** TsvOptions
    tsvOptions_readOptions,

    -- ** TsvStoreOptions
    tsvStoreOptions_annotationType,
    tsvStoreOptions_formatToHeader,
    tsvStoreOptions_schema,

    -- ** VariantImportItemDetail
    variantImportItemDetail_statusMessage,
    variantImportItemDetail_source,
    variantImportItemDetail_jobStatus,

    -- ** VariantImportItemSource
    variantImportItemSource_source,

    -- ** VariantImportJobItem
    variantImportJobItem_annotationFields,
    variantImportJobItem_completionTime,
    variantImportJobItem_runLeftNormalization,
    variantImportJobItem_id,
    variantImportJobItem_destinationName,
    variantImportJobItem_roleArn,
    variantImportJobItem_status,
    variantImportJobItem_creationTime,
    variantImportJobItem_updateTime,

    -- ** VariantStoreItem
    variantStoreItem_id,
    variantStoreItem_reference,
    variantStoreItem_status,
    variantStoreItem_storeArn,
    variantStoreItem_name,
    variantStoreItem_description,
    variantStoreItem_sseConfig,
    variantStoreItem_creationTime,
    variantStoreItem_updateTime,
    variantStoreItem_statusMessage,
    variantStoreItem_storeSizeBytes,

    -- ** VcfOptions
    vcfOptions_ignoreFilterField,
    vcfOptions_ignoreQualField,

    -- ** WorkflowListItem
    workflowListItem_arn,
    workflowListItem_creationTime,
    workflowListItem_digest,
    workflowListItem_id,
    workflowListItem_metadata,
    workflowListItem_name,
    workflowListItem_status,
    workflowListItem_type,

    -- ** WorkflowParameter
    workflowParameter_description,
    workflowParameter_optional,
  )
where

import Amazonka.Omics.AbortMultipartReadSetUpload
import Amazonka.Omics.BatchDeleteReadSet
import Amazonka.Omics.CancelAnnotationImportJob
import Amazonka.Omics.CancelRun
import Amazonka.Omics.CancelVariantImportJob
import Amazonka.Omics.CompleteMultipartReadSetUpload
import Amazonka.Omics.CreateAnnotationStore
import Amazonka.Omics.CreateMultipartReadSetUpload
import Amazonka.Omics.CreateReferenceStore
import Amazonka.Omics.CreateRunGroup
import Amazonka.Omics.CreateSequenceStore
import Amazonka.Omics.CreateVariantStore
import Amazonka.Omics.CreateWorkflow
import Amazonka.Omics.DeleteAnnotationStore
import Amazonka.Omics.DeleteReference
import Amazonka.Omics.DeleteReferenceStore
import Amazonka.Omics.DeleteRun
import Amazonka.Omics.DeleteRunGroup
import Amazonka.Omics.DeleteSequenceStore
import Amazonka.Omics.DeleteVariantStore
import Amazonka.Omics.DeleteWorkflow
import Amazonka.Omics.GetAnnotationImportJob
import Amazonka.Omics.GetAnnotationStore
import Amazonka.Omics.GetReadSet
import Amazonka.Omics.GetReadSetActivationJob
import Amazonka.Omics.GetReadSetExportJob
import Amazonka.Omics.GetReadSetImportJob
import Amazonka.Omics.GetReadSetMetadata
import Amazonka.Omics.GetReference
import Amazonka.Omics.GetReferenceImportJob
import Amazonka.Omics.GetReferenceMetadata
import Amazonka.Omics.GetReferenceStore
import Amazonka.Omics.GetRun
import Amazonka.Omics.GetRunGroup
import Amazonka.Omics.GetRunTask
import Amazonka.Omics.GetSequenceStore
import Amazonka.Omics.GetVariantImportJob
import Amazonka.Omics.GetVariantStore
import Amazonka.Omics.GetWorkflow
import Amazonka.Omics.ListAnnotationImportJobs
import Amazonka.Omics.ListAnnotationStores
import Amazonka.Omics.ListMultipartReadSetUploads
import Amazonka.Omics.ListReadSetActivationJobs
import Amazonka.Omics.ListReadSetExportJobs
import Amazonka.Omics.ListReadSetImportJobs
import Amazonka.Omics.ListReadSetUploadParts
import Amazonka.Omics.ListReadSets
import Amazonka.Omics.ListReferenceImportJobs
import Amazonka.Omics.ListReferenceStores
import Amazonka.Omics.ListReferences
import Amazonka.Omics.ListRunGroups
import Amazonka.Omics.ListRunTasks
import Amazonka.Omics.ListRuns
import Amazonka.Omics.ListSequenceStores
import Amazonka.Omics.ListTagsForResource
import Amazonka.Omics.ListVariantImportJobs
import Amazonka.Omics.ListVariantStores
import Amazonka.Omics.ListWorkflows
import Amazonka.Omics.StartAnnotationImportJob
import Amazonka.Omics.StartReadSetActivationJob
import Amazonka.Omics.StartReadSetExportJob
import Amazonka.Omics.StartReadSetImportJob
import Amazonka.Omics.StartReferenceImportJob
import Amazonka.Omics.StartRun
import Amazonka.Omics.StartVariantImportJob
import Amazonka.Omics.TagResource
import Amazonka.Omics.Types.ActivateReadSetFilter
import Amazonka.Omics.Types.ActivateReadSetJobItem
import Amazonka.Omics.Types.ActivateReadSetSourceItem
import Amazonka.Omics.Types.AnnotationImportItemDetail
import Amazonka.Omics.Types.AnnotationImportItemSource
import Amazonka.Omics.Types.AnnotationImportJobItem
import Amazonka.Omics.Types.AnnotationStoreItem
import Amazonka.Omics.Types.CompleteReadSetUploadPartListItem
import Amazonka.Omics.Types.ExportReadSet
import Amazonka.Omics.Types.ExportReadSetDetail
import Amazonka.Omics.Types.ExportReadSetFilter
import Amazonka.Omics.Types.ExportReadSetJobDetail
import Amazonka.Omics.Types.FileInformation
import Amazonka.Omics.Types.FormatOptions
import Amazonka.Omics.Types.ImportReadSetFilter
import Amazonka.Omics.Types.ImportReadSetJobItem
import Amazonka.Omics.Types.ImportReadSetSourceItem
import Amazonka.Omics.Types.ImportReferenceFilter
import Amazonka.Omics.Types.ImportReferenceJobItem
import Amazonka.Omics.Types.ImportReferenceSourceItem
import Amazonka.Omics.Types.ListAnnotationImportJobsFilter
import Amazonka.Omics.Types.ListAnnotationStoresFilter
import Amazonka.Omics.Types.ListVariantImportJobsFilter
import Amazonka.Omics.Types.ListVariantStoresFilter
import Amazonka.Omics.Types.MultipartReadSetUploadListItem
import Amazonka.Omics.Types.ReadOptions
import Amazonka.Omics.Types.ReadSetBatchError
import Amazonka.Omics.Types.ReadSetFiles
import Amazonka.Omics.Types.ReadSetFilter
import Amazonka.Omics.Types.ReadSetListItem
import Amazonka.Omics.Types.ReadSetUploadPartListFilter
import Amazonka.Omics.Types.ReadSetUploadPartListItem
import Amazonka.Omics.Types.ReferenceFiles
import Amazonka.Omics.Types.ReferenceFilter
import Amazonka.Omics.Types.ReferenceItem
import Amazonka.Omics.Types.ReferenceListItem
import Amazonka.Omics.Types.ReferenceStoreDetail
import Amazonka.Omics.Types.ReferenceStoreFilter
import Amazonka.Omics.Types.RunGroupListItem
import Amazonka.Omics.Types.RunListItem
import Amazonka.Omics.Types.RunParameters
import Amazonka.Omics.Types.SequenceInformation
import Amazonka.Omics.Types.SequenceStoreDetail
import Amazonka.Omics.Types.SequenceStoreFilter
import Amazonka.Omics.Types.SourceFiles
import Amazonka.Omics.Types.SseConfig
import Amazonka.Omics.Types.StartReadSetActivationJobSourceItem
import Amazonka.Omics.Types.StartReadSetImportJobSourceItem
import Amazonka.Omics.Types.StartReferenceImportJobSourceItem
import Amazonka.Omics.Types.StoreOptions
import Amazonka.Omics.Types.TaskListItem
import Amazonka.Omics.Types.TsvOptions
import Amazonka.Omics.Types.TsvStoreOptions
import Amazonka.Omics.Types.VariantImportItemDetail
import Amazonka.Omics.Types.VariantImportItemSource
import Amazonka.Omics.Types.VariantImportJobItem
import Amazonka.Omics.Types.VariantStoreItem
import Amazonka.Omics.Types.VcfOptions
import Amazonka.Omics.Types.WorkflowListItem
import Amazonka.Omics.Types.WorkflowParameter
import Amazonka.Omics.UntagResource
import Amazonka.Omics.UpdateAnnotationStore
import Amazonka.Omics.UpdateRunGroup
import Amazonka.Omics.UpdateVariantStore
import Amazonka.Omics.UpdateWorkflow
import Amazonka.Omics.UploadReadSetPart
