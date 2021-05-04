{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Lens
  ( -- * Operations

    -- ** RemoveTags
    removeTags_pipelineId,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** QueryObjects
    queryObjects_query,
    queryObjects_limit,
    queryObjects_marker,
    queryObjects_pipelineId,
    queryObjects_sphere,
    queryObjectsResponse_ids,
    queryObjectsResponse_hasMoreResults,
    queryObjectsResponse_marker,
    queryObjectsResponse_httpStatus,

    -- ** EvaluateExpression
    evaluateExpression_pipelineId,
    evaluateExpression_objectId,
    evaluateExpression_expression,
    evaluateExpressionResponse_httpStatus,
    evaluateExpressionResponse_evaluatedExpression,

    -- ** CreatePipeline
    createPipeline_tags,
    createPipeline_description,
    createPipeline_name,
    createPipeline_uniqueId,
    createPipelineResponse_httpStatus,
    createPipelineResponse_pipelineId,

    -- ** ReportTaskProgress
    reportTaskProgress_fields,
    reportTaskProgress_taskId,
    reportTaskProgressResponse_httpStatus,
    reportTaskProgressResponse_canceled,

    -- ** SetTaskStatus
    setTaskStatus_errorStackTrace,
    setTaskStatus_errorMessage,
    setTaskStatus_errorId,
    setTaskStatus_taskId,
    setTaskStatus_taskStatus,
    setTaskStatusResponse_httpStatus,

    -- ** DescribeObjects
    describeObjects_marker,
    describeObjects_evaluateExpressions,
    describeObjects_pipelineId,
    describeObjects_objectIds,
    describeObjectsResponse_hasMoreResults,
    describeObjectsResponse_marker,
    describeObjectsResponse_httpStatus,
    describeObjectsResponse_pipelineObjects,

    -- ** DeletePipeline
    deletePipeline_pipelineId,

    -- ** ReportTaskRunnerHeartbeat
    reportTaskRunnerHeartbeat_hostname,
    reportTaskRunnerHeartbeat_workerGroup,
    reportTaskRunnerHeartbeat_taskrunnerId,
    reportTaskRunnerHeartbeatResponse_httpStatus,
    reportTaskRunnerHeartbeatResponse_terminate,

    -- ** AddTags
    addTags_pipelineId,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** DescribePipelines
    describePipelines_pipelineIds,
    describePipelinesResponse_httpStatus,
    describePipelinesResponse_pipelineDescriptionList,

    -- ** GetPipelineDefinition
    getPipelineDefinition_version,
    getPipelineDefinition_pipelineId,
    getPipelineDefinitionResponse_parameterValues,
    getPipelineDefinitionResponse_parameterObjects,
    getPipelineDefinitionResponse_pipelineObjects,
    getPipelineDefinitionResponse_httpStatus,

    -- ** PollForTask
    pollForTask_hostname,
    pollForTask_instanceIdentity,
    pollForTask_workerGroup,
    pollForTaskResponse_taskObject,
    pollForTaskResponse_httpStatus,

    -- ** ValidatePipelineDefinition
    validatePipelineDefinition_parameterValues,
    validatePipelineDefinition_parameterObjects,
    validatePipelineDefinition_pipelineId,
    validatePipelineDefinition_pipelineObjects,
    validatePipelineDefinitionResponse_validationErrors,
    validatePipelineDefinitionResponse_validationWarnings,
    validatePipelineDefinitionResponse_httpStatus,
    validatePipelineDefinitionResponse_errored,

    -- ** PutPipelineDefinition
    putPipelineDefinition_parameterValues,
    putPipelineDefinition_parameterObjects,
    putPipelineDefinition_pipelineId,
    putPipelineDefinition_pipelineObjects,
    putPipelineDefinitionResponse_validationErrors,
    putPipelineDefinitionResponse_validationWarnings,
    putPipelineDefinitionResponse_httpStatus,
    putPipelineDefinitionResponse_errored,

    -- ** SetStatus
    setStatus_pipelineId,
    setStatus_objectIds,
    setStatus_status,

    -- ** ListPipelines
    listPipelines_marker,
    listPipelinesResponse_hasMoreResults,
    listPipelinesResponse_marker,
    listPipelinesResponse_httpStatus,
    listPipelinesResponse_pipelineIdList,

    -- ** ActivatePipeline
    activatePipeline_parameterValues,
    activatePipeline_startTimestamp,
    activatePipeline_pipelineId,
    activatePipelineResponse_httpStatus,

    -- ** DeactivatePipeline
    deactivatePipeline_cancelActive,
    deactivatePipeline_pipelineId,
    deactivatePipelineResponse_httpStatus,

    -- * Types

    -- ** Field
    field_stringValue,
    field_refValue,
    field_key,

    -- ** InstanceIdentity
    instanceIdentity_document,
    instanceIdentity_signature,

    -- ** Operator
    operator_values,
    operator_type,

    -- ** ParameterAttribute
    parameterAttribute_key,
    parameterAttribute_stringValue,

    -- ** ParameterObject
    parameterObject_id,
    parameterObject_attributes,

    -- ** ParameterValue
    parameterValue_id,
    parameterValue_stringValue,

    -- ** PipelineDescription
    pipelineDescription_tags,
    pipelineDescription_description,
    pipelineDescription_pipelineId,
    pipelineDescription_name,
    pipelineDescription_fields,

    -- ** PipelineIdName
    pipelineIdName_id,
    pipelineIdName_name,

    -- ** PipelineObject
    pipelineObject_id,
    pipelineObject_name,
    pipelineObject_fields,

    -- ** Query
    query_selectors,

    -- ** Selector
    selector_operator,
    selector_fieldName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TaskObject
    taskObject_pipelineId,
    taskObject_objects,
    taskObject_taskId,
    taskObject_attemptId,

    -- ** ValidationError
    validationError_id,
    validationError_errors,

    -- ** ValidationWarning
    validationWarning_warnings,
    validationWarning_id,
  )
where

import Network.AWS.DataPipeline.ActivatePipeline
import Network.AWS.DataPipeline.AddTags
import Network.AWS.DataPipeline.CreatePipeline
import Network.AWS.DataPipeline.DeactivatePipeline
import Network.AWS.DataPipeline.DeletePipeline
import Network.AWS.DataPipeline.DescribeObjects
import Network.AWS.DataPipeline.DescribePipelines
import Network.AWS.DataPipeline.EvaluateExpression
import Network.AWS.DataPipeline.GetPipelineDefinition
import Network.AWS.DataPipeline.ListPipelines
import Network.AWS.DataPipeline.PollForTask
import Network.AWS.DataPipeline.PutPipelineDefinition
import Network.AWS.DataPipeline.QueryObjects
import Network.AWS.DataPipeline.RemoveTags
import Network.AWS.DataPipeline.ReportTaskProgress
import Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
import Network.AWS.DataPipeline.SetStatus
import Network.AWS.DataPipeline.SetTaskStatus
import Network.AWS.DataPipeline.Types.Field
import Network.AWS.DataPipeline.Types.InstanceIdentity
import Network.AWS.DataPipeline.Types.Operator
import Network.AWS.DataPipeline.Types.ParameterAttribute
import Network.AWS.DataPipeline.Types.ParameterObject
import Network.AWS.DataPipeline.Types.ParameterValue
import Network.AWS.DataPipeline.Types.PipelineDescription
import Network.AWS.DataPipeline.Types.PipelineIdName
import Network.AWS.DataPipeline.Types.PipelineObject
import Network.AWS.DataPipeline.Types.Query
import Network.AWS.DataPipeline.Types.Selector
import Network.AWS.DataPipeline.Types.Tag
import Network.AWS.DataPipeline.Types.TaskObject
import Network.AWS.DataPipeline.Types.ValidationError
import Network.AWS.DataPipeline.Types.ValidationWarning
import Network.AWS.DataPipeline.ValidatePipelineDefinition
