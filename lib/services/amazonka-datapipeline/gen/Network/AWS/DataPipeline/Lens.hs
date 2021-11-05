{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataPipeline.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Lens
  ( -- * Operations

    -- ** DescribePipelines
    describePipelines_pipelineIds,
    describePipelinesResponse_httpStatus,
    describePipelinesResponse_pipelineDescriptionList,

    -- ** QueryObjects
    queryObjects_query,
    queryObjects_marker,
    queryObjects_limit,
    queryObjects_pipelineId,
    queryObjects_sphere,
    queryObjectsResponse_hasMoreResults,
    queryObjectsResponse_ids,
    queryObjectsResponse_marker,
    queryObjectsResponse_httpStatus,

    -- ** RemoveTags
    removeTags_pipelineId,
    removeTags_tagKeys,
    removeTagsResponse_httpStatus,

    -- ** DeletePipeline
    deletePipeline_pipelineId,

    -- ** ListPipelines
    listPipelines_marker,
    listPipelinesResponse_hasMoreResults,
    listPipelinesResponse_marker,
    listPipelinesResponse_httpStatus,
    listPipelinesResponse_pipelineIdList,

    -- ** EvaluateExpression
    evaluateExpression_pipelineId,
    evaluateExpression_objectId,
    evaluateExpression_expression,
    evaluateExpressionResponse_httpStatus,
    evaluateExpressionResponse_evaluatedExpression,

    -- ** GetPipelineDefinition
    getPipelineDefinition_version,
    getPipelineDefinition_pipelineId,
    getPipelineDefinitionResponse_pipelineObjects,
    getPipelineDefinitionResponse_parameterObjects,
    getPipelineDefinitionResponse_parameterValues,
    getPipelineDefinitionResponse_httpStatus,

    -- ** PollForTask
    pollForTask_hostname,
    pollForTask_instanceIdentity,
    pollForTask_workerGroup,
    pollForTaskResponse_taskObject,
    pollForTaskResponse_httpStatus,

    -- ** DeactivatePipeline
    deactivatePipeline_cancelActive,
    deactivatePipeline_pipelineId,
    deactivatePipelineResponse_httpStatus,

    -- ** AddTags
    addTags_pipelineId,
    addTags_tags,
    addTagsResponse_httpStatus,

    -- ** DescribeObjects
    describeObjects_evaluateExpressions,
    describeObjects_marker,
    describeObjects_pipelineId,
    describeObjects_objectIds,
    describeObjectsResponse_hasMoreResults,
    describeObjectsResponse_marker,
    describeObjectsResponse_httpStatus,
    describeObjectsResponse_pipelineObjects,

    -- ** ReportTaskRunnerHeartbeat
    reportTaskRunnerHeartbeat_hostname,
    reportTaskRunnerHeartbeat_workerGroup,
    reportTaskRunnerHeartbeat_taskrunnerId,
    reportTaskRunnerHeartbeatResponse_httpStatus,
    reportTaskRunnerHeartbeatResponse_terminate,

    -- ** ActivatePipeline
    activatePipeline_startTimestamp,
    activatePipeline_parameterValues,
    activatePipeline_pipelineId,
    activatePipelineResponse_httpStatus,

    -- ** SetTaskStatus
    setTaskStatus_errorStackTrace,
    setTaskStatus_errorId,
    setTaskStatus_errorMessage,
    setTaskStatus_taskId,
    setTaskStatus_taskStatus,
    setTaskStatusResponse_httpStatus,

    -- ** SetStatus
    setStatus_pipelineId,
    setStatus_objectIds,
    setStatus_status,

    -- ** ReportTaskProgress
    reportTaskProgress_fields,
    reportTaskProgress_taskId,
    reportTaskProgressResponse_httpStatus,
    reportTaskProgressResponse_canceled,

    -- ** CreatePipeline
    createPipeline_description,
    createPipeline_tags,
    createPipeline_name,
    createPipeline_uniqueId,
    createPipelineResponse_httpStatus,
    createPipelineResponse_pipelineId,

    -- ** PutPipelineDefinition
    putPipelineDefinition_parameterObjects,
    putPipelineDefinition_parameterValues,
    putPipelineDefinition_pipelineId,
    putPipelineDefinition_pipelineObjects,
    putPipelineDefinitionResponse_validationErrors,
    putPipelineDefinitionResponse_validationWarnings,
    putPipelineDefinitionResponse_httpStatus,
    putPipelineDefinitionResponse_errored,

    -- ** ValidatePipelineDefinition
    validatePipelineDefinition_parameterObjects,
    validatePipelineDefinition_parameterValues,
    validatePipelineDefinition_pipelineId,
    validatePipelineDefinition_pipelineObjects,
    validatePipelineDefinitionResponse_validationErrors,
    validatePipelineDefinitionResponse_validationWarnings,
    validatePipelineDefinitionResponse_httpStatus,
    validatePipelineDefinitionResponse_errored,

    -- * Types

    -- ** Field
    field_refValue,
    field_stringValue,
    field_key,

    -- ** InstanceIdentity
    instanceIdentity_signature,
    instanceIdentity_document,

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
    pipelineDescription_description,
    pipelineDescription_tags,
    pipelineDescription_pipelineId,
    pipelineDescription_name,
    pipelineDescription_fields,

    -- ** PipelineIdName
    pipelineIdName_name,
    pipelineIdName_id,

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
    taskObject_attemptId,
    taskObject_taskId,
    taskObject_objects,

    -- ** ValidationError
    validationError_id,
    validationError_errors,

    -- ** ValidationWarning
    validationWarning_warnings,
    validationWarning_id,
  )
where

import Amazonka.DataPipeline.ActivatePipeline
import Amazonka.DataPipeline.AddTags
import Amazonka.DataPipeline.CreatePipeline
import Amazonka.DataPipeline.DeactivatePipeline
import Amazonka.DataPipeline.DeletePipeline
import Amazonka.DataPipeline.DescribeObjects
import Amazonka.DataPipeline.DescribePipelines
import Amazonka.DataPipeline.EvaluateExpression
import Amazonka.DataPipeline.GetPipelineDefinition
import Amazonka.DataPipeline.ListPipelines
import Amazonka.DataPipeline.PollForTask
import Amazonka.DataPipeline.PutPipelineDefinition
import Amazonka.DataPipeline.QueryObjects
import Amazonka.DataPipeline.RemoveTags
import Amazonka.DataPipeline.ReportTaskProgress
import Amazonka.DataPipeline.ReportTaskRunnerHeartbeat
import Amazonka.DataPipeline.SetStatus
import Amazonka.DataPipeline.SetTaskStatus
import Amazonka.DataPipeline.Types.Field
import Amazonka.DataPipeline.Types.InstanceIdentity
import Amazonka.DataPipeline.Types.Operator
import Amazonka.DataPipeline.Types.ParameterAttribute
import Amazonka.DataPipeline.Types.ParameterObject
import Amazonka.DataPipeline.Types.ParameterValue
import Amazonka.DataPipeline.Types.PipelineDescription
import Amazonka.DataPipeline.Types.PipelineIdName
import Amazonka.DataPipeline.Types.PipelineObject
import Amazonka.DataPipeline.Types.Query
import Amazonka.DataPipeline.Types.Selector
import Amazonka.DataPipeline.Types.Tag
import Amazonka.DataPipeline.Types.TaskObject
import Amazonka.DataPipeline.Types.ValidationError
import Amazonka.DataPipeline.Types.ValidationWarning
import Amazonka.DataPipeline.ValidatePipelineDefinition
