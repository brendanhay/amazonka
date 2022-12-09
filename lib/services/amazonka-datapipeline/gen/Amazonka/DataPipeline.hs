{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DataPipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2012-10-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Data Pipeline configures and manages a data-driven workflow called a
-- pipeline. AWS Data Pipeline handles the details of scheduling and
-- ensuring that data dependencies are met so that your application can
-- focus on processing the data.
--
-- AWS Data Pipeline provides a JAR implementation of a task runner called
-- AWS Data Pipeline Task Runner. AWS Data Pipeline Task Runner provides
-- logic for common data management scenarios, such as performing database
-- queries and running data analysis using Amazon Elastic MapReduce (Amazon
-- EMR). You can use AWS Data Pipeline Task Runner as your task runner, or
-- you can write your own task runner to provide custom data management.
--
-- AWS Data Pipeline implements two main sets of functionality. Use the
-- first set to create a pipeline and define data sources, schedules,
-- dependencies, and the transforms to be performed on the data. Use the
-- second set in your task runner application to receive the next task
-- ready for processing. The logic for performing the task, such as
-- querying the data, running data analysis, or converting the data from
-- one format to another, is contained within the task runner. The task
-- runner performs the task assigned to it by the web service, reporting
-- progress to the web service as it does so. When the task is done, the
-- task runner reports the final success or failure of the task to the web
-- service.
module Amazonka.DataPipeline
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** PipelineDeletedException
    _PipelineDeletedException,

    -- ** PipelineNotFoundException
    _PipelineNotFoundException,

    -- ** TaskNotFoundException
    _TaskNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ActivatePipeline
    ActivatePipeline (ActivatePipeline'),
    newActivatePipeline,
    ActivatePipelineResponse (ActivatePipelineResponse'),
    newActivatePipelineResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** DeactivatePipeline
    DeactivatePipeline (DeactivatePipeline'),
    newDeactivatePipeline,
    DeactivatePipelineResponse (DeactivatePipelineResponse'),
    newDeactivatePipelineResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** DescribeObjects (Paginated)
    DescribeObjects (DescribeObjects'),
    newDescribeObjects,
    DescribeObjectsResponse (DescribeObjectsResponse'),
    newDescribeObjectsResponse,

    -- ** DescribePipelines
    DescribePipelines (DescribePipelines'),
    newDescribePipelines,
    DescribePipelinesResponse (DescribePipelinesResponse'),
    newDescribePipelinesResponse,

    -- ** EvaluateExpression
    EvaluateExpression (EvaluateExpression'),
    newEvaluateExpression,
    EvaluateExpressionResponse (EvaluateExpressionResponse'),
    newEvaluateExpressionResponse,

    -- ** GetPipelineDefinition
    GetPipelineDefinition (GetPipelineDefinition'),
    newGetPipelineDefinition,
    GetPipelineDefinitionResponse (GetPipelineDefinitionResponse'),
    newGetPipelineDefinitionResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** PollForTask
    PollForTask (PollForTask'),
    newPollForTask,
    PollForTaskResponse (PollForTaskResponse'),
    newPollForTaskResponse,

    -- ** PutPipelineDefinition
    PutPipelineDefinition (PutPipelineDefinition'),
    newPutPipelineDefinition,
    PutPipelineDefinitionResponse (PutPipelineDefinitionResponse'),
    newPutPipelineDefinitionResponse,

    -- ** QueryObjects (Paginated)
    QueryObjects (QueryObjects'),
    newQueryObjects,
    QueryObjectsResponse (QueryObjectsResponse'),
    newQueryObjectsResponse,

    -- ** RemoveTags
    RemoveTags (RemoveTags'),
    newRemoveTags,
    RemoveTagsResponse (RemoveTagsResponse'),
    newRemoveTagsResponse,

    -- ** ReportTaskProgress
    ReportTaskProgress (ReportTaskProgress'),
    newReportTaskProgress,
    ReportTaskProgressResponse (ReportTaskProgressResponse'),
    newReportTaskProgressResponse,

    -- ** ReportTaskRunnerHeartbeat
    ReportTaskRunnerHeartbeat (ReportTaskRunnerHeartbeat'),
    newReportTaskRunnerHeartbeat,
    ReportTaskRunnerHeartbeatResponse (ReportTaskRunnerHeartbeatResponse'),
    newReportTaskRunnerHeartbeatResponse,

    -- ** SetStatus
    SetStatus (SetStatus'),
    newSetStatus,
    SetStatusResponse (SetStatusResponse'),
    newSetStatusResponse,

    -- ** SetTaskStatus
    SetTaskStatus (SetTaskStatus'),
    newSetTaskStatus,
    SetTaskStatusResponse (SetTaskStatusResponse'),
    newSetTaskStatusResponse,

    -- ** ValidatePipelineDefinition
    ValidatePipelineDefinition (ValidatePipelineDefinition'),
    newValidatePipelineDefinition,
    ValidatePipelineDefinitionResponse (ValidatePipelineDefinitionResponse'),
    newValidatePipelineDefinitionResponse,

    -- * Types

    -- ** OperatorType
    OperatorType (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** Field
    Field (Field'),
    newField,

    -- ** InstanceIdentity
    InstanceIdentity (InstanceIdentity'),
    newInstanceIdentity,

    -- ** Operator
    Operator (Operator'),
    newOperator,

    -- ** ParameterAttribute
    ParameterAttribute (ParameterAttribute'),
    newParameterAttribute,

    -- ** ParameterObject
    ParameterObject (ParameterObject'),
    newParameterObject,

    -- ** ParameterValue
    ParameterValue (ParameterValue'),
    newParameterValue,

    -- ** PipelineDescription
    PipelineDescription (PipelineDescription'),
    newPipelineDescription,

    -- ** PipelineIdName
    PipelineIdName (PipelineIdName'),
    newPipelineIdName,

    -- ** PipelineObject
    PipelineObject (PipelineObject'),
    newPipelineObject,

    -- ** Query
    Query (Query'),
    newQuery,

    -- ** Selector
    Selector (Selector'),
    newSelector,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TaskObject
    TaskObject (TaskObject'),
    newTaskObject,

    -- ** ValidationError
    ValidationError (ValidationError'),
    newValidationError,

    -- ** ValidationWarning
    ValidationWarning (ValidationWarning'),
    newValidationWarning,
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
import Amazonka.DataPipeline.Lens
import Amazonka.DataPipeline.ListPipelines
import Amazonka.DataPipeline.PollForTask
import Amazonka.DataPipeline.PutPipelineDefinition
import Amazonka.DataPipeline.QueryObjects
import Amazonka.DataPipeline.RemoveTags
import Amazonka.DataPipeline.ReportTaskProgress
import Amazonka.DataPipeline.ReportTaskRunnerHeartbeat
import Amazonka.DataPipeline.SetStatus
import Amazonka.DataPipeline.SetTaskStatus
import Amazonka.DataPipeline.Types
import Amazonka.DataPipeline.ValidatePipelineDefinition
import Amazonka.DataPipeline.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DataPipeline'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
