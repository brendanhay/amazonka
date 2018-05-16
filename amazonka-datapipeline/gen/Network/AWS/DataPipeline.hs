{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Data Pipeline configures and manages a data-driven workflow called a pipeline. AWS Data Pipeline handles the details of scheduling and ensuring that data dependencies are met so that your application can focus on processing the data.
--
--
-- AWS Data Pipeline provides a JAR implementation of a task runner called AWS Data Pipeline Task Runner. AWS Data Pipeline Task Runner provides logic for common data management scenarios, such as performing database queries and running data analysis using Amazon Elastic MapReduce (Amazon EMR). You can use AWS Data Pipeline Task Runner as your task runner, or you can write your own task runner to provide custom data management.
--
-- AWS Data Pipeline implements two main sets of functionality. Use the first set to create a pipeline and define data sources, schedules, dependencies, and the transforms to be performed on the data. Use the second set in your task runner application to receive the next task ready for processing. The logic for performing the task, such as querying the data, running data analysis, or converting the data from one format to another, is contained within the task runner. The task runner performs the task assigned to it by the web service, reporting progress to the web service as it does so. When the task is done, the task runner reports the final success or failure of the task to the web service.
--
module Network.AWS.DataPipeline
    (
    -- * Service Configuration
      dataPipeline

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** InternalServiceError
    , _InternalServiceError

    -- ** PipelineDeletedException
    , _PipelineDeletedException

    -- ** PipelineNotFoundException
    , _PipelineNotFoundException

    -- ** TaskNotFoundException
    , _TaskNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePipelines
    , module Network.AWS.DataPipeline.DescribePipelines

    -- ** QueryObjects (Paginated)
    , module Network.AWS.DataPipeline.QueryObjects

    -- ** RemoveTags
    , module Network.AWS.DataPipeline.RemoveTags

    -- ** DeletePipeline
    , module Network.AWS.DataPipeline.DeletePipeline

    -- ** ListPipelines (Paginated)
    , module Network.AWS.DataPipeline.ListPipelines

    -- ** EvaluateExpression
    , module Network.AWS.DataPipeline.EvaluateExpression

    -- ** GetPipelineDefinition
    , module Network.AWS.DataPipeline.GetPipelineDefinition

    -- ** PollForTask
    , module Network.AWS.DataPipeline.PollForTask

    -- ** DeactivatePipeline
    , module Network.AWS.DataPipeline.DeactivatePipeline

    -- ** AddTags
    , module Network.AWS.DataPipeline.AddTags

    -- ** DescribeObjects (Paginated)
    , module Network.AWS.DataPipeline.DescribeObjects

    -- ** ReportTaskRunnerHeartbeat
    , module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat

    -- ** ActivatePipeline
    , module Network.AWS.DataPipeline.ActivatePipeline

    -- ** SetTaskStatus
    , module Network.AWS.DataPipeline.SetTaskStatus

    -- ** SetStatus
    , module Network.AWS.DataPipeline.SetStatus

    -- ** ReportTaskProgress
    , module Network.AWS.DataPipeline.ReportTaskProgress

    -- ** CreatePipeline
    , module Network.AWS.DataPipeline.CreatePipeline

    -- ** PutPipelineDefinition
    , module Network.AWS.DataPipeline.PutPipelineDefinition

    -- ** ValidatePipelineDefinition
    , module Network.AWS.DataPipeline.ValidatePipelineDefinition

    -- * Types

    -- ** OperatorType
    , OperatorType (..)

    -- ** TaskStatus
    , TaskStatus (..)

    -- ** Field
    , Field
    , field
    , fRefValue
    , fStringValue
    , fKey

    -- ** InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiSignature
    , iiDocument

    -- ** Operator
    , Operator
    , operator
    , oValues
    , oType

    -- ** ParameterAttribute
    , ParameterAttribute
    , parameterAttribute
    , paKey
    , paStringValue

    -- ** ParameterObject
    , ParameterObject
    , parameterObject
    , poId
    , poAttributes

    -- ** ParameterValue
    , ParameterValue
    , parameterValue
    , pvId
    , pvStringValue

    -- ** PipelineDescription
    , PipelineDescription
    , pipelineDescription
    , pdDescription
    , pdTags
    , pdPipelineId
    , pdName
    , pdFields

    -- ** PipelineIdName
    , PipelineIdName
    , pipelineIdName
    , pinName
    , pinId

    -- ** PipelineObject
    , PipelineObject
    , pipelineObject
    , pId
    , pName
    , pFields

    -- ** Query
    , Query
    , query
    , qSelectors

    -- ** Selector
    , Selector
    , selector
    , sOperator
    , sFieldName

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TaskObject
    , TaskObject
    , taskObject
    , toPipelineId
    , toAttemptId
    , toTaskId
    , toObjects

    -- ** ValidationError
    , ValidationError
    , validationError
    , veId
    , veErrors

    -- ** ValidationWarning
    , ValidationWarning
    , validationWarning
    , vwWarnings
    , vwId
    ) where

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
import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.ValidatePipelineDefinition
import Network.AWS.DataPipeline.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DataPipeline'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
