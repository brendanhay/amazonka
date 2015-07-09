-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | AWS Data Pipeline configures and manages a data-driven workflow called a
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
module Network.AWS.DataPipeline
    ( module Export
    ) where

import           Network.AWS.DataPipeline.ActivatePipeline           as Export
import           Network.AWS.DataPipeline.AddTags                    as Export
import           Network.AWS.DataPipeline.CreatePipeline             as Export
import           Network.AWS.DataPipeline.DeactivatePipeline         as Export
import           Network.AWS.DataPipeline.DeletePipeline             as Export
import           Network.AWS.DataPipeline.DescribeObjects            as Export
import           Network.AWS.DataPipeline.DescribePipelines          as Export
import           Network.AWS.DataPipeline.EvaluateExpression         as Export
import           Network.AWS.DataPipeline.GetPipelineDefinition      as Export
import           Network.AWS.DataPipeline.ListPipelines              as Export
import           Network.AWS.DataPipeline.PollForTask                as Export
import           Network.AWS.DataPipeline.PutPipelineDefinition      as Export
import           Network.AWS.DataPipeline.QueryObjects               as Export
import           Network.AWS.DataPipeline.RemoveTags                 as Export
import           Network.AWS.DataPipeline.ReportTaskProgress         as Export
import           Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat  as Export
import           Network.AWS.DataPipeline.SetStatus                  as Export
import           Network.AWS.DataPipeline.SetTaskStatus              as Export
import           Network.AWS.DataPipeline.Types                      as Export
import           Network.AWS.DataPipeline.ValidatePipelineDefinition as Export
import           Network.AWS.DataPipeline.Waiters                    as Export
