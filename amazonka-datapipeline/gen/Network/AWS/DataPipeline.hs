-- Module      : Network.AWS.DataPipeline
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS Data Pipeline is a web service that you can use to automate the movement
-- and transformation of data. With AWS Data Pipeline, you can define
-- data-driven workflows, so that tasks can be dependent on the successful
-- completion of previous tasks.
module Network.AWS.DataPipeline
    ( module Network.AWS.DataPipeline.ActivatePipeline
    , module Network.AWS.DataPipeline.CreatePipeline
    , module Network.AWS.DataPipeline.DeletePipeline
    , module Network.AWS.DataPipeline.DescribeObjects
    , module Network.AWS.DataPipeline.DescribePipelines
    , module Network.AWS.DataPipeline.EvaluateExpression
    , module Network.AWS.DataPipeline.GetPipelineDefinition
    , module Network.AWS.DataPipeline.ListPipelines
    , module Network.AWS.DataPipeline.PollForTask
    , module Network.AWS.DataPipeline.PutPipelineDefinition
    , module Network.AWS.DataPipeline.QueryObjects
    , module Network.AWS.DataPipeline.ReportTaskProgress
    , module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
    , module Network.AWS.DataPipeline.SetStatus
    , module Network.AWS.DataPipeline.SetTaskStatus
    , module Network.AWS.DataPipeline.Types
    , module Network.AWS.DataPipeline.ValidatePipelineDefinition
    ) where

import Network.AWS.DataPipeline.ActivatePipeline
import Network.AWS.DataPipeline.CreatePipeline
import Network.AWS.DataPipeline.DeletePipeline
import Network.AWS.DataPipeline.DescribeObjects
import Network.AWS.DataPipeline.DescribePipelines
import Network.AWS.DataPipeline.EvaluateExpression
import Network.AWS.DataPipeline.GetPipelineDefinition
import Network.AWS.DataPipeline.ListPipelines
import Network.AWS.DataPipeline.PollForTask
import Network.AWS.DataPipeline.PutPipelineDefinition
import Network.AWS.DataPipeline.QueryObjects
import Network.AWS.DataPipeline.ReportTaskProgress
import Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
import Network.AWS.DataPipeline.SetStatus
import Network.AWS.DataPipeline.SetTaskStatus
import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.ValidatePipelineDefinition
