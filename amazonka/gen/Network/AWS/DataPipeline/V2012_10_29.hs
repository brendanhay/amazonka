-- Module      : Network.AWS.DataPipeline.V2012_10_29
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Data Pipeline is a web service that you can use to automate the
-- movement and transformation of data. With AWS Data Pipeline, you can define
-- data-driven workflows, so that tasks can be dependent on the successful
-- completion of previous tasks.
module Network.AWS.DataPipeline.V2012_10_29 (module Export) where

import Network.AWS.DataPipeline.V2012_10_29.ActivatePipeline as Export
import Network.AWS.DataPipeline.V2012_10_29.CreatePipeline as Export
import Network.AWS.DataPipeline.V2012_10_29.DeletePipeline as Export
import Network.AWS.DataPipeline.V2012_10_29.DescribeObjects as Export
import Network.AWS.DataPipeline.V2012_10_29.DescribePipelines as Export
import Network.AWS.DataPipeline.V2012_10_29.EvaluateExpression as Export
import Network.AWS.DataPipeline.V2012_10_29.GetPipelineDefinition as Export
import Network.AWS.DataPipeline.V2012_10_29.ListPipelines as Export
import Network.AWS.DataPipeline.V2012_10_29.PollForTask as Export
import Network.AWS.DataPipeline.V2012_10_29.PutPipelineDefinition as Export
import Network.AWS.DataPipeline.V2012_10_29.QueryObjects as Export
import Network.AWS.DataPipeline.V2012_10_29.ReportTaskProgress as Export
import Network.AWS.DataPipeline.V2012_10_29.ReportTaskRunnerHeartbeat as Export
import Network.AWS.DataPipeline.V2012_10_29.SetStatus as Export
import Network.AWS.DataPipeline.V2012_10_29.SetTaskStatus as Export
import Network.AWS.DataPipeline.V2012_10_29.Types as Export
import Network.AWS.DataPipeline.V2012_10_29.ValidatePipelineDefinition as Export
