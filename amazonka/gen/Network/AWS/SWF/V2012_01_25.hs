-- Module      : Network.AWS.SWF.V2012_01_25
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
-- applications that coordinate work across distributed components. In Amazon
-- SWF, a task represents a logical unit of work that is performed by a
-- component of your application. Coordinating tasks across the application
-- involves managing intertask dependencies, scheduling, and concurrency in
-- accordance with the logical flow of the application. Amazon SWF gives you
-- full control over implementing tasks and coordinating them without worrying
-- about underlying complexities such as tracking their progress and
-- maintaining their state.
module Network.AWS.SWF.V2012_01_25 (module Export) where

import Network.AWS.SWF.V2012_01_25.CountClosedWorkflowExecutions as Export
import Network.AWS.SWF.V2012_01_25.CountOpenWorkflowExecutions as Export
import Network.AWS.SWF.V2012_01_25.CountPendingActivityTasks as Export
import Network.AWS.SWF.V2012_01_25.CountPendingDecisionTasks as Export
import Network.AWS.SWF.V2012_01_25.DeprecateActivityType as Export
import Network.AWS.SWF.V2012_01_25.DeprecateDomain as Export
import Network.AWS.SWF.V2012_01_25.DeprecateWorkflowType as Export
import Network.AWS.SWF.V2012_01_25.DescribeActivityType as Export
import Network.AWS.SWF.V2012_01_25.DescribeDomain as Export
import Network.AWS.SWF.V2012_01_25.DescribeWorkflowExecution as Export
import Network.AWS.SWF.V2012_01_25.DescribeWorkflowType as Export
import Network.AWS.SWF.V2012_01_25.GetWorkflowExecutionHistory as Export
import Network.AWS.SWF.V2012_01_25.ListActivityTypes as Export
import Network.AWS.SWF.V2012_01_25.ListClosedWorkflowExecutions as Export
import Network.AWS.SWF.V2012_01_25.ListDomains as Export
import Network.AWS.SWF.V2012_01_25.ListOpenWorkflowExecutions as Export
import Network.AWS.SWF.V2012_01_25.ListWorkflowTypes as Export
import Network.AWS.SWF.V2012_01_25.PollForActivityTask as Export
import Network.AWS.SWF.V2012_01_25.PollForDecisionTask as Export
import Network.AWS.SWF.V2012_01_25.RecordActivityTaskHeartbeat as Export
import Network.AWS.SWF.V2012_01_25.RegisterActivityType as Export
import Network.AWS.SWF.V2012_01_25.RegisterDomain as Export
import Network.AWS.SWF.V2012_01_25.RegisterWorkflowType as Export
import Network.AWS.SWF.V2012_01_25.RequestCancelWorkflowExecution as Export
import Network.AWS.SWF.V2012_01_25.RespondActivityTaskCanceled as Export
import Network.AWS.SWF.V2012_01_25.RespondActivityTaskCompleted as Export
import Network.AWS.SWF.V2012_01_25.RespondActivityTaskFailed as Export
import Network.AWS.SWF.V2012_01_25.RespondDecisionTaskCompleted as Export
import Network.AWS.SWF.V2012_01_25.SignalWorkflowExecution as Export
import Network.AWS.SWF.V2012_01_25.StartWorkflowExecution as Export
import Network.AWS.SWF.V2012_01_25.TerminateWorkflowExecution as Export
import Network.AWS.SWF.V2012_01_25.Types as Export
