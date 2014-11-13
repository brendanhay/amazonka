-- Module      : Network.AWS.SimpleWorkflow
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
module Network.AWS.SimpleWorkflow
    ( module Network.AWS.SimpleWorkflow.CountClosedWorkflowExecutions
    , module Network.AWS.SimpleWorkflow.CountOpenWorkflowExecutions
    , module Network.AWS.SimpleWorkflow.CountPendingActivityTasks
    , module Network.AWS.SimpleWorkflow.CountPendingDecisionTasks
    , module Network.AWS.SimpleWorkflow.DeprecateActivityType
    , module Network.AWS.SimpleWorkflow.DeprecateDomain
    , module Network.AWS.SimpleWorkflow.DeprecateWorkflowType
    , module Network.AWS.SimpleWorkflow.DescribeActivityType
    , module Network.AWS.SimpleWorkflow.DescribeDomain
    , module Network.AWS.SimpleWorkflow.DescribeWorkflowExecution
    , module Network.AWS.SimpleWorkflow.DescribeWorkflowType
    , module Network.AWS.SimpleWorkflow.GetWorkflowExecutionHistory
    , module Network.AWS.SimpleWorkflow.ListActivityTypes
    , module Network.AWS.SimpleWorkflow.ListClosedWorkflowExecutions
    , module Network.AWS.SimpleWorkflow.ListDomains
    , module Network.AWS.SimpleWorkflow.ListOpenWorkflowExecutions
    , module Network.AWS.SimpleWorkflow.ListWorkflowTypes
    , module Network.AWS.SimpleWorkflow.PollForActivityTask
    , module Network.AWS.SimpleWorkflow.PollForDecisionTask
    , module Network.AWS.SimpleWorkflow.RecordActivityTaskHeartbeat
    , module Network.AWS.SimpleWorkflow.RegisterActivityType
    , module Network.AWS.SimpleWorkflow.RegisterDomain
    , module Network.AWS.SimpleWorkflow.RegisterWorkflowType
    , module Network.AWS.SimpleWorkflow.RequestCancelWorkflowExecution
    , module Network.AWS.SimpleWorkflow.RespondActivityTaskCanceled
    , module Network.AWS.SimpleWorkflow.RespondActivityTaskCompleted
    , module Network.AWS.SimpleWorkflow.RespondActivityTaskFailed
    , module Network.AWS.SimpleWorkflow.RespondDecisionTaskCompleted
    , module Network.AWS.SimpleWorkflow.SignalWorkflowExecution
    , module Network.AWS.SimpleWorkflow.StartWorkflowExecution
    , module Network.AWS.SimpleWorkflow.TerminateWorkflowExecution
    , module Network.AWS.SimpleWorkflow.Types
    ) where

import Network.AWS.SimpleWorkflow.CountClosedWorkflowExecutions
import Network.AWS.SimpleWorkflow.CountOpenWorkflowExecutions
import Network.AWS.SimpleWorkflow.CountPendingActivityTasks
import Network.AWS.SimpleWorkflow.CountPendingDecisionTasks
import Network.AWS.SimpleWorkflow.DeprecateActivityType
import Network.AWS.SimpleWorkflow.DeprecateDomain
import Network.AWS.SimpleWorkflow.DeprecateWorkflowType
import Network.AWS.SimpleWorkflow.DescribeActivityType
import Network.AWS.SimpleWorkflow.DescribeDomain
import Network.AWS.SimpleWorkflow.DescribeWorkflowExecution
import Network.AWS.SimpleWorkflow.DescribeWorkflowType
import Network.AWS.SimpleWorkflow.GetWorkflowExecutionHistory
import Network.AWS.SimpleWorkflow.ListActivityTypes
import Network.AWS.SimpleWorkflow.ListClosedWorkflowExecutions
import Network.AWS.SimpleWorkflow.ListDomains
import Network.AWS.SimpleWorkflow.ListOpenWorkflowExecutions
import Network.AWS.SimpleWorkflow.ListWorkflowTypes
import Network.AWS.SimpleWorkflow.PollForActivityTask
import Network.AWS.SimpleWorkflow.PollForDecisionTask
import Network.AWS.SimpleWorkflow.RecordActivityTaskHeartbeat
import Network.AWS.SimpleWorkflow.RegisterActivityType
import Network.AWS.SimpleWorkflow.RegisterDomain
import Network.AWS.SimpleWorkflow.RegisterWorkflowType
import Network.AWS.SimpleWorkflow.RequestCancelWorkflowExecution
import Network.AWS.SimpleWorkflow.RespondActivityTaskCanceled
import Network.AWS.SimpleWorkflow.RespondActivityTaskCompleted
import Network.AWS.SimpleWorkflow.RespondActivityTaskFailed
import Network.AWS.SimpleWorkflow.RespondDecisionTaskCompleted
import Network.AWS.SimpleWorkflow.SignalWorkflowExecution
import Network.AWS.SimpleWorkflow.StartWorkflowExecution
import Network.AWS.SimpleWorkflow.TerminateWorkflowExecution
import Network.AWS.SimpleWorkflow.Types
