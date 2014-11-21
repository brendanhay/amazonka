-- Module      : Network.AWS.SWF
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
module Network.AWS.SWF
    ( module Network.AWS.SWF.CountClosedWorkflowExecutions
    , module Network.AWS.SWF.CountOpenWorkflowExecutions
    , module Network.AWS.SWF.CountPendingActivityTasks
    , module Network.AWS.SWF.CountPendingDecisionTasks
    , module Network.AWS.SWF.DeprecateActivityType
    , module Network.AWS.SWF.DeprecateDomain
    , module Network.AWS.SWF.DeprecateWorkflowType
    , module Network.AWS.SWF.DescribeActivityType
    , module Network.AWS.SWF.DescribeDomain
    , module Network.AWS.SWF.DescribeWorkflowExecution
    , module Network.AWS.SWF.DescribeWorkflowType
    , module Network.AWS.SWF.GetWorkflowExecutionHistory
    , module Network.AWS.SWF.ListActivityTypes
    , module Network.AWS.SWF.ListClosedWorkflowExecutions
    , module Network.AWS.SWF.ListDomains
    , module Network.AWS.SWF.ListOpenWorkflowExecutions
    , module Network.AWS.SWF.ListWorkflowTypes
    , module Network.AWS.SWF.PollForActivityTask
    , module Network.AWS.SWF.PollForDecisionTask
    , module Network.AWS.SWF.RecordActivityTaskHeartbeat
    , module Network.AWS.SWF.RegisterActivityType
    , module Network.AWS.SWF.RegisterDomain
    , module Network.AWS.SWF.RegisterWorkflowType
    , module Network.AWS.SWF.RequestCancelWorkflowExecution
    , module Network.AWS.SWF.RespondActivityTaskCanceled
    , module Network.AWS.SWF.RespondActivityTaskCompleted
    , module Network.AWS.SWF.RespondActivityTaskFailed
    , module Network.AWS.SWF.RespondDecisionTaskCompleted
    , module Network.AWS.SWF.SignalWorkflowExecution
    , module Network.AWS.SWF.StartWorkflowExecution
    , module Network.AWS.SWF.TerminateWorkflowExecution
    , module Network.AWS.SWF.Types
    ) where

import Network.AWS.SWF.CountClosedWorkflowExecutions
import Network.AWS.SWF.CountOpenWorkflowExecutions
import Network.AWS.SWF.CountPendingActivityTasks
import Network.AWS.SWF.CountPendingDecisionTasks
import Network.AWS.SWF.DeprecateActivityType
import Network.AWS.SWF.DeprecateDomain
import Network.AWS.SWF.DeprecateWorkflowType
import Network.AWS.SWF.DescribeActivityType
import Network.AWS.SWF.DescribeDomain
import Network.AWS.SWF.DescribeWorkflowExecution
import Network.AWS.SWF.DescribeWorkflowType
import Network.AWS.SWF.GetWorkflowExecutionHistory
import Network.AWS.SWF.ListActivityTypes
import Network.AWS.SWF.ListClosedWorkflowExecutions
import Network.AWS.SWF.ListDomains
import Network.AWS.SWF.ListOpenWorkflowExecutions
import Network.AWS.SWF.ListWorkflowTypes
import Network.AWS.SWF.PollForActivityTask
import Network.AWS.SWF.PollForDecisionTask
import Network.AWS.SWF.RecordActivityTaskHeartbeat
import Network.AWS.SWF.RegisterActivityType
import Network.AWS.SWF.RegisterDomain
import Network.AWS.SWF.RegisterWorkflowType
import Network.AWS.SWF.RequestCancelWorkflowExecution
import Network.AWS.SWF.RespondActivityTaskCanceled
import Network.AWS.SWF.RespondActivityTaskCompleted
import Network.AWS.SWF.RespondActivityTaskFailed
import Network.AWS.SWF.RespondDecisionTaskCompleted
import Network.AWS.SWF.SignalWorkflowExecution
import Network.AWS.SWF.StartWorkflowExecution
import Network.AWS.SWF.TerminateWorkflowExecution
import Network.AWS.SWF.Types
