-- Module      : Network.AWS.SWF
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Simple Workflow Service
--
-- The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
-- applications that use Amazon\'s cloud to coordinate work across
-- distributed components. In Amazon SWF, a /task/ represents a logical
-- unit of work that is performed by a component of your workflow.
-- Coordinating tasks in a workflow involves managing intertask
-- dependencies, scheduling, and concurrency in accordance with the logical
-- flow of the application.
--
-- Amazon SWF gives you full control over implementing tasks and
-- coordinating them without worrying about underlying complexities such as
-- tracking their progress and maintaining their state.
--
-- This documentation serves as reference only. For a broader overview of
-- the Amazon SWF programming model, see the
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/ Amazon SWF Developer Guide>.
module Network.AWS.SWF
    ( module Export
    ) where

import Network.AWS.SWF.CountClosedWorkflowExecutions as Export
import Network.AWS.SWF.CountOpenWorkflowExecutions as Export
import Network.AWS.SWF.CountPendingActivityTasks as Export
import Network.AWS.SWF.CountPendingDecisionTasks as Export
import Network.AWS.SWF.DeprecateActivityType as Export
import Network.AWS.SWF.DeprecateDomain as Export
import Network.AWS.SWF.DeprecateWorkflowType as Export
import Network.AWS.SWF.DescribeActivityType as Export
import Network.AWS.SWF.DescribeDomain as Export
import Network.AWS.SWF.DescribeWorkflowExecution as Export
import Network.AWS.SWF.DescribeWorkflowType as Export
import Network.AWS.SWF.GetWorkflowExecutionHistory as Export
import Network.AWS.SWF.ListActivityTypes as Export
import Network.AWS.SWF.ListClosedWorkflowExecutions as Export
import Network.AWS.SWF.ListDomains as Export
import Network.AWS.SWF.ListOpenWorkflowExecutions as Export
import Network.AWS.SWF.ListWorkflowTypes as Export
import Network.AWS.SWF.PollForActivityTask as Export
import Network.AWS.SWF.PollForDecisionTask as Export
import Network.AWS.SWF.RecordActivityTaskHeartbeat as Export
import Network.AWS.SWF.RegisterActivityType as Export
import Network.AWS.SWF.RegisterDomain as Export
import Network.AWS.SWF.RegisterWorkflowType as Export
import Network.AWS.SWF.RequestCancelWorkflowExecution as Export
import Network.AWS.SWF.RespondActivityTaskCanceled as Export
import Network.AWS.SWF.RespondActivityTaskCompleted as Export
import Network.AWS.SWF.RespondActivityTaskFailed as Export
import Network.AWS.SWF.RespondDecisionTaskCompleted as Export
import Network.AWS.SWF.SignalWorkflowExecution as Export
import Network.AWS.SWF.StartWorkflowExecution as Export
import Network.AWS.SWF.TerminateWorkflowExecution as Export
import Network.AWS.SWF.Types as Export
import Network.AWS.SWF.Waiters as Export
