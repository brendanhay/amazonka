{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a 'WorkflowExecutionTerminated' event and forces closure of the
-- workflow execution identified by the given domain, runId, and
-- workflowId. The child policy, registered with the workflow type or
-- specified when starting this execution, is applied to any open child
-- workflow executions of this workflow execution.
--
-- If the identified workflow execution was in progress, it is terminated
-- immediately.
--
-- If a runId is not specified, then the 'WorkflowExecutionTerminated'
-- event is recorded in the history of the current open workflow with the
-- matching workflowId in the domain.
--
-- You should consider using < RequestCancelWorkflowExecution> action
-- instead because it allows the workflow to gracefully close while
-- < TerminateWorkflowExecution> does not.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a 'Resource' element with the domain name to limit the action to
--     only specified domains.
-- -   Use an 'Action' element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
module Network.AWS.SWF.TerminateWorkflowExecution
    (
    -- * Creating a Request
      terminateWorkflowExecution
    , TerminateWorkflowExecution
    -- * Request Lenses
    , tweReason
    , tweRunId
    , tweChildPolicy
    , tweDetails
    , tweDomain
    , tweWorkflowId

    -- * Destructuring the Response
    , terminateWorkflowExecutionResponse
    , TerminateWorkflowExecutionResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types
import           Network.AWS.SWF.Types.Product

-- | /See:/ 'terminateWorkflowExecution' smart constructor.
data TerminateWorkflowExecution = TerminateWorkflowExecution'
    { _tweReason      :: !(Maybe Text)
    , _tweRunId       :: !(Maybe Text)
    , _tweChildPolicy :: !(Maybe ChildPolicy)
    , _tweDetails     :: !(Maybe Text)
    , _tweDomain      :: !Text
    , _tweWorkflowId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateWorkflowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tweReason'
--
-- * 'tweRunId'
--
-- * 'tweChildPolicy'
--
-- * 'tweDetails'
--
-- * 'tweDomain'
--
-- * 'tweWorkflowId'
terminateWorkflowExecution
    :: Text -- ^ 'tweDomain'
    -> Text -- ^ 'tweWorkflowId'
    -> TerminateWorkflowExecution
terminateWorkflowExecution pDomain_ pWorkflowId_ =
    TerminateWorkflowExecution'
    { _tweReason = Nothing
    , _tweRunId = Nothing
    , _tweChildPolicy = Nothing
    , _tweDetails = Nothing
    , _tweDomain = pDomain_
    , _tweWorkflowId = pWorkflowId_
    }

-- | /Optional./ A descriptive reason for terminating the workflow execution.
tweReason :: Lens' TerminateWorkflowExecution (Maybe Text)
tweReason = lens _tweReason (\ s a -> s{_tweReason = a});

-- | The runId of the workflow execution to terminate.
tweRunId :: Lens' TerminateWorkflowExecution (Maybe Text)
tweRunId = lens _tweRunId (\ s a -> s{_tweRunId = a});

-- | If set, specifies the policy to use for the child workflow executions of
-- the workflow execution being terminated. This policy overrides the child
-- policy specified for the workflow execution at registration time or when
-- starting the execution.
--
-- The supported child policies are:
--
-- -   __TERMINATE:__ the child executions will be terminated.
-- -   __REQUEST_CANCEL:__ a request to cancel will be attempted for each
--     child execution by recording a 'WorkflowExecutionCancelRequested'
--     event in its history. It is up to the decider to take appropriate
--     actions when it receives an execution history with this event.
-- -   __ABANDON:__ no action will be taken. The child executions will
--     continue to run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault will be returned.
tweChildPolicy :: Lens' TerminateWorkflowExecution (Maybe ChildPolicy)
tweChildPolicy = lens _tweChildPolicy (\ s a -> s{_tweChildPolicy = a});

-- | /Optional./ Details for terminating the workflow execution.
tweDetails :: Lens' TerminateWorkflowExecution (Maybe Text)
tweDetails = lens _tweDetails (\ s a -> s{_tweDetails = a});

-- | The domain of the workflow execution to terminate.
tweDomain :: Lens' TerminateWorkflowExecution Text
tweDomain = lens _tweDomain (\ s a -> s{_tweDomain = a});

-- | The workflowId of the workflow execution to terminate.
tweWorkflowId :: Lens' TerminateWorkflowExecution Text
tweWorkflowId = lens _tweWorkflowId (\ s a -> s{_tweWorkflowId = a});

instance AWSRequest TerminateWorkflowExecution where
        type Rs TerminateWorkflowExecution =
             TerminateWorkflowExecutionResponse
        request = postJSON sWF
        response
          = receiveNull TerminateWorkflowExecutionResponse'

instance Hashable TerminateWorkflowExecution

instance ToHeaders TerminateWorkflowExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.TerminateWorkflowExecution"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON TerminateWorkflowExecution where
        toJSON TerminateWorkflowExecution'{..}
          = object
              (catMaybes
                 [("reason" .=) <$> _tweReason,
                  ("runId" .=) <$> _tweRunId,
                  ("childPolicy" .=) <$> _tweChildPolicy,
                  ("details" .=) <$> _tweDetails,
                  Just ("domain" .= _tweDomain),
                  Just ("workflowId" .= _tweWorkflowId)])

instance ToPath TerminateWorkflowExecution where
        toPath = const "/"

instance ToQuery TerminateWorkflowExecution where
        toQuery = const mempty

-- | /See:/ 'terminateWorkflowExecutionResponse' smart constructor.
data TerminateWorkflowExecutionResponse =
    TerminateWorkflowExecutionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateWorkflowExecutionResponse' with the minimum fields required to make a request.
--
terminateWorkflowExecutionResponse
    :: TerminateWorkflowExecutionResponse
terminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse'
