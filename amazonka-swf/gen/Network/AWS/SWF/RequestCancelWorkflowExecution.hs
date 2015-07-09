{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RequestCancelWorkflowExecution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Records a @WorkflowExecutionCancelRequested@ event in the currently
-- running workflow execution identified by the given domain, workflowId,
-- and runId. This logically requests the cancellation of the workflow
-- execution as a whole. It is up to the decider to take appropriate
-- actions when it receives an execution history with this event.
--
-- If the runId is not specified, the @WorkflowExecutionCancelRequested@
-- event is recorded in the history of the current open workflow execution
-- with the specified workflowId in the domain.
--
-- Because this action allows the workflow to properly clean up and
-- gracefully close, it should be used instead of
-- TerminateWorkflowExecution when possible.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RequestCancelWorkflowExecution.html>
module Network.AWS.SWF.RequestCancelWorkflowExecution
    (
    -- * Request
      RequestCancelWorkflowExecution
    -- ** Request constructor
    , requestCancelWorkflowExecution
    -- ** Request lenses
    , rcweRunId
    , rcweDomain
    , rcweWorkflowId

    -- * Response
    , RequestCancelWorkflowExecutionResponse
    -- ** Response constructor
    , requestCancelWorkflowExecutionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'requestCancelWorkflowExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcweRunId'
--
-- * 'rcweDomain'
--
-- * 'rcweWorkflowId'
data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution'
    { _rcweRunId      :: !(Maybe Text)
    , _rcweDomain     :: !Text
    , _rcweWorkflowId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestCancelWorkflowExecution' smart constructor.
requestCancelWorkflowExecution :: Text -> Text -> RequestCancelWorkflowExecution
requestCancelWorkflowExecution pDomain pWorkflowId =
    RequestCancelWorkflowExecution'
    { _rcweRunId = Nothing
    , _rcweDomain = pDomain
    , _rcweWorkflowId = pWorkflowId
    }

-- | The runId of the workflow execution to cancel.
rcweRunId :: Lens' RequestCancelWorkflowExecution (Maybe Text)
rcweRunId = lens _rcweRunId (\ s a -> s{_rcweRunId = a});

-- | The name of the domain containing the workflow execution to cancel.
rcweDomain :: Lens' RequestCancelWorkflowExecution Text
rcweDomain = lens _rcweDomain (\ s a -> s{_rcweDomain = a});

-- | The workflowId of the workflow execution to cancel.
rcweWorkflowId :: Lens' RequestCancelWorkflowExecution Text
rcweWorkflowId = lens _rcweWorkflowId (\ s a -> s{_rcweWorkflowId = a});

instance AWSRequest RequestCancelWorkflowExecution
         where
        type Sv RequestCancelWorkflowExecution = SWF
        type Rs RequestCancelWorkflowExecution =
             RequestCancelWorkflowExecutionResponse
        request = postJSON
        response
          = receiveNull RequestCancelWorkflowExecutionResponse'

instance ToHeaders RequestCancelWorkflowExecution
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RequestCancelWorkflowExecution"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RequestCancelWorkflowExecution where
        toJSON RequestCancelWorkflowExecution'{..}
          = object
              ["runId" .= _rcweRunId, "domain" .= _rcweDomain,
               "workflowId" .= _rcweWorkflowId]

instance ToPath RequestCancelWorkflowExecution where
        toPath = const "/"

instance ToQuery RequestCancelWorkflowExecution where
        toQuery = const mempty

-- | /See:/ 'requestCancelWorkflowExecutionResponse' smart constructor.
data RequestCancelWorkflowExecutionResponse =
    RequestCancelWorkflowExecutionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RequestCancelWorkflowExecutionResponse' smart constructor.
requestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse
requestCancelWorkflowExecutionResponse =
    RequestCancelWorkflowExecutionResponse'
