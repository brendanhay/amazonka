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
-- Module      : Network.AWS.SWF.RequestCancelWorkflowExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionCancelRequested@ event in the currently running workflow execution identified by the given domain, workflowId, and runId. This logically requests the cancellation of the workflow execution as a whole. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.RequestCancelWorkflowExecution
    (
    -- * Creating a Request
      requestCancelWorkflowExecution
    , RequestCancelWorkflowExecution
    -- * Request Lenses
    , rcweRunId
    , rcweDomain
    , rcweWorkflowId

    -- * Destructuring the Response
    , requestCancelWorkflowExecutionResponse
    , RequestCancelWorkflowExecutionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'requestCancelWorkflowExecution' smart constructor.
data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution'
  { _rcweRunId      :: !(Maybe Text)
  , _rcweDomain     :: !Text
  , _rcweWorkflowId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestCancelWorkflowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcweRunId' - The runId of the workflow execution to cancel.
--
-- * 'rcweDomain' - The name of the domain containing the workflow execution to cancel.
--
-- * 'rcweWorkflowId' - The workflowId of the workflow execution to cancel.
requestCancelWorkflowExecution
    :: Text -- ^ 'rcweDomain'
    -> Text -- ^ 'rcweWorkflowId'
    -> RequestCancelWorkflowExecution
requestCancelWorkflowExecution pDomain_ pWorkflowId_ =
  RequestCancelWorkflowExecution'
    { _rcweRunId = Nothing
    , _rcweDomain = pDomain_
    , _rcweWorkflowId = pWorkflowId_
    }


-- | The runId of the workflow execution to cancel.
rcweRunId :: Lens' RequestCancelWorkflowExecution (Maybe Text)
rcweRunId = lens _rcweRunId (\ s a -> s{_rcweRunId = a})

-- | The name of the domain containing the workflow execution to cancel.
rcweDomain :: Lens' RequestCancelWorkflowExecution Text
rcweDomain = lens _rcweDomain (\ s a -> s{_rcweDomain = a})

-- | The workflowId of the workflow execution to cancel.
rcweWorkflowId :: Lens' RequestCancelWorkflowExecution Text
rcweWorkflowId = lens _rcweWorkflowId (\ s a -> s{_rcweWorkflowId = a})

instance AWSRequest RequestCancelWorkflowExecution
         where
        type Rs RequestCancelWorkflowExecution =
             RequestCancelWorkflowExecutionResponse
        request = postJSON swf
        response
          = receiveNull RequestCancelWorkflowExecutionResponse'

instance Hashable RequestCancelWorkflowExecution
         where

instance NFData RequestCancelWorkflowExecution where

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
              (catMaybes
                 [("runId" .=) <$> _rcweRunId,
                  Just ("domain" .= _rcweDomain),
                  Just ("workflowId" .= _rcweWorkflowId)])

instance ToPath RequestCancelWorkflowExecution where
        toPath = const "/"

instance ToQuery RequestCancelWorkflowExecution where
        toQuery = const mempty

-- | /See:/ 'requestCancelWorkflowExecutionResponse' smart constructor.
data RequestCancelWorkflowExecutionResponse =
  RequestCancelWorkflowExecutionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestCancelWorkflowExecutionResponse' with the minimum fields required to make a request.
--
requestCancelWorkflowExecutionResponse
    :: RequestCancelWorkflowExecutionResponse
requestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse'


instance NFData
           RequestCancelWorkflowExecutionResponse
         where
