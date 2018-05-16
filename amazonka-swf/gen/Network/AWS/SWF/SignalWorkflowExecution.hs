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
-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionSignaled@ event in the workflow execution history and creates a decision task for the workflow execution identified by the given domain, workflowId and runId. The event is recorded with the specified user defined signalName and input (if provided).
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
module Network.AWS.SWF.SignalWorkflowExecution
    (
    -- * Creating a Request
      signalWorkflowExecution
    , SignalWorkflowExecution
    -- * Request Lenses
    , sweInput
    , sweRunId
    , sweDomain
    , sweWorkflowId
    , sweSignalName

    -- * Destructuring the Response
    , signalWorkflowExecutionResponse
    , SignalWorkflowExecutionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'signalWorkflowExecution' smart constructor.
data SignalWorkflowExecution = SignalWorkflowExecution'
  { _sweInput      :: !(Maybe Text)
  , _sweRunId      :: !(Maybe Text)
  , _sweDomain     :: !Text
  , _sweWorkflowId :: !Text
  , _sweSignalName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SignalWorkflowExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sweInput' - Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
--
-- * 'sweRunId' - The runId of the workflow execution to signal.
--
-- * 'sweDomain' - The name of the domain containing the workflow execution to signal.
--
-- * 'sweWorkflowId' - The workflowId of the workflow execution to signal.
--
-- * 'sweSignalName' - The name of the signal. This name must be meaningful to the target workflow.
signalWorkflowExecution
    :: Text -- ^ 'sweDomain'
    -> Text -- ^ 'sweWorkflowId'
    -> Text -- ^ 'sweSignalName'
    -> SignalWorkflowExecution
signalWorkflowExecution pDomain_ pWorkflowId_ pSignalName_ =
  SignalWorkflowExecution'
    { _sweInput = Nothing
    , _sweRunId = Nothing
    , _sweDomain = pDomain_
    , _sweWorkflowId = pWorkflowId_
    , _sweSignalName = pSignalName_
    }


-- | Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
sweInput :: Lens' SignalWorkflowExecution (Maybe Text)
sweInput = lens _sweInput (\ s a -> s{_sweInput = a})

-- | The runId of the workflow execution to signal.
sweRunId :: Lens' SignalWorkflowExecution (Maybe Text)
sweRunId = lens _sweRunId (\ s a -> s{_sweRunId = a})

-- | The name of the domain containing the workflow execution to signal.
sweDomain :: Lens' SignalWorkflowExecution Text
sweDomain = lens _sweDomain (\ s a -> s{_sweDomain = a})

-- | The workflowId of the workflow execution to signal.
sweWorkflowId :: Lens' SignalWorkflowExecution Text
sweWorkflowId = lens _sweWorkflowId (\ s a -> s{_sweWorkflowId = a})

-- | The name of the signal. This name must be meaningful to the target workflow.
sweSignalName :: Lens' SignalWorkflowExecution Text
sweSignalName = lens _sweSignalName (\ s a -> s{_sweSignalName = a})

instance AWSRequest SignalWorkflowExecution where
        type Rs SignalWorkflowExecution =
             SignalWorkflowExecutionResponse
        request = postJSON swf
        response
          = receiveNull SignalWorkflowExecutionResponse'

instance Hashable SignalWorkflowExecution where

instance NFData SignalWorkflowExecution where

instance ToHeaders SignalWorkflowExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.SignalWorkflowExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON SignalWorkflowExecution where
        toJSON SignalWorkflowExecution'{..}
          = object
              (catMaybes
                 [("input" .=) <$> _sweInput,
                  ("runId" .=) <$> _sweRunId,
                  Just ("domain" .= _sweDomain),
                  Just ("workflowId" .= _sweWorkflowId),
                  Just ("signalName" .= _sweSignalName)])

instance ToPath SignalWorkflowExecution where
        toPath = const "/"

instance ToQuery SignalWorkflowExecution where
        toQuery = const mempty

-- | /See:/ 'signalWorkflowExecutionResponse' smart constructor.
data SignalWorkflowExecutionResponse =
  SignalWorkflowExecutionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SignalWorkflowExecutionResponse' with the minimum fields required to make a request.
--
signalWorkflowExecutionResponse
    :: SignalWorkflowExecutionResponse
signalWorkflowExecutionResponse = SignalWorkflowExecutionResponse'


instance NFData SignalWorkflowExecutionResponse where
