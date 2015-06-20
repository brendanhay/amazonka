{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SWF.SignalWorkflowExecution
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

-- | Records a @WorkflowExecutionSignaled@ event in the workflow execution
-- history and creates a decision task for the workflow execution
-- identified by the given domain, workflowId and runId. The event is
-- recorded with the specified user defined signalName and input (if
-- provided).
--
-- If a runId is not specified, then the @WorkflowExecutionSignaled@ event
-- is recorded in the history of the current open workflow with the
-- matching workflowId in the domain.
--
-- If the specified workflow execution is not open, this method fails with
-- @UnknownResource@.
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_SignalWorkflowExecution.html>
module Network.AWS.SWF.SignalWorkflowExecution
    (
    -- * Request
      SignalWorkflowExecution
    -- ** Request constructor
    , signalWorkflowExecution
    -- ** Request lenses
    , sweInput
    , sweRunId
    , sweDomain
    , sweWorkflowId
    , sweSignalName

    -- * Response
    , SignalWorkflowExecutionResponse
    -- ** Response constructor
    , signalWorkflowExecutionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types

-- | /See:/ 'signalWorkflowExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sweInput'
--
-- * 'sweRunId'
--
-- * 'sweDomain'
--
-- * 'sweWorkflowId'
--
-- * 'sweSignalName'
data SignalWorkflowExecution = SignalWorkflowExecution'{_sweInput :: Maybe Text, _sweRunId :: Maybe Text, _sweDomain :: Text, _sweWorkflowId :: Text, _sweSignalName :: Text} deriving (Eq, Read, Show)

-- | 'SignalWorkflowExecution' smart constructor.
signalWorkflowExecution :: Text -> Text -> Text -> SignalWorkflowExecution
signalWorkflowExecution pDomain pWorkflowId pSignalName = SignalWorkflowExecution'{_sweInput = Nothing, _sweRunId = Nothing, _sweDomain = pDomain, _sweWorkflowId = pWorkflowId, _sweSignalName = pSignalName};

-- | Data to attach to the @WorkflowExecutionSignaled@ event in the target
-- workflow execution\'s history.
sweInput :: Lens' SignalWorkflowExecution (Maybe Text)
sweInput = lens _sweInput (\ s a -> s{_sweInput = a});

-- | The runId of the workflow execution to signal.
sweRunId :: Lens' SignalWorkflowExecution (Maybe Text)
sweRunId = lens _sweRunId (\ s a -> s{_sweRunId = a});

-- | The name of the domain containing the workflow execution to signal.
sweDomain :: Lens' SignalWorkflowExecution Text
sweDomain = lens _sweDomain (\ s a -> s{_sweDomain = a});

-- | The workflowId of the workflow execution to signal.
sweWorkflowId :: Lens' SignalWorkflowExecution Text
sweWorkflowId = lens _sweWorkflowId (\ s a -> s{_sweWorkflowId = a});

-- | The name of the signal. This name must be meaningful to the target
-- workflow.
sweSignalName :: Lens' SignalWorkflowExecution Text
sweSignalName = lens _sweSignalName (\ s a -> s{_sweSignalName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest SignalWorkflowExecution where
        type Sv SignalWorkflowExecution = SWF
        type Rs SignalWorkflowExecution =
             SignalWorkflowExecutionResponse
        request = postJSON
        response
          = receiveNull SignalWorkflowExecutionResponse'

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
              ["input" .= _sweInput, "runId" .= _sweRunId,
               "domain" .= _sweDomain,
               "workflowId" .= _sweWorkflowId,
               "signalName" .= _sweSignalName]

instance ToPath SignalWorkflowExecution where
        toPath = const "/"

instance ToQuery SignalWorkflowExecution where
        toQuery = const mempty

-- | /See:/ 'signalWorkflowExecutionResponse' smart constructor.
data SignalWorkflowExecutionResponse = SignalWorkflowExecutionResponse' deriving (Eq, Read, Show)

-- | 'SignalWorkflowExecutionResponse' smart constructor.
signalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse
signalWorkflowExecutionResponse = SignalWorkflowExecutionResponse';
