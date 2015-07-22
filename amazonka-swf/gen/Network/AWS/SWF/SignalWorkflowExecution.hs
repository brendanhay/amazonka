{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionSignaled@ event in the workflow execution
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
    , swerqInput
    , swerqRunId
    , swerqDomain
    , swerqWorkflowId
    , swerqSignalName

    -- * Response
    , SignalWorkflowExecutionResponse
    -- ** Response constructor
    , signalWorkflowExecutionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'signalWorkflowExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'swerqInput'
--
-- * 'swerqRunId'
--
-- * 'swerqDomain'
--
-- * 'swerqWorkflowId'
--
-- * 'swerqSignalName'
data SignalWorkflowExecution = SignalWorkflowExecution'
    { _swerqInput      :: !(Maybe Text)
    , _swerqRunId      :: !(Maybe Text)
    , _swerqDomain     :: !Text
    , _swerqWorkflowId :: !Text
    , _swerqSignalName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SignalWorkflowExecution' smart constructor.
signalWorkflowExecution :: Text -> Text -> Text -> SignalWorkflowExecution
signalWorkflowExecution pDomain pWorkflowId pSignalName =
    SignalWorkflowExecution'
    { _swerqInput = Nothing
    , _swerqRunId = Nothing
    , _swerqDomain = pDomain
    , _swerqWorkflowId = pWorkflowId
    , _swerqSignalName = pSignalName
    }

-- | Data to attach to the @WorkflowExecutionSignaled@ event in the target
-- workflow execution\'s history.
swerqInput :: Lens' SignalWorkflowExecution (Maybe Text)
swerqInput = lens _swerqInput (\ s a -> s{_swerqInput = a});

-- | The runId of the workflow execution to signal.
swerqRunId :: Lens' SignalWorkflowExecution (Maybe Text)
swerqRunId = lens _swerqRunId (\ s a -> s{_swerqRunId = a});

-- | The name of the domain containing the workflow execution to signal.
swerqDomain :: Lens' SignalWorkflowExecution Text
swerqDomain = lens _swerqDomain (\ s a -> s{_swerqDomain = a});

-- | The workflowId of the workflow execution to signal.
swerqWorkflowId :: Lens' SignalWorkflowExecution Text
swerqWorkflowId = lens _swerqWorkflowId (\ s a -> s{_swerqWorkflowId = a});

-- | The name of the signal. This name must be meaningful to the target
-- workflow.
swerqSignalName :: Lens' SignalWorkflowExecution Text
swerqSignalName = lens _swerqSignalName (\ s a -> s{_swerqSignalName = a});

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
              ["input" .= _swerqInput, "runId" .= _swerqRunId,
               "domain" .= _swerqDomain,
               "workflowId" .= _swerqWorkflowId,
               "signalName" .= _swerqSignalName]

instance ToPath SignalWorkflowExecution where
        toPath = const "/"

instance ToQuery SignalWorkflowExecution where
        toQuery = const mempty

-- | /See:/ 'signalWorkflowExecutionResponse' smart constructor.
data SignalWorkflowExecutionResponse =
    SignalWorkflowExecutionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SignalWorkflowExecutionResponse' smart constructor.
signalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse
signalWorkflowExecutionResponse = SignalWorkflowExecutionResponse'
