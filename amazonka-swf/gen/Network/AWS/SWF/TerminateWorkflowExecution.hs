{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a 'WorkflowExecutionTerminated' event and forces closure of the
-- workflow execution identified by the given domain, runId, and workflowId. The
-- child policy, registered with the workflow type or specified when starting
-- this execution, is applied to any open child workflow executions of this
-- workflow execution.
--
-- If the identified workflow execution was in progress, it is terminated
-- immediately.  Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. Use an 'Action' element to allow or deny permission to call
-- this action. You cannot use an IAM policy to constrain this action's
-- parameters.  If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing 'OperationNotPermitted'. For details and example IAM
-- policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_TerminateWorkflowExecution.html>
module Network.AWS.SWF.TerminateWorkflowExecution
    (
    -- * Request
      TerminateWorkflowExecution
    -- ** Request constructor
    , terminateWorkflowExecution
    -- ** Request lenses
    , tweChildPolicy
    , tweDetails
    , tweDomain
    , tweReason
    , tweRunId
    , tweWorkflowId

    -- * Response
    , TerminateWorkflowExecutionResponse
    -- ** Response constructor
    , terminateWorkflowExecutionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data TerminateWorkflowExecution = TerminateWorkflowExecution
    { _tweChildPolicy :: Maybe ChildPolicy
    , _tweDetails     :: Maybe Text
    , _tweDomain      :: Text
    , _tweReason      :: Maybe Text
    , _tweRunId       :: Maybe Text
    , _tweWorkflowId  :: Text
    } deriving (Eq, Show)

-- | 'TerminateWorkflowExecution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tweChildPolicy' @::@ 'Maybe' 'ChildPolicy'
--
-- * 'tweDetails' @::@ 'Maybe' 'Text'
--
-- * 'tweDomain' @::@ 'Text'
--
-- * 'tweReason' @::@ 'Maybe' 'Text'
--
-- * 'tweRunId' @::@ 'Maybe' 'Text'
--
-- * 'tweWorkflowId' @::@ 'Text'
--
terminateWorkflowExecution :: Text -- ^ 'tweDomain'
                           -> Text -- ^ 'tweWorkflowId'
                           -> TerminateWorkflowExecution
terminateWorkflowExecution p1 p2 = TerminateWorkflowExecution
    { _tweDomain      = p1
    , _tweWorkflowId  = p2
    , _tweRunId       = Nothing
    , _tweReason      = Nothing
    , _tweDetails     = Nothing
    , _tweChildPolicy = Nothing
    }

-- | If set, specifies the policy to use for the child workflow executions of the
-- workflow execution being terminated. This policy overrides the child policy
-- specified for the workflow execution at registration time or when starting
-- the execution. The supported child policies are:
--
-- TERMINATE: the child executions will be terminated.  REQUEST_CANCEL: a
-- request to cancel will be attempted for each child execution by recording a 'WorkflowExecutionCancelRequested' event in its history. It is up to the decider to take appropriate actions
-- when it receives an execution history with this event.   ABANDON: no action
-- will be taken. The child executions will continue to run.
tweChildPolicy :: Lens' TerminateWorkflowExecution (Maybe ChildPolicy)
tweChildPolicy = lens _tweChildPolicy (\s a -> s { _tweChildPolicy = a })

-- | Optional details for terminating the workflow execution.
tweDetails :: Lens' TerminateWorkflowExecution (Maybe Text)
tweDetails = lens _tweDetails (\s a -> s { _tweDetails = a })

-- | The domain of the workflow execution to terminate.
tweDomain :: Lens' TerminateWorkflowExecution Text
tweDomain = lens _tweDomain (\s a -> s { _tweDomain = a })

-- | An optional descriptive reason for terminating the workflow execution.
tweReason :: Lens' TerminateWorkflowExecution (Maybe Text)
tweReason = lens _tweReason (\s a -> s { _tweReason = a })

-- | The runId of the workflow execution to terminate.
tweRunId :: Lens' TerminateWorkflowExecution (Maybe Text)
tweRunId = lens _tweRunId (\s a -> s { _tweRunId = a })

-- | The workflowId of the workflow execution to terminate.
tweWorkflowId :: Lens' TerminateWorkflowExecution Text
tweWorkflowId = lens _tweWorkflowId (\s a -> s { _tweWorkflowId = a })

data TerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'TerminateWorkflowExecutionResponse' constructor.
terminateWorkflowExecutionResponse :: TerminateWorkflowExecutionResponse
terminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse

instance ToPath TerminateWorkflowExecution where
    toPath = const "/"

instance ToQuery TerminateWorkflowExecution where
    toQuery = const mempty

instance ToHeaders TerminateWorkflowExecution

instance ToJSON TerminateWorkflowExecution where
    toJSON TerminateWorkflowExecution{..} = object
        [ "domain"      .= _tweDomain
        , "workflowId"  .= _tweWorkflowId
        , "runId"       .= _tweRunId
        , "reason"      .= _tweReason
        , "details"     .= _tweDetails
        , "childPolicy" .= _tweChildPolicy
        ]

instance AWSRequest TerminateWorkflowExecution where
    type Sv TerminateWorkflowExecution = SWF
    type Rs TerminateWorkflowExecution = TerminateWorkflowExecutionResponse

    request  = post "TerminateWorkflowExecution"
    response = nullResponse TerminateWorkflowExecutionResponse
