{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.RequestCancelWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a WorkflowExecutionCancelRequested event in the currently running
-- workflow execution identified by the given domain, workflowId, and runId.
-- This logically requests the cancellation of the workflow execution as a
-- whole. It is up to the decider to take appropriate actions when it receives
-- an execution history with this event. Access Control You can use IAM
-- policies to control this action's access to Amazon SWF resources as
-- follows: Use a Resource element with the domain name to limit the action to
-- only specified domains. Use an Action element to allow or deny permission
-- to call this action. You cannot use an IAM policy to constrain this
-- action's parameters. If the caller does not have sufficient permissions to
-- invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails by throwing OperationNotPermitted. For
-- details and example IAM policies, see Using IAM to Manage Access to Amazon
-- SWF Workflows.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_RequestCancelWorkflowExecution.html>
module Network.AWS.SWF.RequestCancelWorkflowExecution
    (
    -- * Request
      RequestCancelWorkflowExecution
    -- ** Request constructor
    , requestCancelWorkflowExecution
    -- ** Request lenses
    , rcweDomain
    , rcweRunId
    , rcweWorkflowId

    -- * Response
    , RequestCancelWorkflowExecutionResponse
    -- ** Response constructor
    , requestCancelWorkflowExecutionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution
    { _rcweDomain     :: Text
    , _rcweRunId      :: Maybe Text
    , _rcweWorkflowId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RequestCancelWorkflowExecution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcweDomain' @::@ 'Text'
--
-- * 'rcweRunId' @::@ 'Maybe' 'Text'
--
-- * 'rcweWorkflowId' @::@ 'Text'
--
requestCancelWorkflowExecution :: Text -- ^ 'rcweDomain'
                               -> Text -- ^ 'rcweWorkflowId'
                               -> RequestCancelWorkflowExecution
requestCancelWorkflowExecution p1 p2 = RequestCancelWorkflowExecution
    { _rcweDomain     = p1
    , _rcweWorkflowId = p2
    , _rcweRunId      = Nothing
    }

-- | The name of the domain containing the workflow execution to cancel.
rcweDomain :: Lens' RequestCancelWorkflowExecution Text
rcweDomain = lens _rcweDomain (\s a -> s { _rcweDomain = a })

-- | The runId of the workflow execution to cancel.
rcweRunId :: Lens' RequestCancelWorkflowExecution (Maybe Text)
rcweRunId = lens _rcweRunId (\s a -> s { _rcweRunId = a })

-- | The workflowId of the workflow execution to cancel.
rcweWorkflowId :: Lens' RequestCancelWorkflowExecution Text
rcweWorkflowId = lens _rcweWorkflowId (\s a -> s { _rcweWorkflowId = a })

data RequestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RequestCancelWorkflowExecutionResponse' constructor.
requestCancelWorkflowExecutionResponse :: RequestCancelWorkflowExecutionResponse
requestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse

instance AWSRequest RequestCancelWorkflowExecution where
    type Sv RequestCancelWorkflowExecution = SWF
    type Rs RequestCancelWorkflowExecution = RequestCancelWorkflowExecutionResponse

    request  = post
    response = nullResponse RequestCancelWorkflowExecutionResponse

instance ToPath RequestCancelWorkflowExecution where
    toPath = const "/"

instance ToHeaders RequestCancelWorkflowExecution

instance ToQuery RequestCancelWorkflowExecution where
    toQuery = const mempty

instance ToJSON RequestCancelWorkflowExecution where
    toJSON = genericToJSON jsonOptions
