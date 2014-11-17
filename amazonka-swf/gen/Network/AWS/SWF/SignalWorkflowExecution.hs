{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Records a WorkflowExecutionSignaled event in the workflow execution history
-- and creates a decision task for the workflow execution identified by the
-- given domain, workflowId and runId. The event is recorded with the
-- specified user defined signalName and input (if provided). Access Control
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_SignalWorkflowExecution.html>
module Network.AWS.SWF.SignalWorkflowExecution
    (
    -- * Request
      SignalWorkflowExecution
    -- ** Request constructor
    , signalWorkflowExecution
    -- ** Request lenses
    , sweDomain
    , sweInput
    , sweRunId
    , sweSignalName
    , sweWorkflowId

    -- * Response
    , SignalWorkflowExecutionResponse
    -- ** Response constructor
    , signalWorkflowExecutionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data SignalWorkflowExecution = SignalWorkflowExecution
    { _sweDomain     :: Text
    , _sweInput      :: Maybe Text
    , _sweRunId      :: Maybe Text
    , _sweSignalName :: Text
    , _sweWorkflowId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SignalWorkflowExecution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sweDomain' @::@ 'Text'
--
-- * 'sweInput' @::@ 'Maybe' 'Text'
--
-- * 'sweRunId' @::@ 'Maybe' 'Text'
--
-- * 'sweSignalName' @::@ 'Text'
--
-- * 'sweWorkflowId' @::@ 'Text'
--
signalWorkflowExecution :: Text -- ^ 'sweDomain'
                        -> Text -- ^ 'sweWorkflowId'
                        -> Text -- ^ 'sweSignalName'
                        -> SignalWorkflowExecution
signalWorkflowExecution p1 p2 p3 = SignalWorkflowExecution
    { _sweDomain     = p1
    , _sweWorkflowId = p2
    , _sweSignalName = p3
    , _sweRunId      = Nothing
    , _sweInput      = Nothing
    }

-- | The name of the domain containing the workflow execution to signal.
sweDomain :: Lens' SignalWorkflowExecution Text
sweDomain = lens _sweDomain (\s a -> s { _sweDomain = a })

-- | Data to attach to the WorkflowExecutionSignaled event in the target
-- workflow execution's history.
sweInput :: Lens' SignalWorkflowExecution (Maybe Text)
sweInput = lens _sweInput (\s a -> s { _sweInput = a })

-- | The runId of the workflow execution to signal.
sweRunId :: Lens' SignalWorkflowExecution (Maybe Text)
sweRunId = lens _sweRunId (\s a -> s { _sweRunId = a })

-- | The name of the signal. This name must be meaningful to the target
-- workflow.
sweSignalName :: Lens' SignalWorkflowExecution Text
sweSignalName = lens _sweSignalName (\s a -> s { _sweSignalName = a })

-- | The workflowId of the workflow execution to signal.
sweWorkflowId :: Lens' SignalWorkflowExecution Text
sweWorkflowId = lens _sweWorkflowId (\s a -> s { _sweWorkflowId = a })

data SignalWorkflowExecutionResponse = SignalWorkflowExecutionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SignalWorkflowExecutionResponse' constructor.
signalWorkflowExecutionResponse :: SignalWorkflowExecutionResponse
signalWorkflowExecutionResponse = SignalWorkflowExecutionResponse

instance ToPath SignalWorkflowExecution where
    toPath = const "/"

instance ToQuery SignalWorkflowExecution where
    toQuery = const mempty

instance ToHeaders SignalWorkflowExecution
instance ToJSON SignalWorkflowExecution where
    toJSON = genericToJSON jsonOptions

instance AWSRequest SignalWorkflowExecution where
    type Sv SignalWorkflowExecution = SWF
    type Rs SignalWorkflowExecution = SignalWorkflowExecutionResponse

    request  = post
    response = nullResponse SignalWorkflowExecutionResponse
