{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified workflow execution including its
-- type and some statistics. Access Control You can use IAM policies to
-- control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. Use an Action element to allow or deny permission to call this
-- action. You cannot use an IAM policy to constrain this action's parameters.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeWorkflowExecution.html>
module Network.AWS.SWF.DescribeWorkflowExecution
    (
    -- * Request
      DescribeWorkflowExecution
    -- ** Request constructor
    , describeWorkflowExecution
    -- ** Request lenses
    , dweDomain
    , dweExecution

    -- * Response
    , DescribeWorkflowExecutionResponse
    -- ** Response constructor
    , describeWorkflowExecutionResponse
    -- ** Response lenses
    , dwerExecutionConfiguration
    , dwerExecutionInfo
    , dwerLatestActivityTaskTimestamp
    , dwerLatestExecutionContext
    , dwerOpenCounts
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data DescribeWorkflowExecution = DescribeWorkflowExecution
    { _dweDomain    :: Text
    , _dweExecution :: WorkflowExecution
    } deriving (Eq, Show, Generic)

-- | 'DescribeWorkflowExecution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dweDomain' @::@ 'Text'
--
-- * 'dweExecution' @::@ 'WorkflowExecution'
--
describeWorkflowExecution :: Text -- ^ 'dweDomain'
                          -> WorkflowExecution -- ^ 'dweExecution'
                          -> DescribeWorkflowExecution
describeWorkflowExecution p1 p2 = DescribeWorkflowExecution
    { _dweDomain    = p1
    , _dweExecution = p2
    }

-- | The name of the domain containing the workflow execution.
dweDomain :: Lens' DescribeWorkflowExecution Text
dweDomain = lens _dweDomain (\s a -> s { _dweDomain = a })

-- | The workflow execution to describe.
dweExecution :: Lens' DescribeWorkflowExecution WorkflowExecution
dweExecution = lens _dweExecution (\s a -> s { _dweExecution = a })

data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse
    { _dwerExecutionConfiguration      :: WorkflowExecutionConfiguration
    , _dwerExecutionInfo               :: WorkflowExecutionInfo
    , _dwerLatestActivityTaskTimestamp :: Maybe RFC822
    , _dwerLatestExecutionContext      :: Maybe Text
    , _dwerOpenCounts                  :: WorkflowExecutionOpenCounts
    } deriving (Eq, Show, Generic)

-- | 'DescribeWorkflowExecutionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwerExecutionConfiguration' @::@ 'WorkflowExecutionConfiguration'
--
-- * 'dwerExecutionInfo' @::@ 'WorkflowExecutionInfo'
--
-- * 'dwerLatestActivityTaskTimestamp' @::@ 'Maybe' 'UTCTime'
--
-- * 'dwerLatestExecutionContext' @::@ 'Maybe' 'Text'
--
-- * 'dwerOpenCounts' @::@ 'WorkflowExecutionOpenCounts'
--
describeWorkflowExecutionResponse :: WorkflowExecutionInfo -- ^ 'dwerExecutionInfo'
                                  -> WorkflowExecutionConfiguration -- ^ 'dwerExecutionConfiguration'
                                  -> WorkflowExecutionOpenCounts -- ^ 'dwerOpenCounts'
                                  -> DescribeWorkflowExecutionResponse
describeWorkflowExecutionResponse p1 p2 p3 = DescribeWorkflowExecutionResponse
    { _dwerExecutionInfo               = p1
    , _dwerExecutionConfiguration      = p2
    , _dwerOpenCounts                  = p3
    , _dwerLatestActivityTaskTimestamp = Nothing
    , _dwerLatestExecutionContext      = Nothing
    }

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
dwerExecutionConfiguration :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionConfiguration
dwerExecutionConfiguration =
    lens _dwerExecutionConfiguration
        (\s a -> s { _dwerExecutionConfiguration = a })

-- | Information about the workflow execution.
dwerExecutionInfo :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionInfo
dwerExecutionInfo =
    lens _dwerExecutionInfo (\s a -> s { _dwerExecutionInfo = a })

-- | The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
dwerLatestActivityTaskTimestamp :: Lens' DescribeWorkflowExecutionResponse (Maybe UTCTime)
dwerLatestActivityTaskTimestamp =
    lens _dwerLatestActivityTaskTimestamp
        (\s a -> s { _dwerLatestActivityTaskTimestamp = a })
            . mapping _Time

-- | The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext, which is a free
-- form string, when closing a decision task using
-- RespondDecisionTaskCompleted.
dwerLatestExecutionContext :: Lens' DescribeWorkflowExecutionResponse (Maybe Text)
dwerLatestExecutionContext =
    lens _dwerLatestExecutionContext
        (\s a -> s { _dwerLatestExecutionContext = a })

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
dwerOpenCounts :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionOpenCounts
dwerOpenCounts = lens _dwerOpenCounts (\s a -> s { _dwerOpenCounts = a })

instance ToPath DescribeWorkflowExecution where
    toPath = const "/"

instance ToQuery DescribeWorkflowExecution where
    toQuery = const mempty

instance ToHeaders DescribeWorkflowExecution

instance ToJSON DescribeWorkflowExecution where
    toJSON DescribeWorkflowExecution{..} = object
        [ "domain"    .= _dweDomain
        , "execution" .= _dweExecution
        ]

instance AWSRequest DescribeWorkflowExecution where
    type Sv DescribeWorkflowExecution = SWF
    type Rs DescribeWorkflowExecution = DescribeWorkflowExecutionResponse

    request  = post "DescribeWorkflowExecution"
    response = jsonResponse

instance FromJSON DescribeWorkflowExecutionResponse where
    parseJSON = withObject "DescribeWorkflowExecutionResponse" $ \o -> DescribeWorkflowExecutionResponse
        <$> o .: "executionConfiguration"
        <*> o .: "executionInfo"
        <*> o .: "latestActivityTaskTimestamp"
        <*> o .: "latestExecutionContext"
        <*> o .: "openCounts"
