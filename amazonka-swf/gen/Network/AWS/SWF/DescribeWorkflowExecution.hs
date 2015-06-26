{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
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

-- | Returns information about the specified workflow execution including its
-- type and some statistics.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
    , dwerLatestActivityTaskTimestamp
    , dwerLatestExecutionContext
    , dwerExecutionInfo
    , dwerExecutionConfiguration
    , dwerOpenCounts
    , dwerStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types

-- | /See:/ 'describeWorkflowExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dweDomain'
--
-- * 'dweExecution'
data DescribeWorkflowExecution = DescribeWorkflowExecution'{_dweDomain :: Text, _dweExecution :: WorkflowExecution} deriving (Eq, Read, Show)

-- | 'DescribeWorkflowExecution' smart constructor.
describeWorkflowExecution :: Text -> WorkflowExecution -> DescribeWorkflowExecution
describeWorkflowExecution pDomain pExecution = DescribeWorkflowExecution'{_dweDomain = pDomain, _dweExecution = pExecution};

-- | The name of the domain containing the workflow execution.
dweDomain :: Lens' DescribeWorkflowExecution Text
dweDomain = lens _dweDomain (\ s a -> s{_dweDomain = a});

-- | The workflow execution to describe.
dweExecution :: Lens' DescribeWorkflowExecution WorkflowExecution
dweExecution = lens _dweExecution (\ s a -> s{_dweExecution = a});

instance AWSRequest DescribeWorkflowExecution where
        type Sv DescribeWorkflowExecution = SWF
        type Rs DescribeWorkflowExecution =
             DescribeWorkflowExecutionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkflowExecutionResponse' <$>
                   (x .?> "latestActivityTaskTimestamp") <*>
                     (x .?> "latestExecutionContext")
                     <*> (x .:> "executionInfo")
                     <*> (x .:> "executionConfiguration")
                     <*> (x .:> "openCounts")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeWorkflowExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DescribeWorkflowExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeWorkflowExecution where
        toJSON DescribeWorkflowExecution'{..}
          = object
              ["domain" .= _dweDomain,
               "execution" .= _dweExecution]

instance ToPath DescribeWorkflowExecution where
        toPath = const "/"

instance ToQuery DescribeWorkflowExecution where
        toQuery = const mempty

-- | Contains details about a workflow execution.
--
-- /See:/ 'describeWorkflowExecutionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwerLatestActivityTaskTimestamp'
--
-- * 'dwerLatestExecutionContext'
--
-- * 'dwerExecutionInfo'
--
-- * 'dwerExecutionConfiguration'
--
-- * 'dwerOpenCounts'
--
-- * 'dwerStatusCode'
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse'{_dwerLatestActivityTaskTimestamp :: Maybe POSIX, _dwerLatestExecutionContext :: Maybe Text, _dwerExecutionInfo :: WorkflowExecutionInfo, _dwerExecutionConfiguration :: WorkflowExecutionConfiguration, _dwerOpenCounts :: WorkflowExecutionOpenCounts, _dwerStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeWorkflowExecutionResponse' smart constructor.
describeWorkflowExecutionResponse :: WorkflowExecutionInfo -> WorkflowExecutionConfiguration -> WorkflowExecutionOpenCounts -> Int -> DescribeWorkflowExecutionResponse
describeWorkflowExecutionResponse pExecutionInfo pExecutionConfiguration pOpenCounts pStatusCode = DescribeWorkflowExecutionResponse'{_dwerLatestActivityTaskTimestamp = Nothing, _dwerLatestExecutionContext = Nothing, _dwerExecutionInfo = pExecutionInfo, _dwerExecutionConfiguration = pExecutionConfiguration, _dwerOpenCounts = pOpenCounts, _dwerStatusCode = pStatusCode};

-- | The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
dwerLatestActivityTaskTimestamp :: Lens' DescribeWorkflowExecutionResponse (Maybe UTCTime)
dwerLatestActivityTaskTimestamp = lens _dwerLatestActivityTaskTimestamp (\ s a -> s{_dwerLatestActivityTaskTimestamp = a}) . mapping _Time;

-- | The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext (a free-form
-- string) when closing a decision task using RespondDecisionTaskCompleted.
dwerLatestExecutionContext :: Lens' DescribeWorkflowExecutionResponse (Maybe Text)
dwerLatestExecutionContext = lens _dwerLatestExecutionContext (\ s a -> s{_dwerLatestExecutionContext = a});

-- | Information about the workflow execution.
dwerExecutionInfo :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionInfo
dwerExecutionInfo = lens _dwerExecutionInfo (\ s a -> s{_dwerExecutionInfo = a});

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
dwerExecutionConfiguration :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionConfiguration
dwerExecutionConfiguration = lens _dwerExecutionConfiguration (\ s a -> s{_dwerExecutionConfiguration = a});

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
dwerOpenCounts :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionOpenCounts
dwerOpenCounts = lens _dwerOpenCounts (\ s a -> s{_dwerOpenCounts = a});

-- | FIXME: Undocumented member.
dwerStatusCode :: Lens' DescribeWorkflowExecutionResponse Int
dwerStatusCode = lens _dwerStatusCode (\ s a -> s{_dwerStatusCode = a});
