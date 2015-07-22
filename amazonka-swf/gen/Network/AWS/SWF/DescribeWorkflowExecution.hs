{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.DescribeWorkflowExecution
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified workflow execution including its
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
    , dwerqDomain
    , dwerqExecution

    -- * Response
    , DescribeWorkflowExecutionResponse
    -- ** Response constructor
    , describeWorkflowExecutionResponse
    -- ** Response lenses
    , dwersLatestActivityTaskTimestamp
    , dwersLatestExecutionContext
    , dwersStatus
    , dwersExecutionInfo
    , dwersExecutionConfiguration
    , dwersOpenCounts
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'describeWorkflowExecution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwerqDomain'
--
-- * 'dwerqExecution'
data DescribeWorkflowExecution = DescribeWorkflowExecution'
    { _dwerqDomain    :: !Text
    , _dwerqExecution :: !WorkflowExecution
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkflowExecution' smart constructor.
describeWorkflowExecution :: Text -> WorkflowExecution -> DescribeWorkflowExecution
describeWorkflowExecution pDomain pExecution =
    DescribeWorkflowExecution'
    { _dwerqDomain = pDomain
    , _dwerqExecution = pExecution
    }

-- | The name of the domain containing the workflow execution.
dwerqDomain :: Lens' DescribeWorkflowExecution Text
dwerqDomain = lens _dwerqDomain (\ s a -> s{_dwerqDomain = a});

-- | The workflow execution to describe.
dwerqExecution :: Lens' DescribeWorkflowExecution WorkflowExecution
dwerqExecution = lens _dwerqExecution (\ s a -> s{_dwerqExecution = a});

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
                     <*> (pure (fromEnum s))
                     <*> (x .:> "executionInfo")
                     <*> (x .:> "executionConfiguration")
                     <*> (x .:> "openCounts"))

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
              ["domain" .= _dwerqDomain,
               "execution" .= _dwerqExecution]

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
-- * 'dwersLatestActivityTaskTimestamp'
--
-- * 'dwersLatestExecutionContext'
--
-- * 'dwersStatus'
--
-- * 'dwersExecutionInfo'
--
-- * 'dwersExecutionConfiguration'
--
-- * 'dwersOpenCounts'
data DescribeWorkflowExecutionResponse = DescribeWorkflowExecutionResponse'
    { _dwersLatestActivityTaskTimestamp :: !(Maybe POSIX)
    , _dwersLatestExecutionContext      :: !(Maybe Text)
    , _dwersStatus                      :: !Int
    , _dwersExecutionInfo               :: !WorkflowExecutionInfo
    , _dwersExecutionConfiguration      :: !WorkflowExecutionConfiguration
    , _dwersOpenCounts                  :: !WorkflowExecutionOpenCounts
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeWorkflowExecutionResponse' smart constructor.
describeWorkflowExecutionResponse :: Int -> WorkflowExecutionInfo -> WorkflowExecutionConfiguration -> WorkflowExecutionOpenCounts -> DescribeWorkflowExecutionResponse
describeWorkflowExecutionResponse pStatus pExecutionInfo pExecutionConfiguration pOpenCounts =
    DescribeWorkflowExecutionResponse'
    { _dwersLatestActivityTaskTimestamp = Nothing
    , _dwersLatestExecutionContext = Nothing
    , _dwersStatus = pStatus
    , _dwersExecutionInfo = pExecutionInfo
    , _dwersExecutionConfiguration = pExecutionConfiguration
    , _dwersOpenCounts = pOpenCounts
    }

-- | The time when the last activity task was scheduled for this workflow
-- execution. You can use this information to determine if the workflow has
-- not made progress for an unusually long period of time and might require
-- a corrective action.
dwersLatestActivityTaskTimestamp :: Lens' DescribeWorkflowExecutionResponse (Maybe UTCTime)
dwersLatestActivityTaskTimestamp = lens _dwersLatestActivityTaskTimestamp (\ s a -> s{_dwersLatestActivityTaskTimestamp = a}) . mapping _Time;

-- | The latest executionContext provided by the decider for this workflow
-- execution. A decider can provide an executionContext (a free-form
-- string) when closing a decision task using RespondDecisionTaskCompleted.
dwersLatestExecutionContext :: Lens' DescribeWorkflowExecutionResponse (Maybe Text)
dwersLatestExecutionContext = lens _dwersLatestExecutionContext (\ s a -> s{_dwersLatestExecutionContext = a});

-- | FIXME: Undocumented member.
dwersStatus :: Lens' DescribeWorkflowExecutionResponse Int
dwersStatus = lens _dwersStatus (\ s a -> s{_dwersStatus = a});

-- | Information about the workflow execution.
dwersExecutionInfo :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionInfo
dwersExecutionInfo = lens _dwersExecutionInfo (\ s a -> s{_dwersExecutionInfo = a});

-- | The configuration settings for this workflow execution including timeout
-- values, tasklist etc.
dwersExecutionConfiguration :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionConfiguration
dwersExecutionConfiguration = lens _dwersExecutionConfiguration (\ s a -> s{_dwersExecutionConfiguration = a});

-- | The number of tasks for this workflow execution. This includes open and
-- closed tasks of all types.
dwersOpenCounts :: Lens' DescribeWorkflowExecutionResponse WorkflowExecutionOpenCounts
dwersOpenCounts = lens _dwersOpenCounts (\ s a -> s{_dwersOpenCounts = a});
