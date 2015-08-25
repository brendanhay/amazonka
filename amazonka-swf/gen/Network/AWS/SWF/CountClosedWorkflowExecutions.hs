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
-- Module      : Network.AWS.SWF.CountClosedWorkflowExecutions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of closed workflow executions within the given domain
-- that meet the specified filtering criteria.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a 'Resource' element with the domain name to limit the action to
--     only specified domains.
-- -   Use an 'Action' element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a 'Condition' element
--     with the appropriate keys.
--     -   'tagFilter.tag': String constraint. The key is
--         'swf:tagFilter.tag'.
--     -   'typeFilter.name': String constraint. The key is
--         'swf:typeFilter.name'.
--     -   'typeFilter.version': String constraint. The key is
--         'swf:typeFilter.version'.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- /See:/ <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_CountClosedWorkflowExecutions.html AWS API Reference> for CountClosedWorkflowExecutions.
module Network.AWS.SWF.CountClosedWorkflowExecutions
    (
    -- * Creating a Request
      countClosedWorkflowExecutions
    , CountClosedWorkflowExecutions
    -- * Request Lenses
    , ccweCloseStatusFilter
    , ccweExecutionFilter
    , ccweTypeFilter
    , ccweCloseTimeFilter
    , ccweTagFilter
    , ccweStartTimeFilter
    , ccweDomain

    -- * Destructuring the Response
    , workflowExecutionCount
    , WorkflowExecutionCount
    -- * Response Lenses
    , wecTruncated
    , wecCount
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types
import           Network.AWS.SWF.Types.Product

-- | /See:/ 'countClosedWorkflowExecutions' smart constructor.
data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions'
    { _ccweCloseStatusFilter :: !(Maybe CloseStatusFilter)
    , _ccweExecutionFilter   :: !(Maybe WorkflowExecutionFilter)
    , _ccweTypeFilter        :: !(Maybe WorkflowTypeFilter)
    , _ccweCloseTimeFilter   :: !(Maybe ExecutionTimeFilter)
    , _ccweTagFilter         :: !(Maybe TagFilter)
    , _ccweStartTimeFilter   :: !(Maybe ExecutionTimeFilter)
    , _ccweDomain            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CountClosedWorkflowExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccweCloseStatusFilter'
--
-- * 'ccweExecutionFilter'
--
-- * 'ccweTypeFilter'
--
-- * 'ccweCloseTimeFilter'
--
-- * 'ccweTagFilter'
--
-- * 'ccweStartTimeFilter'
--
-- * 'ccweDomain'
countClosedWorkflowExecutions
    :: Text -- ^ 'ccweDomain'
    -> CountClosedWorkflowExecutions
countClosedWorkflowExecutions pDomain_ =
    CountClosedWorkflowExecutions'
    { _ccweCloseStatusFilter = Nothing
    , _ccweExecutionFilter = Nothing
    , _ccweTypeFilter = Nothing
    , _ccweCloseTimeFilter = Nothing
    , _ccweTagFilter = Nothing
    , _ccweStartTimeFilter = Nothing
    , _ccweDomain = pDomain_
    }

-- | If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if 'executionStatus' is
-- specified as 'CLOSED'.
--
-- 'closeStatusFilter', 'executionFilter', 'typeFilter' and 'tagFilter' are
-- mutually exclusive. You can specify at most one of these in a request.
ccweCloseStatusFilter :: Lens' CountClosedWorkflowExecutions (Maybe CloseStatusFilter)
ccweCloseStatusFilter = lens _ccweCloseStatusFilter (\ s a -> s{_ccweCloseStatusFilter = a});

-- | If specified, only workflow executions matching the 'WorkflowId' in the
-- filter are counted.
--
-- 'closeStatusFilter', 'executionFilter', 'typeFilter' and 'tagFilter' are
-- mutually exclusive. You can specify at most one of these in a request.
ccweExecutionFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowExecutionFilter)
ccweExecutionFilter = lens _ccweExecutionFilter (\ s a -> s{_ccweExecutionFilter = a});

-- | If specified, indicates the type of the workflow executions to be
-- counted.
--
-- 'closeStatusFilter', 'executionFilter', 'typeFilter' and 'tagFilter' are
-- mutually exclusive. You can specify at most one of these in a request.
ccweTypeFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowTypeFilter)
ccweTypeFilter = lens _ccweTypeFilter (\ s a -> s{_ccweTypeFilter = a});

-- | If specified, only workflow executions that meet the close time criteria
-- of the filter are counted.
--
-- 'startTimeFilter' and 'closeTimeFilter' are mutually exclusive. You must
-- specify one of these in a request but not both.
ccweCloseTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccweCloseTimeFilter = lens _ccweCloseTimeFilter (\ s a -> s{_ccweCloseTimeFilter = a});

-- | If specified, only executions that have a tag that matches the filter
-- are counted.
--
-- 'closeStatusFilter', 'executionFilter', 'typeFilter' and 'tagFilter' are
-- mutually exclusive. You can specify at most one of these in a request.
ccweTagFilter :: Lens' CountClosedWorkflowExecutions (Maybe TagFilter)
ccweTagFilter = lens _ccweTagFilter (\ s a -> s{_ccweTagFilter = a});

-- | If specified, only workflow executions that meet the start time criteria
-- of the filter are counted.
--
-- 'startTimeFilter' and 'closeTimeFilter' are mutually exclusive. You must
-- specify one of these in a request but not both.
ccweStartTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccweStartTimeFilter = lens _ccweStartTimeFilter (\ s a -> s{_ccweStartTimeFilter = a});

-- | The name of the domain containing the workflow executions to count.
ccweDomain :: Lens' CountClosedWorkflowExecutions Text
ccweDomain = lens _ccweDomain (\ s a -> s{_ccweDomain = a});

instance AWSRequest CountClosedWorkflowExecutions
         where
        type Rs CountClosedWorkflowExecutions =
             WorkflowExecutionCount
        request = postJSON sWF
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CountClosedWorkflowExecutions
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.CountClosedWorkflowExecutions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CountClosedWorkflowExecutions where
        toJSON CountClosedWorkflowExecutions'{..}
          = object
              (catMaybes
                 [("closeStatusFilter" .=) <$> _ccweCloseStatusFilter,
                  ("executionFilter" .=) <$> _ccweExecutionFilter,
                  ("typeFilter" .=) <$> _ccweTypeFilter,
                  ("closeTimeFilter" .=) <$> _ccweCloseTimeFilter,
                  ("tagFilter" .=) <$> _ccweTagFilter,
                  ("startTimeFilter" .=) <$> _ccweStartTimeFilter,
                  Just ("domain" .= _ccweDomain)])

instance ToPath CountClosedWorkflowExecutions where
        toPath = const "/"

instance ToQuery CountClosedWorkflowExecutions where
        toQuery = const mempty
