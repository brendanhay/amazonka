{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.CountClosedWorkflowExecutions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--     -   @tagFilter.tag@: String constraint. The key is
--         @swf:tagFilter.tag@.
--     -   @typeFilter.name@: String constraint. The key is
--         @swf:typeFilter.name@.
--     -   @typeFilter.version@: String constraint. The key is
--         @swf:typeFilter.version@.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_CountClosedWorkflowExecutions.html>
module Network.AWS.SWF.CountClosedWorkflowExecutions
    (
    -- * Request
      CountClosedWorkflowExecutions
    -- ** Request constructor
    , countClosedWorkflowExecutions
    -- ** Request lenses
    , ccwerqCloseStatusFilter
    , ccwerqExecutionFilter
    , ccwerqTypeFilter
    , ccwerqCloseTimeFilter
    , ccwerqTagFilter
    , ccwerqStartTimeFilter
    , ccwerqDomain

    -- * Response
    , WorkflowExecutionCount
    -- ** Response constructor
    , workflowExecutionCount
    -- ** Response lenses
    , ccwersTruncated
    , ccwersCount
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'countClosedWorkflowExecutions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccwerqCloseStatusFilter'
--
-- * 'ccwerqExecutionFilter'
--
-- * 'ccwerqTypeFilter'
--
-- * 'ccwerqCloseTimeFilter'
--
-- * 'ccwerqTagFilter'
--
-- * 'ccwerqStartTimeFilter'
--
-- * 'ccwerqDomain'
data CountClosedWorkflowExecutions = CountClosedWorkflowExecutions'
    { _ccwerqCloseStatusFilter :: !(Maybe CloseStatusFilter)
    , _ccwerqExecutionFilter   :: !(Maybe WorkflowExecutionFilter)
    , _ccwerqTypeFilter        :: !(Maybe WorkflowTypeFilter)
    , _ccwerqCloseTimeFilter   :: !(Maybe ExecutionTimeFilter)
    , _ccwerqTagFilter         :: !(Maybe TagFilter)
    , _ccwerqStartTimeFilter   :: !(Maybe ExecutionTimeFilter)
    , _ccwerqDomain            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CountClosedWorkflowExecutions' smart constructor.
countClosedWorkflowExecutions :: Text -> CountClosedWorkflowExecutions
countClosedWorkflowExecutions pDomain =
    CountClosedWorkflowExecutions'
    { _ccwerqCloseStatusFilter = Nothing
    , _ccwerqExecutionFilter = Nothing
    , _ccwerqTypeFilter = Nothing
    , _ccwerqCloseTimeFilter = Nothing
    , _ccwerqTagFilter = Nothing
    , _ccwerqStartTimeFilter = Nothing
    , _ccwerqDomain = pDomain
    }

-- | If specified, only workflow executions that match this close status are
-- counted. This filter has an affect only if @executionStatus@ is
-- specified as @CLOSED@.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
ccwerqCloseStatusFilter :: Lens' CountClosedWorkflowExecutions (Maybe CloseStatusFilter)
ccwerqCloseStatusFilter = lens _ccwerqCloseStatusFilter (\ s a -> s{_ccwerqCloseStatusFilter = a});

-- | If specified, only workflow executions matching the @WorkflowId@ in the
-- filter are counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
ccwerqExecutionFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowExecutionFilter)
ccwerqExecutionFilter = lens _ccwerqExecutionFilter (\ s a -> s{_ccwerqExecutionFilter = a});

-- | If specified, indicates the type of the workflow executions to be
-- counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
ccwerqTypeFilter :: Lens' CountClosedWorkflowExecutions (Maybe WorkflowTypeFilter)
ccwerqTypeFilter = lens _ccwerqTypeFilter (\ s a -> s{_ccwerqTypeFilter = a});

-- | If specified, only workflow executions that meet the close time criteria
-- of the filter are counted.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
ccwerqCloseTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccwerqCloseTimeFilter = lens _ccwerqCloseTimeFilter (\ s a -> s{_ccwerqCloseTimeFilter = a});

-- | If specified, only executions that have a tag that matches the filter
-- are counted.
--
-- @closeStatusFilter@, @executionFilter@, @typeFilter@ and @tagFilter@ are
-- mutually exclusive. You can specify at most one of these in a request.
ccwerqTagFilter :: Lens' CountClosedWorkflowExecutions (Maybe TagFilter)
ccwerqTagFilter = lens _ccwerqTagFilter (\ s a -> s{_ccwerqTagFilter = a});

-- | If specified, only workflow executions that meet the start time criteria
-- of the filter are counted.
--
-- @startTimeFilter@ and @closeTimeFilter@ are mutually exclusive. You must
-- specify one of these in a request but not both.
ccwerqStartTimeFilter :: Lens' CountClosedWorkflowExecutions (Maybe ExecutionTimeFilter)
ccwerqStartTimeFilter = lens _ccwerqStartTimeFilter (\ s a -> s{_ccwerqStartTimeFilter = a});

-- | The name of the domain containing the workflow executions to count.
ccwerqDomain :: Lens' CountClosedWorkflowExecutions Text
ccwerqDomain = lens _ccwerqDomain (\ s a -> s{_ccwerqDomain = a});

instance AWSRequest CountClosedWorkflowExecutions
         where
        type Sv CountClosedWorkflowExecutions = SWF
        type Rs CountClosedWorkflowExecutions =
             WorkflowExecutionCount
        request = postJSON
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
              ["closeStatusFilter" .= _ccwerqCloseStatusFilter,
               "executionFilter" .= _ccwerqExecutionFilter,
               "typeFilter" .= _ccwerqTypeFilter,
               "closeTimeFilter" .= _ccwerqCloseTimeFilter,
               "tagFilter" .= _ccwerqTagFilter,
               "startTimeFilter" .= _ccwerqStartTimeFilter,
               "domain" .= _ccwerqDomain]

instance ToPath CountClosedWorkflowExecutions where
        toPath = const "/"

instance ToQuery CountClosedWorkflowExecutions where
        toQuery = const mempty
