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
-- Module      : Network.AWS.SWF.CountOpenWorkflowExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of open workflow executions within the given domain that meet the specified filtering criteria.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @tagFilter.tag@ : String constraint. The key is @swf:tagFilter.tag@ .
--
--     * @typeFilter.name@ : String constraint. The key is @swf:typeFilter.name@ .
--
--     * @typeFilter.version@ : String constraint. The key is @swf:typeFilter.version@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.CountOpenWorkflowExecutions
    (
    -- * Creating a Request
      countOpenWorkflowExecutions
    , CountOpenWorkflowExecutions
    -- * Request Lenses
    , coweExecutionFilter
    , coweTypeFilter
    , coweTagFilter
    , coweDomain
    , coweStartTimeFilter

    -- * Destructuring the Response
    , workflowExecutionCount
    , WorkflowExecutionCount
    -- * Response Lenses
    , wecTruncated
    , wecCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'countOpenWorkflowExecutions' smart constructor.
data CountOpenWorkflowExecutions = CountOpenWorkflowExecutions'
  { _coweExecutionFilter :: !(Maybe WorkflowExecutionFilter)
  , _coweTypeFilter      :: !(Maybe WorkflowTypeFilter)
  , _coweTagFilter       :: !(Maybe TagFilter)
  , _coweDomain          :: !Text
  , _coweStartTimeFilter :: !ExecutionTimeFilter
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CountOpenWorkflowExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coweExecutionFilter' - If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
--
-- * 'coweTypeFilter' - Specifies the type of the workflow executions to be counted.
--
-- * 'coweTagFilter' - If specified, only executions that have a tag that matches the filter are counted.
--
-- * 'coweDomain' - The name of the domain containing the workflow executions to count.
--
-- * 'coweStartTimeFilter' - Specifies the start time criteria that workflow executions must meet in order to be counted.
countOpenWorkflowExecutions
    :: Text -- ^ 'coweDomain'
    -> ExecutionTimeFilter -- ^ 'coweStartTimeFilter'
    -> CountOpenWorkflowExecutions
countOpenWorkflowExecutions pDomain_ pStartTimeFilter_ =
  CountOpenWorkflowExecutions'
    { _coweExecutionFilter = Nothing
    , _coweTypeFilter = Nothing
    , _coweTagFilter = Nothing
    , _coweDomain = pDomain_
    , _coweStartTimeFilter = pStartTimeFilter_
    }


-- | If specified, only workflow executions matching the @WorkflowId@ in the filter are counted.
coweExecutionFilter :: Lens' CountOpenWorkflowExecutions (Maybe WorkflowExecutionFilter)
coweExecutionFilter = lens _coweExecutionFilter (\ s a -> s{_coweExecutionFilter = a})

-- | Specifies the type of the workflow executions to be counted.
coweTypeFilter :: Lens' CountOpenWorkflowExecutions (Maybe WorkflowTypeFilter)
coweTypeFilter = lens _coweTypeFilter (\ s a -> s{_coweTypeFilter = a})

-- | If specified, only executions that have a tag that matches the filter are counted.
coweTagFilter :: Lens' CountOpenWorkflowExecutions (Maybe TagFilter)
coweTagFilter = lens _coweTagFilter (\ s a -> s{_coweTagFilter = a})

-- | The name of the domain containing the workflow executions to count.
coweDomain :: Lens' CountOpenWorkflowExecutions Text
coweDomain = lens _coweDomain (\ s a -> s{_coweDomain = a})

-- | Specifies the start time criteria that workflow executions must meet in order to be counted.
coweStartTimeFilter :: Lens' CountOpenWorkflowExecutions ExecutionTimeFilter
coweStartTimeFilter = lens _coweStartTimeFilter (\ s a -> s{_coweStartTimeFilter = a})

instance AWSRequest CountOpenWorkflowExecutions where
        type Rs CountOpenWorkflowExecutions =
             WorkflowExecutionCount
        request = postJSON swf
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CountOpenWorkflowExecutions where

instance NFData CountOpenWorkflowExecutions where

instance ToHeaders CountOpenWorkflowExecutions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.CountOpenWorkflowExecutions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CountOpenWorkflowExecutions where
        toJSON CountOpenWorkflowExecutions'{..}
          = object
              (catMaybes
                 [("executionFilter" .=) <$> _coweExecutionFilter,
                  ("typeFilter" .=) <$> _coweTypeFilter,
                  ("tagFilter" .=) <$> _coweTagFilter,
                  Just ("domain" .= _coweDomain),
                  Just ("startTimeFilter" .= _coweStartTimeFilter)])

instance ToPath CountOpenWorkflowExecutions where
        toPath = const "/"

instance ToQuery CountOpenWorkflowExecutions where
        toQuery = const mempty
