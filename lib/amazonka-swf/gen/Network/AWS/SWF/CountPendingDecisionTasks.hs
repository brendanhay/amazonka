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
-- Module      : Network.AWS.SWF.CountPendingDecisionTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated number of decision tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no decision task was ever scheduled in then @0@ is returned.
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
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.CountPendingDecisionTasks
    (
    -- * Creating a Request
      countPendingDecisionTasks
    , CountPendingDecisionTasks
    -- * Request Lenses
    , cpdtDomain
    , cpdtTaskList

    -- * Destructuring the Response
    , pendingTaskCount
    , PendingTaskCount
    -- * Response Lenses
    , ptcTruncated
    , ptcCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'countPendingDecisionTasks' smart constructor.
data CountPendingDecisionTasks = CountPendingDecisionTasks'
  { _cpdtDomain   :: !Text
  , _cpdtTaskList :: !TaskList
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CountPendingDecisionTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpdtDomain' - The name of the domain that contains the task list.
--
-- * 'cpdtTaskList' - The name of the task list.
countPendingDecisionTasks
    :: Text -- ^ 'cpdtDomain'
    -> TaskList -- ^ 'cpdtTaskList'
    -> CountPendingDecisionTasks
countPendingDecisionTasks pDomain_ pTaskList_ =
  CountPendingDecisionTasks'
    {_cpdtDomain = pDomain_, _cpdtTaskList = pTaskList_}


-- | The name of the domain that contains the task list.
cpdtDomain :: Lens' CountPendingDecisionTasks Text
cpdtDomain = lens _cpdtDomain (\ s a -> s{_cpdtDomain = a})

-- | The name of the task list.
cpdtTaskList :: Lens' CountPendingDecisionTasks TaskList
cpdtTaskList = lens _cpdtTaskList (\ s a -> s{_cpdtTaskList = a})

instance AWSRequest CountPendingDecisionTasks where
        type Rs CountPendingDecisionTasks = PendingTaskCount
        request = postJSON swf
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CountPendingDecisionTasks where

instance NFData CountPendingDecisionTasks where

instance ToHeaders CountPendingDecisionTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.CountPendingDecisionTasks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CountPendingDecisionTasks where
        toJSON CountPendingDecisionTasks'{..}
          = object
              (catMaybes
                 [Just ("domain" .= _cpdtDomain),
                  Just ("taskList" .= _cpdtTaskList)])

instance ToPath CountPendingDecisionTasks where
        toPath = const "/"

instance ToQuery CountPendingDecisionTasks where
        toQuery = const mempty
