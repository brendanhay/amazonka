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
-- Module      : Network.AWS.SWF.CountPendingActivityTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated number of activity tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no activity task was ever scheduled in then @0@ is returned.
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
module Network.AWS.SWF.CountPendingActivityTasks
    (
    -- * Creating a Request
      countPendingActivityTasks
    , CountPendingActivityTasks
    -- * Request Lenses
    , cpatDomain
    , cpatTaskList

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

-- | /See:/ 'countPendingActivityTasks' smart constructor.
data CountPendingActivityTasks = CountPendingActivityTasks'
  { _cpatDomain   :: !Text
  , _cpatTaskList :: !TaskList
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CountPendingActivityTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpatDomain' - The name of the domain that contains the task list.
--
-- * 'cpatTaskList' - The name of the task list.
countPendingActivityTasks
    :: Text -- ^ 'cpatDomain'
    -> TaskList -- ^ 'cpatTaskList'
    -> CountPendingActivityTasks
countPendingActivityTasks pDomain_ pTaskList_ =
  CountPendingActivityTasks'
    {_cpatDomain = pDomain_, _cpatTaskList = pTaskList_}


-- | The name of the domain that contains the task list.
cpatDomain :: Lens' CountPendingActivityTasks Text
cpatDomain = lens _cpatDomain (\ s a -> s{_cpatDomain = a})

-- | The name of the task list.
cpatTaskList :: Lens' CountPendingActivityTasks TaskList
cpatTaskList = lens _cpatTaskList (\ s a -> s{_cpatTaskList = a})

instance AWSRequest CountPendingActivityTasks where
        type Rs CountPendingActivityTasks = PendingTaskCount
        request = postJSON swf
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CountPendingActivityTasks where

instance NFData CountPendingActivityTasks where

instance ToHeaders CountPendingActivityTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.CountPendingActivityTasks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CountPendingActivityTasks where
        toJSON CountPendingActivityTasks'{..}
          = object
              (catMaybes
                 [Just ("domain" .= _cpatDomain),
                  Just ("taskList" .= _cpatTaskList)])

instance ToPath CountPendingActivityTasks where
        toPath = const "/"

instance ToQuery CountPendingActivityTasks where
        toQuery = const mempty
