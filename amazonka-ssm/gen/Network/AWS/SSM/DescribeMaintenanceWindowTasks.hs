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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tasks in a Maintenance Window.
--
--
module Network.AWS.SSM.DescribeMaintenanceWindowTasks
    (
    -- * Creating a Request
      describeMaintenanceWindowTasks
    , DescribeMaintenanceWindowTasks
    -- * Request Lenses
    , dFilters
    , dNextToken
    , dMaxResults
    , dWindowId

    -- * Destructuring the Response
    , describeMaintenanceWindowTasksResponse
    , DescribeMaintenanceWindowTasksResponse
    -- * Response Lenses
    , dmwtsrsTasks
    , dmwtsrsNextToken
    , dmwtsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeMaintenanceWindowTasks' smart constructor.
data DescribeMaintenanceWindowTasks = DescribeMaintenanceWindowTasks'
  { _dFilters    :: !(Maybe [MaintenanceWindowFilter])
  , _dNextToken  :: !(Maybe Text)
  , _dMaxResults :: !(Maybe Nat)
  , _dWindowId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFilters' - Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
--
-- * 'dNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dWindowId' - The ID of the Maintenance Window whose tasks should be retrieved.
describeMaintenanceWindowTasks
    :: Text -- ^ 'dWindowId'
    -> DescribeMaintenanceWindowTasks
describeMaintenanceWindowTasks pWindowId_ =
  DescribeMaintenanceWindowTasks'
    { _dFilters = Nothing
    , _dNextToken = Nothing
    , _dMaxResults = Nothing
    , _dWindowId = pWindowId_
    }


-- | Optional filters used to narrow down the scope of the returned tasks. The supported filter keys are WindowTaskId, TaskArn, Priority, and TaskType.
dFilters :: Lens' DescribeMaintenanceWindowTasks [MaintenanceWindowFilter]
dFilters = lens _dFilters (\ s a -> s{_dFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dNextToken :: Lens' DescribeMaintenanceWindowTasks (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dMaxResults :: Lens' DescribeMaintenanceWindowTasks (Maybe Natural)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a}) . mapping _Nat

-- | The ID of the Maintenance Window whose tasks should be retrieved.
dWindowId :: Lens' DescribeMaintenanceWindowTasks Text
dWindowId = lens _dWindowId (\ s a -> s{_dWindowId = a})

instance AWSRequest DescribeMaintenanceWindowTasks
         where
        type Rs DescribeMaintenanceWindowTasks =
             DescribeMaintenanceWindowTasksResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowTasksResponse' <$>
                   (x .?> "Tasks" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMaintenanceWindowTasks
         where

instance NFData DescribeMaintenanceWindowTasks where

instance ToHeaders DescribeMaintenanceWindowTasks
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeMaintenanceWindowTasks" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMaintenanceWindowTasks where
        toJSON DescribeMaintenanceWindowTasks'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dFilters,
                  ("NextToken" .=) <$> _dNextToken,
                  ("MaxResults" .=) <$> _dMaxResults,
                  Just ("WindowId" .= _dWindowId)])

instance ToPath DescribeMaintenanceWindowTasks where
        toPath = const "/"

instance ToQuery DescribeMaintenanceWindowTasks where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowTasksResponse' smart constructor.
data DescribeMaintenanceWindowTasksResponse = DescribeMaintenanceWindowTasksResponse'
  { _dmwtsrsTasks          :: !(Maybe [MaintenanceWindowTask])
  , _dmwtsrsNextToken      :: !(Maybe Text)
  , _dmwtsrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwtsrsTasks' - Information about the tasks in the Maintenance Window.
--
-- * 'dmwtsrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dmwtsrsResponseStatus' - -- | The response status code.
describeMaintenanceWindowTasksResponse
    :: Int -- ^ 'dmwtsrsResponseStatus'
    -> DescribeMaintenanceWindowTasksResponse
describeMaintenanceWindowTasksResponse pResponseStatus_ =
  DescribeMaintenanceWindowTasksResponse'
    { _dmwtsrsTasks = Nothing
    , _dmwtsrsNextToken = Nothing
    , _dmwtsrsResponseStatus = pResponseStatus_
    }


-- | Information about the tasks in the Maintenance Window.
dmwtsrsTasks :: Lens' DescribeMaintenanceWindowTasksResponse [MaintenanceWindowTask]
dmwtsrsTasks = lens _dmwtsrsTasks (\ s a -> s{_dmwtsrsTasks = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dmwtsrsNextToken :: Lens' DescribeMaintenanceWindowTasksResponse (Maybe Text)
dmwtsrsNextToken = lens _dmwtsrsNextToken (\ s a -> s{_dmwtsrsNextToken = a})

-- | -- | The response status code.
dmwtsrsResponseStatus :: Lens' DescribeMaintenanceWindowTasksResponse Int
dmwtsrsResponseStatus = lens _dmwtsrsResponseStatus (\ s a -> s{_dmwtsrsResponseStatus = a})

instance NFData
           DescribeMaintenanceWindowTasksResponse
         where
