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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a given Maintenance Window execution, lists the tasks that were executed.
--
--
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTasks
    (
    -- * Creating a Request
      describeMaintenanceWindowExecutionTasks
    , DescribeMaintenanceWindowExecutionTasks
    -- * Request Lenses
    , dmwetFilters
    , dmwetNextToken
    , dmwetMaxResults
    , dmwetWindowExecutionId

    -- * Destructuring the Response
    , describeMaintenanceWindowExecutionTasksResponse
    , DescribeMaintenanceWindowExecutionTasksResponse
    -- * Response Lenses
    , dmwetrsNextToken
    , dmwetrsWindowExecutionTaskIdentities
    , dmwetrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeMaintenanceWindowExecutionTasks' smart constructor.
data DescribeMaintenanceWindowExecutionTasks = DescribeMaintenanceWindowExecutionTasks'
  { _dmwetFilters           :: !(Maybe [MaintenanceWindowFilter])
  , _dmwetNextToken         :: !(Maybe Text)
  , _dmwetMaxResults        :: !(Maybe Nat)
  , _dmwetWindowExecutionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowExecutionTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwetFilters' - Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- * 'dmwetNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmwetMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dmwetWindowExecutionId' - The ID of the Maintenance Window execution whose task executions should be retrieved.
describeMaintenanceWindowExecutionTasks
    :: Text -- ^ 'dmwetWindowExecutionId'
    -> DescribeMaintenanceWindowExecutionTasks
describeMaintenanceWindowExecutionTasks pWindowExecutionId_ =
  DescribeMaintenanceWindowExecutionTasks'
    { _dmwetFilters = Nothing
    , _dmwetNextToken = Nothing
    , _dmwetMaxResults = Nothing
    , _dmwetWindowExecutionId = pWindowExecutionId_
    }


-- | Optional filters used to scope down the returned tasks. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
dmwetFilters :: Lens' DescribeMaintenanceWindowExecutionTasks [MaintenanceWindowFilter]
dmwetFilters = lens _dmwetFilters (\ s a -> s{_dmwetFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmwetNextToken :: Lens' DescribeMaintenanceWindowExecutionTasks (Maybe Text)
dmwetNextToken = lens _dmwetNextToken (\ s a -> s{_dmwetNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmwetMaxResults :: Lens' DescribeMaintenanceWindowExecutionTasks (Maybe Natural)
dmwetMaxResults = lens _dmwetMaxResults (\ s a -> s{_dmwetMaxResults = a}) . mapping _Nat

-- | The ID of the Maintenance Window execution whose task executions should be retrieved.
dmwetWindowExecutionId :: Lens' DescribeMaintenanceWindowExecutionTasks Text
dmwetWindowExecutionId = lens _dmwetWindowExecutionId (\ s a -> s{_dmwetWindowExecutionId = a})

instance AWSRequest
           DescribeMaintenanceWindowExecutionTasks
         where
        type Rs DescribeMaintenanceWindowExecutionTasks =
             DescribeMaintenanceWindowExecutionTasksResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowExecutionTasksResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "WindowExecutionTaskIdentities" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeMaintenanceWindowExecutionTasks
         where

instance NFData
           DescribeMaintenanceWindowExecutionTasks
         where

instance ToHeaders
           DescribeMaintenanceWindowExecutionTasks
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeMaintenanceWindowExecutionTasks"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeMaintenanceWindowExecutionTasks
         where
        toJSON DescribeMaintenanceWindowExecutionTasks'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dmwetFilters,
                  ("NextToken" .=) <$> _dmwetNextToken,
                  ("MaxResults" .=) <$> _dmwetMaxResults,
                  Just
                    ("WindowExecutionId" .= _dmwetWindowExecutionId)])

instance ToPath
           DescribeMaintenanceWindowExecutionTasks
         where
        toPath = const "/"

instance ToQuery
           DescribeMaintenanceWindowExecutionTasks
         where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowExecutionTasksResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTasksResponse = DescribeMaintenanceWindowExecutionTasksResponse'
  { _dmwetrsNextToken :: !(Maybe Text)
  , _dmwetrsWindowExecutionTaskIdentities :: !(Maybe [MaintenanceWindowExecutionTaskIdentity])
  , _dmwetrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowExecutionTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwetrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dmwetrsWindowExecutionTaskIdentities' - Information about the task executions.
--
-- * 'dmwetrsResponseStatus' - -- | The response status code.
describeMaintenanceWindowExecutionTasksResponse
    :: Int -- ^ 'dmwetrsResponseStatus'
    -> DescribeMaintenanceWindowExecutionTasksResponse
describeMaintenanceWindowExecutionTasksResponse pResponseStatus_ =
  DescribeMaintenanceWindowExecutionTasksResponse'
    { _dmwetrsNextToken = Nothing
    , _dmwetrsWindowExecutionTaskIdentities = Nothing
    , _dmwetrsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dmwetrsNextToken :: Lens' DescribeMaintenanceWindowExecutionTasksResponse (Maybe Text)
dmwetrsNextToken = lens _dmwetrsNextToken (\ s a -> s{_dmwetrsNextToken = a})

-- | Information about the task executions.
dmwetrsWindowExecutionTaskIdentities :: Lens' DescribeMaintenanceWindowExecutionTasksResponse [MaintenanceWindowExecutionTaskIdentity]
dmwetrsWindowExecutionTaskIdentities = lens _dmwetrsWindowExecutionTaskIdentities (\ s a -> s{_dmwetrsWindowExecutionTaskIdentities = a}) . _Default . _Coerce

-- | -- | The response status code.
dmwetrsResponseStatus :: Lens' DescribeMaintenanceWindowExecutionTasksResponse Int
dmwetrsResponseStatus = lens _dmwetrsResponseStatus (\ s a -> s{_dmwetrsResponseStatus = a})

instance NFData
           DescribeMaintenanceWindowExecutionTasksResponse
         where
