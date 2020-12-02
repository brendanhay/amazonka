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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the individual task executions (one per target) for a particular task executed as part of a Maintenance Window execution.
--
--
module Network.AWS.SSM.DescribeMaintenanceWindowExecutionTaskInvocations
    (
    -- * Creating a Request
      describeMaintenanceWindowExecutionTaskInvocations
    , DescribeMaintenanceWindowExecutionTaskInvocations
    -- * Request Lenses
    , dmwetiFilters
    , dmwetiNextToken
    , dmwetiMaxResults
    , dmwetiWindowExecutionId
    , dmwetiTaskId

    -- * Destructuring the Response
    , describeMaintenanceWindowExecutionTaskInvocationsResponse
    , DescribeMaintenanceWindowExecutionTaskInvocationsResponse
    -- * Response Lenses
    , dmwetirsWindowExecutionTaskInvocationIdentities
    , dmwetirsNextToken
    , dmwetirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeMaintenanceWindowExecutionTaskInvocations' smart constructor.
data DescribeMaintenanceWindowExecutionTaskInvocations = DescribeMaintenanceWindowExecutionTaskInvocations'
  { _dmwetiFilters           :: !(Maybe [MaintenanceWindowFilter])
  , _dmwetiNextToken         :: !(Maybe Text)
  , _dmwetiMaxResults        :: !(Maybe Nat)
  , _dmwetiWindowExecutionId :: !Text
  , _dmwetiTaskId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowExecutionTaskInvocations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwetiFilters' - Optional filters used to scope down the returned task invocations. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
--
-- * 'dmwetiNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmwetiMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dmwetiWindowExecutionId' - The ID of the Maintenance Window execution the task is part of.
--
-- * 'dmwetiTaskId' - The ID of the specific task in the Maintenance Window task that should be retrieved.
describeMaintenanceWindowExecutionTaskInvocations
    :: Text -- ^ 'dmwetiWindowExecutionId'
    -> Text -- ^ 'dmwetiTaskId'
    -> DescribeMaintenanceWindowExecutionTaskInvocations
describeMaintenanceWindowExecutionTaskInvocations pWindowExecutionId_ pTaskId_ =
  DescribeMaintenanceWindowExecutionTaskInvocations'
    { _dmwetiFilters = Nothing
    , _dmwetiNextToken = Nothing
    , _dmwetiMaxResults = Nothing
    , _dmwetiWindowExecutionId = pWindowExecutionId_
    , _dmwetiTaskId = pTaskId_
    }


-- | Optional filters used to scope down the returned task invocations. The supported filter key is STATUS with the corresponding values PENDING, IN_PROGRESS, SUCCESS, FAILED, TIMED_OUT, CANCELLING, and CANCELLED.
dmwetiFilters :: Lens' DescribeMaintenanceWindowExecutionTaskInvocations [MaintenanceWindowFilter]
dmwetiFilters = lens _dmwetiFilters (\ s a -> s{_dmwetiFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmwetiNextToken :: Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Maybe Text)
dmwetiNextToken = lens _dmwetiNextToken (\ s a -> s{_dmwetiNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmwetiMaxResults :: Lens' DescribeMaintenanceWindowExecutionTaskInvocations (Maybe Natural)
dmwetiMaxResults = lens _dmwetiMaxResults (\ s a -> s{_dmwetiMaxResults = a}) . mapping _Nat

-- | The ID of the Maintenance Window execution the task is part of.
dmwetiWindowExecutionId :: Lens' DescribeMaintenanceWindowExecutionTaskInvocations Text
dmwetiWindowExecutionId = lens _dmwetiWindowExecutionId (\ s a -> s{_dmwetiWindowExecutionId = a})

-- | The ID of the specific task in the Maintenance Window task that should be retrieved.
dmwetiTaskId :: Lens' DescribeMaintenanceWindowExecutionTaskInvocations Text
dmwetiTaskId = lens _dmwetiTaskId (\ s a -> s{_dmwetiTaskId = a})

instance AWSRequest
           DescribeMaintenanceWindowExecutionTaskInvocations
         where
        type Rs
               DescribeMaintenanceWindowExecutionTaskInvocations
             =
             DescribeMaintenanceWindowExecutionTaskInvocationsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
                   <$>
                   (x .?> "WindowExecutionTaskInvocationIdentities" .!@
                      mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeMaintenanceWindowExecutionTaskInvocations
         where

instance NFData
           DescribeMaintenanceWindowExecutionTaskInvocations
         where

instance ToHeaders
           DescribeMaintenanceWindowExecutionTaskInvocations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeMaintenanceWindowExecutionTaskInvocations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeMaintenanceWindowExecutionTaskInvocations
         where
        toJSON
          DescribeMaintenanceWindowExecutionTaskInvocations'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dmwetiFilters,
                  ("NextToken" .=) <$> _dmwetiNextToken,
                  ("MaxResults" .=) <$> _dmwetiMaxResults,
                  Just
                    ("WindowExecutionId" .= _dmwetiWindowExecutionId),
                  Just ("TaskId" .= _dmwetiTaskId)])

instance ToPath
           DescribeMaintenanceWindowExecutionTaskInvocations
         where
        toPath = const "/"

instance ToQuery
           DescribeMaintenanceWindowExecutionTaskInvocations
         where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowExecutionTaskInvocationsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionTaskInvocationsResponse = DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
  { _dmwetirsWindowExecutionTaskInvocationIdentities :: !(Maybe [MaintenanceWindowExecutionTaskInvocationIdentity])
  , _dmwetirsNextToken :: !(Maybe Text)
  , _dmwetirsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowExecutionTaskInvocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwetirsWindowExecutionTaskInvocationIdentities' - Information about the task invocation results per invocation.
--
-- * 'dmwetirsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dmwetirsResponseStatus' - -- | The response status code.
describeMaintenanceWindowExecutionTaskInvocationsResponse
    :: Int -- ^ 'dmwetirsResponseStatus'
    -> DescribeMaintenanceWindowExecutionTaskInvocationsResponse
describeMaintenanceWindowExecutionTaskInvocationsResponse pResponseStatus_ =
  DescribeMaintenanceWindowExecutionTaskInvocationsResponse'
    { _dmwetirsWindowExecutionTaskInvocationIdentities = Nothing
    , _dmwetirsNextToken = Nothing
    , _dmwetirsResponseStatus = pResponseStatus_
    }


-- | Information about the task invocation results per invocation.
dmwetirsWindowExecutionTaskInvocationIdentities :: Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse [MaintenanceWindowExecutionTaskInvocationIdentity]
dmwetirsWindowExecutionTaskInvocationIdentities = lens _dmwetirsWindowExecutionTaskInvocationIdentities (\ s a -> s{_dmwetirsWindowExecutionTaskInvocationIdentities = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dmwetirsNextToken :: Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse (Maybe Text)
dmwetirsNextToken = lens _dmwetirsNextToken (\ s a -> s{_dmwetirsNextToken = a})

-- | -- | The response status code.
dmwetirsResponseStatus :: Lens' DescribeMaintenanceWindowExecutionTaskInvocationsResponse Int
dmwetirsResponseStatus = lens _dmwetirsResponseStatus (\ s a -> s{_dmwetirsResponseStatus = a})

instance NFData
           DescribeMaintenanceWindowExecutionTaskInvocationsResponse
         where
