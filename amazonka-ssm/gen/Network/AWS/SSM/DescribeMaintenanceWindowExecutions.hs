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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowExecutions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the executions of a Maintenance Window. This includes information about when the Maintenance Window was scheduled to be active, and information about tasks registered and run with the Maintenance Window.
--
--
module Network.AWS.SSM.DescribeMaintenanceWindowExecutions
    (
    -- * Creating a Request
      describeMaintenanceWindowExecutions
    , DescribeMaintenanceWindowExecutions
    -- * Request Lenses
    , dmweFilters
    , dmweNextToken
    , dmweMaxResults
    , dmweWindowId

    -- * Destructuring the Response
    , describeMaintenanceWindowExecutionsResponse
    , DescribeMaintenanceWindowExecutionsResponse
    -- * Response Lenses
    , dmwersWindowExecutions
    , dmwersNextToken
    , dmwersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeMaintenanceWindowExecutions' smart constructor.
data DescribeMaintenanceWindowExecutions = DescribeMaintenanceWindowExecutions'
  { _dmweFilters    :: !(Maybe [MaintenanceWindowFilter])
  , _dmweNextToken  :: !(Maybe Text)
  , _dmweMaxResults :: !(Maybe Nat)
  , _dmweWindowId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowExecutions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmweFilters' - Each entry in the array is a structure containing: Key (string, between 1 and 128 characters) Values (array of strings, each string is between 1 and 256 characters) The supported Keys are ExecutedBefore and ExecutedAfter with the value being a date/time string such as 2016-11-04T05:00:00Z.
--
-- * 'dmweNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmweMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dmweWindowId' - The ID of the Maintenance Window whose executions should be retrieved.
describeMaintenanceWindowExecutions
    :: Text -- ^ 'dmweWindowId'
    -> DescribeMaintenanceWindowExecutions
describeMaintenanceWindowExecutions pWindowId_ =
  DescribeMaintenanceWindowExecutions'
    { _dmweFilters = Nothing
    , _dmweNextToken = Nothing
    , _dmweMaxResults = Nothing
    , _dmweWindowId = pWindowId_
    }


-- | Each entry in the array is a structure containing: Key (string, between 1 and 128 characters) Values (array of strings, each string is between 1 and 256 characters) The supported Keys are ExecutedBefore and ExecutedAfter with the value being a date/time string such as 2016-11-04T05:00:00Z.
dmweFilters :: Lens' DescribeMaintenanceWindowExecutions [MaintenanceWindowFilter]
dmweFilters = lens _dmweFilters (\ s a -> s{_dmweFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmweNextToken :: Lens' DescribeMaintenanceWindowExecutions (Maybe Text)
dmweNextToken = lens _dmweNextToken (\ s a -> s{_dmweNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmweMaxResults :: Lens' DescribeMaintenanceWindowExecutions (Maybe Natural)
dmweMaxResults = lens _dmweMaxResults (\ s a -> s{_dmweMaxResults = a}) . mapping _Nat

-- | The ID of the Maintenance Window whose executions should be retrieved.
dmweWindowId :: Lens' DescribeMaintenanceWindowExecutions Text
dmweWindowId = lens _dmweWindowId (\ s a -> s{_dmweWindowId = a})

instance AWSRequest
           DescribeMaintenanceWindowExecutions
         where
        type Rs DescribeMaintenanceWindowExecutions =
             DescribeMaintenanceWindowExecutionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowExecutionsResponse' <$>
                   (x .?> "WindowExecutions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMaintenanceWindowExecutions
         where

instance NFData DescribeMaintenanceWindowExecutions
         where

instance ToHeaders
           DescribeMaintenanceWindowExecutions
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeMaintenanceWindowExecutions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMaintenanceWindowExecutions
         where
        toJSON DescribeMaintenanceWindowExecutions'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dmweFilters,
                  ("NextToken" .=) <$> _dmweNextToken,
                  ("MaxResults" .=) <$> _dmweMaxResults,
                  Just ("WindowId" .= _dmweWindowId)])

instance ToPath DescribeMaintenanceWindowExecutions
         where
        toPath = const "/"

instance ToQuery DescribeMaintenanceWindowExecutions
         where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowExecutionsResponse' smart constructor.
data DescribeMaintenanceWindowExecutionsResponse = DescribeMaintenanceWindowExecutionsResponse'
  { _dmwersWindowExecutions :: !(Maybe [MaintenanceWindowExecution])
  , _dmwersNextToken        :: !(Maybe Text)
  , _dmwersResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowExecutionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwersWindowExecutions' - Information about the Maintenance Windows execution.
--
-- * 'dmwersNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dmwersResponseStatus' - -- | The response status code.
describeMaintenanceWindowExecutionsResponse
    :: Int -- ^ 'dmwersResponseStatus'
    -> DescribeMaintenanceWindowExecutionsResponse
describeMaintenanceWindowExecutionsResponse pResponseStatus_ =
  DescribeMaintenanceWindowExecutionsResponse'
    { _dmwersWindowExecutions = Nothing
    , _dmwersNextToken = Nothing
    , _dmwersResponseStatus = pResponseStatus_
    }


-- | Information about the Maintenance Windows execution.
dmwersWindowExecutions :: Lens' DescribeMaintenanceWindowExecutionsResponse [MaintenanceWindowExecution]
dmwersWindowExecutions = lens _dmwersWindowExecutions (\ s a -> s{_dmwersWindowExecutions = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dmwersNextToken :: Lens' DescribeMaintenanceWindowExecutionsResponse (Maybe Text)
dmwersNextToken = lens _dmwersNextToken (\ s a -> s{_dmwersNextToken = a})

-- | -- | The response status code.
dmwersResponseStatus :: Lens' DescribeMaintenanceWindowExecutionsResponse Int
dmwersResponseStatus = lens _dmwersResponseStatus (\ s a -> s{_dmwersResponseStatus = a})

instance NFData
           DescribeMaintenanceWindowExecutionsResponse
         where
