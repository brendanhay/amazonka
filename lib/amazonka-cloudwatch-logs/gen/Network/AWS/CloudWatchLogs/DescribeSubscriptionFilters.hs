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
-- Module      : Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscription filters for the specified log group. You can list all the subscription filters or filter the results by prefix. The results are ASCII-sorted by filter name.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters
    (
    -- * Creating a Request
      describeSubscriptionFilters
    , DescribeSubscriptionFilters
    -- * Request Lenses
    , dsfFilterNamePrefix
    , dsfNextToken
    , dsfLimit
    , dsfLogGroupName

    -- * Destructuring the Response
    , describeSubscriptionFiltersResponse
    , DescribeSubscriptionFiltersResponse
    -- * Response Lenses
    , dsfrsSubscriptionFilters
    , dsfrsNextToken
    , dsfrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSubscriptionFilters' smart constructor.
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
  { _dsfFilterNamePrefix :: !(Maybe Text)
  , _dsfNextToken        :: !(Maybe Text)
  , _dsfLimit            :: !(Maybe Nat)
  , _dsfLogGroupName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSubscriptionFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfFilterNamePrefix' - The prefix to match. If you don't specify a value, no prefix filter is applied.
--
-- * 'dsfNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dsfLimit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- * 'dsfLogGroupName' - The name of the log group.
describeSubscriptionFilters
    :: Text -- ^ 'dsfLogGroupName'
    -> DescribeSubscriptionFilters
describeSubscriptionFilters pLogGroupName_ =
  DescribeSubscriptionFilters'
    { _dsfFilterNamePrefix = Nothing
    , _dsfNextToken = Nothing
    , _dsfLimit = Nothing
    , _dsfLogGroupName = pLogGroupName_
    }


-- | The prefix to match. If you don't specify a value, no prefix filter is applied.
dsfFilterNamePrefix :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfFilterNamePrefix = lens _dsfFilterNamePrefix (\ s a -> s{_dsfFilterNamePrefix = a})

-- | The token for the next set of items to return. (You received this token from a previous call.)
dsfNextToken :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfNextToken = lens _dsfNextToken (\ s a -> s{_dsfNextToken = a})

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
dsfLimit :: Lens' DescribeSubscriptionFilters (Maybe Natural)
dsfLimit = lens _dsfLimit (\ s a -> s{_dsfLimit = a}) . mapping _Nat

-- | The name of the log group.
dsfLogGroupName :: Lens' DescribeSubscriptionFilters Text
dsfLogGroupName = lens _dsfLogGroupName (\ s a -> s{_dsfLogGroupName = a})

instance AWSPager DescribeSubscriptionFilters where
        page rq rs
          | stop (rs ^. dsfrsNextToken) = Nothing
          | stop (rs ^. dsfrsSubscriptionFilters) = Nothing
          | otherwise =
            Just $ rq & dsfNextToken .~ rs ^. dsfrsNextToken

instance AWSRequest DescribeSubscriptionFilters where
        type Rs DescribeSubscriptionFilters =
             DescribeSubscriptionFiltersResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSubscriptionFiltersResponse' <$>
                   (x .?> "subscriptionFilters" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSubscriptionFilters where

instance NFData DescribeSubscriptionFilters where

instance ToHeaders DescribeSubscriptionFilters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeSubscriptionFilters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSubscriptionFilters where
        toJSON DescribeSubscriptionFilters'{..}
          = object
              (catMaybes
                 [("filterNamePrefix" .=) <$> _dsfFilterNamePrefix,
                  ("nextToken" .=) <$> _dsfNextToken,
                  ("limit" .=) <$> _dsfLimit,
                  Just ("logGroupName" .= _dsfLogGroupName)])

instance ToPath DescribeSubscriptionFilters where
        toPath = const "/"

instance ToQuery DescribeSubscriptionFilters where
        toQuery = const mempty

-- | /See:/ 'describeSubscriptionFiltersResponse' smart constructor.
data DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse'
  { _dsfrsSubscriptionFilters :: !(Maybe [SubscriptionFilter])
  , _dsfrsNextToken           :: !(Maybe Text)
  , _dsfrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSubscriptionFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfrsSubscriptionFilters' - The subscription filters.
--
-- * 'dsfrsNextToken' - Undocumented member.
--
-- * 'dsfrsResponseStatus' - -- | The response status code.
describeSubscriptionFiltersResponse
    :: Int -- ^ 'dsfrsResponseStatus'
    -> DescribeSubscriptionFiltersResponse
describeSubscriptionFiltersResponse pResponseStatus_ =
  DescribeSubscriptionFiltersResponse'
    { _dsfrsSubscriptionFilters = Nothing
    , _dsfrsNextToken = Nothing
    , _dsfrsResponseStatus = pResponseStatus_
    }


-- | The subscription filters.
dsfrsSubscriptionFilters :: Lens' DescribeSubscriptionFiltersResponse [SubscriptionFilter]
dsfrsSubscriptionFilters = lens _dsfrsSubscriptionFilters (\ s a -> s{_dsfrsSubscriptionFilters = a}) . _Default . _Coerce

-- | Undocumented member.
dsfrsNextToken :: Lens' DescribeSubscriptionFiltersResponse (Maybe Text)
dsfrsNextToken = lens _dsfrsNextToken (\ s a -> s{_dsfrsNextToken = a})

-- | -- | The response status code.
dsfrsResponseStatus :: Lens' DescribeSubscriptionFiltersResponse Int
dsfrsResponseStatus = lens _dsfrsResponseStatus (\ s a -> s{_dsfrsResponseStatus = a})

instance NFData DescribeSubscriptionFiltersResponse
         where
