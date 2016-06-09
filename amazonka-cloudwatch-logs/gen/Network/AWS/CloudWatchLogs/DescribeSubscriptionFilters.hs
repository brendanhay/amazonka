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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the subscription filters associated with the specified log group. The list returned in the response is ASCII-sorted by filter name.
--
-- By default, this operation returns up to 50 subscription filters. If there are more subscription filters to list, the response would contain a 'nextToken' value in the response body. You can also limit the number of subscription filters returned in the response by specifying the 'limit' parameter in the request.
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

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeSubscriptionFilters' smart constructor.
data DescribeSubscriptionFilters = DescribeSubscriptionFilters'
    { _dsfFilterNamePrefix :: !(Maybe Text)
    , _dsfNextToken        :: !(Maybe Text)
    , _dsfLimit            :: !(Maybe Nat)
    , _dsfLogGroupName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSubscriptionFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfFilterNamePrefix'
--
-- * 'dsfNextToken'
--
-- * 'dsfLimit'
--
-- * 'dsfLogGroupName'
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

-- | Will only return subscription filters that match the provided filterNamePrefix. If you don\'t specify a value, no prefix filter is applied.
dsfFilterNamePrefix :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfFilterNamePrefix = lens _dsfFilterNamePrefix (\ s a -> s{_dsfFilterNamePrefix = a});

-- | Undocumented member.
dsfNextToken :: Lens' DescribeSubscriptionFilters (Maybe Text)
dsfNextToken = lens _dsfNextToken (\ s a -> s{_dsfNextToken = a});

-- | Undocumented member.
dsfLimit :: Lens' DescribeSubscriptionFilters (Maybe Natural)
dsfLimit = lens _dsfLimit (\ s a -> s{_dsfLimit = a}) . mapping _Nat;

-- | The log group name for which subscription filters are to be listed.
dsfLogGroupName :: Lens' DescribeSubscriptionFilters Text
dsfLogGroupName = lens _dsfLogGroupName (\ s a -> s{_dsfLogGroupName = a});

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

instance Hashable DescribeSubscriptionFilters

instance NFData DescribeSubscriptionFilters

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSubscriptionFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfrsSubscriptionFilters'
--
-- * 'dsfrsNextToken'
--
-- * 'dsfrsResponseStatus'
describeSubscriptionFiltersResponse
    :: Int -- ^ 'dsfrsResponseStatus'
    -> DescribeSubscriptionFiltersResponse
describeSubscriptionFiltersResponse pResponseStatus_ =
    DescribeSubscriptionFiltersResponse'
    { _dsfrsSubscriptionFilters = Nothing
    , _dsfrsNextToken = Nothing
    , _dsfrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dsfrsSubscriptionFilters :: Lens' DescribeSubscriptionFiltersResponse [SubscriptionFilter]
dsfrsSubscriptionFilters = lens _dsfrsSubscriptionFilters (\ s a -> s{_dsfrsSubscriptionFilters = a}) . _Default . _Coerce;

-- | Undocumented member.
dsfrsNextToken :: Lens' DescribeSubscriptionFiltersResponse (Maybe Text)
dsfrsNextToken = lens _dsfrsNextToken (\ s a -> s{_dsfrsNextToken = a});

-- | The response status code.
dsfrsResponseStatus :: Lens' DescribeSubscriptionFiltersResponse Int
dsfrsResponseStatus = lens _dsfrsResponseStatus (\ s a -> s{_dsfrsResponseStatus = a});

instance NFData DescribeSubscriptionFiltersResponse
