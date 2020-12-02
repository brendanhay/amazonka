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
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Spot Fleet requests.
--
--
-- Spot Fleet requests are deleted 48 hours after they are canceled and their instances are terminated.
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetRequests
    (
    -- * Creating a Request
      describeSpotFleetRequests
    , DescribeSpotFleetRequests
    -- * Request Lenses
    , dsfrSpotFleetRequestIds
    , dsfrNextToken
    , dsfrDryRun
    , dsfrMaxResults

    -- * Destructuring the Response
    , describeSpotFleetRequestsResponse
    , DescribeSpotFleetRequestsResponse
    -- * Response Lenses
    , dsfrrsNextToken
    , dsfrrsResponseStatus
    , dsfrrsSpotFleetRequestConfigs
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetRequests.
--
--
--
-- /See:/ 'describeSpotFleetRequests' smart constructor.
data DescribeSpotFleetRequests = DescribeSpotFleetRequests'
  { _dsfrSpotFleetRequestIds :: !(Maybe [Text])
  , _dsfrNextToken           :: !(Maybe Text)
  , _dsfrDryRun              :: !(Maybe Bool)
  , _dsfrMaxResults          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotFleetRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfrSpotFleetRequestIds' - The IDs of the Spot Fleet requests.
--
-- * 'dsfrNextToken' - The token for the next set of results.
--
-- * 'dsfrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsfrMaxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeSpotFleetRequests
    :: DescribeSpotFleetRequests
describeSpotFleetRequests =
  DescribeSpotFleetRequests'
    { _dsfrSpotFleetRequestIds = Nothing
    , _dsfrNextToken = Nothing
    , _dsfrDryRun = Nothing
    , _dsfrMaxResults = Nothing
    }


-- | The IDs of the Spot Fleet requests.
dsfrSpotFleetRequestIds :: Lens' DescribeSpotFleetRequests [Text]
dsfrSpotFleetRequestIds = lens _dsfrSpotFleetRequestIds (\ s a -> s{_dsfrSpotFleetRequestIds = a}) . _Default . _Coerce

-- | The token for the next set of results.
dsfrNextToken :: Lens' DescribeSpotFleetRequests (Maybe Text)
dsfrNextToken = lens _dsfrNextToken (\ s a -> s{_dsfrNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsfrDryRun :: Lens' DescribeSpotFleetRequests (Maybe Bool)
dsfrDryRun = lens _dsfrDryRun (\ s a -> s{_dsfrDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dsfrMaxResults :: Lens' DescribeSpotFleetRequests (Maybe Int)
dsfrMaxResults = lens _dsfrMaxResults (\ s a -> s{_dsfrMaxResults = a})

instance AWSPager DescribeSpotFleetRequests where
        page rq rs
          | stop (rs ^. dsfrrsNextToken) = Nothing
          | stop (rs ^. dsfrrsSpotFleetRequestConfigs) =
            Nothing
          | otherwise =
            Just $ rq & dsfrNextToken .~ rs ^. dsfrrsNextToken

instance AWSRequest DescribeSpotFleetRequests where
        type Rs DescribeSpotFleetRequests =
             DescribeSpotFleetRequestsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetRequestsResponse' <$>
                   (x .@? "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "spotFleetRequestConfigSet" .!@ mempty >>=
                        parseXMLList "item"))

instance Hashable DescribeSpotFleetRequests where

instance NFData DescribeSpotFleetRequests where

instance ToHeaders DescribeSpotFleetRequests where
        toHeaders = const mempty

instance ToPath DescribeSpotFleetRequests where
        toPath = const "/"

instance ToQuery DescribeSpotFleetRequests where
        toQuery DescribeSpotFleetRequests'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotFleetRequests" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "SpotFleetRequestId" <$>
                    _dsfrSpotFleetRequestIds),
               "NextToken" =: _dsfrNextToken,
               "DryRun" =: _dsfrDryRun,
               "MaxResults" =: _dsfrMaxResults]

-- | Contains the output of DescribeSpotFleetRequests.
--
--
--
-- /See:/ 'describeSpotFleetRequestsResponse' smart constructor.
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
  { _dsfrrsNextToken               :: !(Maybe Text)
  , _dsfrrsResponseStatus          :: !Int
  , _dsfrrsSpotFleetRequestConfigs :: ![SpotFleetRequestConfig]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotFleetRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfrrsNextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- * 'dsfrrsResponseStatus' - -- | The response status code.
--
-- * 'dsfrrsSpotFleetRequestConfigs' - Information about the configuration of your Spot Fleet.
describeSpotFleetRequestsResponse
    :: Int -- ^ 'dsfrrsResponseStatus'
    -> DescribeSpotFleetRequestsResponse
describeSpotFleetRequestsResponse pResponseStatus_ =
  DescribeSpotFleetRequestsResponse'
    { _dsfrrsNextToken = Nothing
    , _dsfrrsResponseStatus = pResponseStatus_
    , _dsfrrsSpotFleetRequestConfigs = mempty
    }


-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
dsfrrsNextToken :: Lens' DescribeSpotFleetRequestsResponse (Maybe Text)
dsfrrsNextToken = lens _dsfrrsNextToken (\ s a -> s{_dsfrrsNextToken = a})

-- | -- | The response status code.
dsfrrsResponseStatus :: Lens' DescribeSpotFleetRequestsResponse Int
dsfrrsResponseStatus = lens _dsfrrsResponseStatus (\ s a -> s{_dsfrrsResponseStatus = a})

-- | Information about the configuration of your Spot Fleet.
dsfrrsSpotFleetRequestConfigs :: Lens' DescribeSpotFleetRequestsResponse [SpotFleetRequestConfig]
dsfrrsSpotFleetRequestConfigs = lens _dsfrrsSpotFleetRequestConfigs (\ s a -> s{_dsfrrsSpotFleetRequestConfigs = a}) . _Coerce

instance NFData DescribeSpotFleetRequestsResponse
         where
