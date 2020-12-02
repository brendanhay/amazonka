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
-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified Spot Fleet.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetInstances
    (
    -- * Creating a Request
      describeSpotFleetInstances
    , DescribeSpotFleetInstances
    -- * Request Lenses
    , dsfiNextToken
    , dsfiDryRun
    , dsfiMaxResults
    , dsfiSpotFleetRequestId

    -- * Destructuring the Response
    , describeSpotFleetInstancesResponse
    , DescribeSpotFleetInstancesResponse
    -- * Response Lenses
    , dsfirsNextToken
    , dsfirsResponseStatus
    , dsfirsActiveInstances
    , dsfirsSpotFleetRequestId
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetInstances.
--
--
--
-- /See:/ 'describeSpotFleetInstances' smart constructor.
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
  { _dsfiNextToken          :: !(Maybe Text)
  , _dsfiDryRun             :: !(Maybe Bool)
  , _dsfiMaxResults         :: !(Maybe Int)
  , _dsfiSpotFleetRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotFleetInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfiNextToken' - The token for the next set of results.
--
-- * 'dsfiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsfiMaxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- * 'dsfiSpotFleetRequestId' - The ID of the Spot Fleet request.
describeSpotFleetInstances
    :: Text -- ^ 'dsfiSpotFleetRequestId'
    -> DescribeSpotFleetInstances
describeSpotFleetInstances pSpotFleetRequestId_ =
  DescribeSpotFleetInstances'
    { _dsfiNextToken = Nothing
    , _dsfiDryRun = Nothing
    , _dsfiMaxResults = Nothing
    , _dsfiSpotFleetRequestId = pSpotFleetRequestId_
    }


-- | The token for the next set of results.
dsfiNextToken :: Lens' DescribeSpotFleetInstances (Maybe Text)
dsfiNextToken = lens _dsfiNextToken (\ s a -> s{_dsfiNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsfiDryRun :: Lens' DescribeSpotFleetInstances (Maybe Bool)
dsfiDryRun = lens _dsfiDryRun (\ s a -> s{_dsfiDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dsfiMaxResults :: Lens' DescribeSpotFleetInstances (Maybe Int)
dsfiMaxResults = lens _dsfiMaxResults (\ s a -> s{_dsfiMaxResults = a})

-- | The ID of the Spot Fleet request.
dsfiSpotFleetRequestId :: Lens' DescribeSpotFleetInstances Text
dsfiSpotFleetRequestId = lens _dsfiSpotFleetRequestId (\ s a -> s{_dsfiSpotFleetRequestId = a})

instance AWSPager DescribeSpotFleetInstances where
        page rq rs
          | stop (rs ^. dsfirsNextToken) = Nothing
          | stop (rs ^. dsfirsActiveInstances) = Nothing
          | otherwise =
            Just $ rq & dsfiNextToken .~ rs ^. dsfirsNextToken

instance AWSRequest DescribeSpotFleetInstances where
        type Rs DescribeSpotFleetInstances =
             DescribeSpotFleetInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetInstancesResponse' <$>
                   (x .@? "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "activeInstanceSet" .!@ mempty >>=
                        parseXMLList "item")
                     <*> (x .@ "spotFleetRequestId"))

instance Hashable DescribeSpotFleetInstances where

instance NFData DescribeSpotFleetInstances where

instance ToHeaders DescribeSpotFleetInstances where
        toHeaders = const mempty

instance ToPath DescribeSpotFleetInstances where
        toPath = const "/"

instance ToQuery DescribeSpotFleetInstances where
        toQuery DescribeSpotFleetInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotFleetInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NextToken" =: _dsfiNextToken,
               "DryRun" =: _dsfiDryRun,
               "MaxResults" =: _dsfiMaxResults,
               "SpotFleetRequestId" =: _dsfiSpotFleetRequestId]

-- | Contains the output of DescribeSpotFleetInstances.
--
--
--
-- /See:/ 'describeSpotFleetInstancesResponse' smart constructor.
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
  { _dsfirsNextToken          :: !(Maybe Text)
  , _dsfirsResponseStatus     :: !Int
  , _dsfirsActiveInstances    :: ![ActiveInstance]
  , _dsfirsSpotFleetRequestId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotFleetInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfirsNextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- * 'dsfirsResponseStatus' - -- | The response status code.
--
-- * 'dsfirsActiveInstances' - The running instances. This list is refreshed periodically and might be out of date.
--
-- * 'dsfirsSpotFleetRequestId' - The ID of the Spot Fleet request.
describeSpotFleetInstancesResponse
    :: Int -- ^ 'dsfirsResponseStatus'
    -> Text -- ^ 'dsfirsSpotFleetRequestId'
    -> DescribeSpotFleetInstancesResponse
describeSpotFleetInstancesResponse pResponseStatus_ pSpotFleetRequestId_ =
  DescribeSpotFleetInstancesResponse'
    { _dsfirsNextToken = Nothing
    , _dsfirsResponseStatus = pResponseStatus_
    , _dsfirsActiveInstances = mempty
    , _dsfirsSpotFleetRequestId = pSpotFleetRequestId_
    }


-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
dsfirsNextToken :: Lens' DescribeSpotFleetInstancesResponse (Maybe Text)
dsfirsNextToken = lens _dsfirsNextToken (\ s a -> s{_dsfirsNextToken = a})

-- | -- | The response status code.
dsfirsResponseStatus :: Lens' DescribeSpotFleetInstancesResponse Int
dsfirsResponseStatus = lens _dsfirsResponseStatus (\ s a -> s{_dsfirsResponseStatus = a})

-- | The running instances. This list is refreshed periodically and might be out of date.
dsfirsActiveInstances :: Lens' DescribeSpotFleetInstancesResponse [ActiveInstance]
dsfirsActiveInstances = lens _dsfirsActiveInstances (\ s a -> s{_dsfirsActiveInstances = a}) . _Coerce

-- | The ID of the Spot Fleet request.
dsfirsSpotFleetRequestId :: Lens' DescribeSpotFleetInstancesResponse Text
dsfirsSpotFleetRequestId = lens _dsfirsSpotFleetRequestId (\ s a -> s{_dsfirsSpotFleetRequestId = a})

instance NFData DescribeSpotFleetInstancesResponse
         where
