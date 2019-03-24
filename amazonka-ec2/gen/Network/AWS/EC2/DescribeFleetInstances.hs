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
-- Module      : Network.AWS.EC2.DescribeFleetInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified EC2 Fleet.
--
--
module Network.AWS.EC2.DescribeFleetInstances
    (
    -- * Creating a Request
      describeFleetInstances
    , DescribeFleetInstances
    -- * Request Lenses
    , dfisFilters
    , dfisNextToken
    , dfisDryRun
    , dfisMaxResults
    , dfisFleetId

    -- * Destructuring the Response
    , describeFleetInstancesResponse
    , DescribeFleetInstancesResponse
    -- * Response Lenses
    , drsNextToken
    , drsFleetId
    , drsActiveInstances
    , drsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFleetInstances' smart constructor.
data DescribeFleetInstances = DescribeFleetInstances'
  { _dfisFilters    :: !(Maybe [Filter])
  , _dfisNextToken  :: !(Maybe Text)
  , _dfisDryRun     :: !(Maybe Bool)
  , _dfisMaxResults :: !(Maybe Int)
  , _dfisFleetId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfisFilters' - One or more filters.     * @instance-type@ - The instance type.
--
-- * 'dfisNextToken' - The token for the next set of results.
--
-- * 'dfisDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfisMaxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- * 'dfisFleetId' - The ID of the EC2 Fleet.
describeFleetInstances
    :: Text -- ^ 'dfisFleetId'
    -> DescribeFleetInstances
describeFleetInstances pFleetId_ =
  DescribeFleetInstances'
    { _dfisFilters = Nothing
    , _dfisNextToken = Nothing
    , _dfisDryRun = Nothing
    , _dfisMaxResults = Nothing
    , _dfisFleetId = pFleetId_
    }


-- | One or more filters.     * @instance-type@ - The instance type.
dfisFilters :: Lens' DescribeFleetInstances [Filter]
dfisFilters = lens _dfisFilters (\ s a -> s{_dfisFilters = a}) . _Default . _Coerce

-- | The token for the next set of results.
dfisNextToken :: Lens' DescribeFleetInstances (Maybe Text)
dfisNextToken = lens _dfisNextToken (\ s a -> s{_dfisNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfisDryRun :: Lens' DescribeFleetInstances (Maybe Bool)
dfisDryRun = lens _dfisDryRun (\ s a -> s{_dfisDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dfisMaxResults :: Lens' DescribeFleetInstances (Maybe Int)
dfisMaxResults = lens _dfisMaxResults (\ s a -> s{_dfisMaxResults = a})

-- | The ID of the EC2 Fleet.
dfisFleetId :: Lens' DescribeFleetInstances Text
dfisFleetId = lens _dfisFleetId (\ s a -> s{_dfisFleetId = a})

instance AWSRequest DescribeFleetInstances where
        type Rs DescribeFleetInstances =
             DescribeFleetInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeFleetInstancesResponse' <$>
                   (x .@? "nextToken") <*> (x .@? "fleetId") <*>
                     (x .@? "activeInstanceSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFleetInstances where

instance NFData DescribeFleetInstances where

instance ToHeaders DescribeFleetInstances where
        toHeaders = const mempty

instance ToPath DescribeFleetInstances where
        toPath = const "/"

instance ToQuery DescribeFleetInstances where
        toQuery DescribeFleetInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeFleetInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dfisFilters),
               "NextToken" =: _dfisNextToken,
               "DryRun" =: _dfisDryRun,
               "MaxResults" =: _dfisMaxResults,
               "FleetId" =: _dfisFleetId]

-- | /See:/ 'describeFleetInstancesResponse' smart constructor.
data DescribeFleetInstancesResponse = DescribeFleetInstancesResponse'
  { _drsNextToken       :: !(Maybe Text)
  , _drsFleetId         :: !(Maybe Text)
  , _drsActiveInstances :: !(Maybe [ActiveInstance])
  , _drsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFleetInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - The token for the next set of results.
--
-- * 'drsFleetId' - The ID of the EC2 Fleet.
--
-- * 'drsActiveInstances' - The running instances. This list is refreshed periodically and might be out of date.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeFleetInstancesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeFleetInstancesResponse
describeFleetInstancesResponse pResponseStatus_ =
  DescribeFleetInstancesResponse'
    { _drsNextToken = Nothing
    , _drsFleetId = Nothing
    , _drsActiveInstances = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of results.
drsNextToken :: Lens' DescribeFleetInstancesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | The ID of the EC2 Fleet.
drsFleetId :: Lens' DescribeFleetInstancesResponse (Maybe Text)
drsFleetId = lens _drsFleetId (\ s a -> s{_drsFleetId = a})

-- | The running instances. This list is refreshed periodically and might be out of date.
drsActiveInstances :: Lens' DescribeFleetInstancesResponse [ActiveInstance]
drsActiveInstances = lens _drsActiveInstances (\ s a -> s{_drsActiveInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeFleetInstancesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeFleetInstancesResponse where
