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
-- Module      : Network.AWS.EC2.DescribeScheduledInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Scheduled Instances.
--
--
module Network.AWS.EC2.DescribeScheduledInstances
    (
    -- * Creating a Request
      describeScheduledInstances
    , DescribeScheduledInstances
    -- * Request Lenses
    , dsiFilters
    , dsiSlotStartTimeRange
    , dsiNextToken
    , dsiScheduledInstanceIds
    , dsiDryRun
    , dsiMaxResults

    -- * Destructuring the Response
    , describeScheduledInstancesResponse
    , DescribeScheduledInstancesResponse
    -- * Response Lenses
    , dsirsNextToken
    , dsirsScheduledInstanceSet
    , dsirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeScheduledInstances.
--
--
--
-- /See:/ 'describeScheduledInstances' smart constructor.
data DescribeScheduledInstances = DescribeScheduledInstances'
  { _dsiFilters              :: !(Maybe [Filter])
  , _dsiSlotStartTimeRange   :: !(Maybe SlotStartTimeRangeRequest)
  , _dsiNextToken            :: !(Maybe Text)
  , _dsiScheduledInstanceIds :: !(Maybe [Text])
  , _dsiDryRun               :: !(Maybe Bool)
  , _dsiMaxResults           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScheduledInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiFilters' - One or more filters.     * @availability-zone@ - The Availability Zone (for example, @us-west-2a@ ).     * @instance-type@ - The instance type (for example, @c4.large@ ).     * @network-platform@ - The network platform (@EC2-Classic@ or @EC2-VPC@ ).     * @platform@ - The platform (@Linux/UNIX@ or @Windows@ ).
--
-- * 'dsiSlotStartTimeRange' - The time period for the first schedule to start.
--
-- * 'dsiNextToken' - The token for the next set of results.
--
-- * 'dsiScheduledInstanceIds' - One or more Scheduled Instance IDs.
--
-- * 'dsiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsiMaxResults' - The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeScheduledInstances
    :: DescribeScheduledInstances
describeScheduledInstances =
  DescribeScheduledInstances'
    { _dsiFilters = Nothing
    , _dsiSlotStartTimeRange = Nothing
    , _dsiNextToken = Nothing
    , _dsiScheduledInstanceIds = Nothing
    , _dsiDryRun = Nothing
    , _dsiMaxResults = Nothing
    }


-- | One or more filters.     * @availability-zone@ - The Availability Zone (for example, @us-west-2a@ ).     * @instance-type@ - The instance type (for example, @c4.large@ ).     * @network-platform@ - The network platform (@EC2-Classic@ or @EC2-VPC@ ).     * @platform@ - The platform (@Linux/UNIX@ or @Windows@ ).
dsiFilters :: Lens' DescribeScheduledInstances [Filter]
dsiFilters = lens _dsiFilters (\ s a -> s{_dsiFilters = a}) . _Default . _Coerce

-- | The time period for the first schedule to start.
dsiSlotStartTimeRange :: Lens' DescribeScheduledInstances (Maybe SlotStartTimeRangeRequest)
dsiSlotStartTimeRange = lens _dsiSlotStartTimeRange (\ s a -> s{_dsiSlotStartTimeRange = a})

-- | The token for the next set of results.
dsiNextToken :: Lens' DescribeScheduledInstances (Maybe Text)
dsiNextToken = lens _dsiNextToken (\ s a -> s{_dsiNextToken = a})

-- | One or more Scheduled Instance IDs.
dsiScheduledInstanceIds :: Lens' DescribeScheduledInstances [Text]
dsiScheduledInstanceIds = lens _dsiScheduledInstanceIds (\ s a -> s{_dsiScheduledInstanceIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsiDryRun :: Lens' DescribeScheduledInstances (Maybe Bool)
dsiDryRun = lens _dsiDryRun (\ s a -> s{_dsiDryRun = a})

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dsiMaxResults :: Lens' DescribeScheduledInstances (Maybe Int)
dsiMaxResults = lens _dsiMaxResults (\ s a -> s{_dsiMaxResults = a})

instance AWSRequest DescribeScheduledInstances where
        type Rs DescribeScheduledInstances =
             DescribeScheduledInstancesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeScheduledInstancesResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "scheduledInstanceSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScheduledInstances where

instance NFData DescribeScheduledInstances where

instance ToHeaders DescribeScheduledInstances where
        toHeaders = const mempty

instance ToPath DescribeScheduledInstances where
        toPath = const "/"

instance ToQuery DescribeScheduledInstances where
        toQuery DescribeScheduledInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeScheduledInstances" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dsiFilters),
               "SlotStartTimeRange" =: _dsiSlotStartTimeRange,
               "NextToken" =: _dsiNextToken,
               toQuery
                 (toQueryList "ScheduledInstanceId" <$>
                    _dsiScheduledInstanceIds),
               "DryRun" =: _dsiDryRun,
               "MaxResults" =: _dsiMaxResults]

-- | Contains the output of DescribeScheduledInstances.
--
--
--
-- /See:/ 'describeScheduledInstancesResponse' smart constructor.
data DescribeScheduledInstancesResponse = DescribeScheduledInstancesResponse'
  { _dsirsNextToken            :: !(Maybe Text)
  , _dsirsScheduledInstanceSet :: !(Maybe [ScheduledInstance])
  , _dsirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeScheduledInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirsNextToken' - The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- * 'dsirsScheduledInstanceSet' - Information about the Scheduled Instances.
--
-- * 'dsirsResponseStatus' - -- | The response status code.
describeScheduledInstancesResponse
    :: Int -- ^ 'dsirsResponseStatus'
    -> DescribeScheduledInstancesResponse
describeScheduledInstancesResponse pResponseStatus_ =
  DescribeScheduledInstancesResponse'
    { _dsirsNextToken = Nothing
    , _dsirsScheduledInstanceSet = Nothing
    , _dsirsResponseStatus = pResponseStatus_
    }


-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
dsirsNextToken :: Lens' DescribeScheduledInstancesResponse (Maybe Text)
dsirsNextToken = lens _dsirsNextToken (\ s a -> s{_dsirsNextToken = a})

-- | Information about the Scheduled Instances.
dsirsScheduledInstanceSet :: Lens' DescribeScheduledInstancesResponse [ScheduledInstance]
dsirsScheduledInstanceSet = lens _dsirsScheduledInstanceSet (\ s a -> s{_dsirsScheduledInstanceSet = a}) . _Default . _Coerce

-- | -- | The response status code.
dsirsResponseStatus :: Lens' DescribeScheduledInstancesResponse Int
dsirsResponseStatus = lens _dsirsResponseStatus (\ s a -> s{_dsirsResponseStatus = a})

instance NFData DescribeScheduledInstancesResponse
         where
