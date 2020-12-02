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
-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Spot price history. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html Spot Instance Pricing History> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
-- When you specify a start and end time, this operation returns the prices of the instance types within the time range that you specified and the time when the price changed. The price is valid within the time period that you specified; the response merely indicates the last time that the price changed.
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotPriceHistory
    (
    -- * Creating a Request
      describeSpotPriceHistory
    , DescribeSpotPriceHistory
    -- * Request Lenses
    , dsphInstanceTypes
    , dsphStartTime
    , dsphFilters
    , dsphNextToken
    , dsphAvailabilityZone
    , dsphEndTime
    , dsphProductDescriptions
    , dsphDryRun
    , dsphMaxResults

    -- * Destructuring the Response
    , describeSpotPriceHistoryResponse
    , DescribeSpotPriceHistoryResponse
    -- * Response Lenses
    , dsphrsNextToken
    , dsphrsSpotPriceHistory
    , dsphrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSpotPriceHistory.
--
--
--
-- /See:/ 'describeSpotPriceHistory' smart constructor.
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'
  { _dsphInstanceTypes       :: !(Maybe [InstanceType])
  , _dsphStartTime           :: !(Maybe ISO8601)
  , _dsphFilters             :: !(Maybe [Filter])
  , _dsphNextToken           :: !(Maybe Text)
  , _dsphAvailabilityZone    :: !(Maybe Text)
  , _dsphEndTime             :: !(Maybe ISO8601)
  , _dsphProductDescriptions :: !(Maybe [Text])
  , _dsphDryRun              :: !(Maybe Bool)
  , _dsphMaxResults          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotPriceHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsphInstanceTypes' - Filters the results by the specified instance types.
--
-- * 'dsphStartTime' - The date and time, up to the past 90 days, from which to start retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- * 'dsphFilters' - One or more filters.     * @availability-zone@ - The Availability Zone for which prices should be returned.     * @instance-type@ - The type of instance (for example, @m3.medium@ ).     * @product-description@ - The product description for the Spot price (@Linux/UNIX@ | @SUSE Linux@ | @Windows@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@ | @Windows (Amazon VPC)@ ).     * @spot-price@ - The Spot price. The value must match exactly (or use wildcards; greater than or less than comparison is not supported).     * @timestamp@ - The time stamp of the Spot price history, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). You can use wildcards (* and ?). Greater than or less than comparison is not supported.
--
-- * 'dsphNextToken' - The token for the next set of results.
--
-- * 'dsphAvailabilityZone' - Filters the results by the specified Availability Zone.
--
-- * 'dsphEndTime' - The date and time, up to the current date, from which to stop retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- * 'dsphProductDescriptions' - Filters the results by the specified basic product descriptions.
--
-- * 'dsphDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dsphMaxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeSpotPriceHistory
    :: DescribeSpotPriceHistory
describeSpotPriceHistory =
  DescribeSpotPriceHistory'
    { _dsphInstanceTypes = Nothing
    , _dsphStartTime = Nothing
    , _dsphFilters = Nothing
    , _dsphNextToken = Nothing
    , _dsphAvailabilityZone = Nothing
    , _dsphEndTime = Nothing
    , _dsphProductDescriptions = Nothing
    , _dsphDryRun = Nothing
    , _dsphMaxResults = Nothing
    }


-- | Filters the results by the specified instance types.
dsphInstanceTypes :: Lens' DescribeSpotPriceHistory [InstanceType]
dsphInstanceTypes = lens _dsphInstanceTypes (\ s a -> s{_dsphInstanceTypes = a}) . _Default . _Coerce

-- | The date and time, up to the past 90 days, from which to start retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
dsphStartTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphStartTime = lens _dsphStartTime (\ s a -> s{_dsphStartTime = a}) . mapping _Time

-- | One or more filters.     * @availability-zone@ - The Availability Zone for which prices should be returned.     * @instance-type@ - The type of instance (for example, @m3.medium@ ).     * @product-description@ - The product description for the Spot price (@Linux/UNIX@ | @SUSE Linux@ | @Windows@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@ | @Windows (Amazon VPC)@ ).     * @spot-price@ - The Spot price. The value must match exactly (or use wildcards; greater than or less than comparison is not supported).     * @timestamp@ - The time stamp of the Spot price history, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). You can use wildcards (* and ?). Greater than or less than comparison is not supported.
dsphFilters :: Lens' DescribeSpotPriceHistory [Filter]
dsphFilters = lens _dsphFilters (\ s a -> s{_dsphFilters = a}) . _Default . _Coerce

-- | The token for the next set of results.
dsphNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphNextToken = lens _dsphNextToken (\ s a -> s{_dsphNextToken = a})

-- | Filters the results by the specified Availability Zone.
dsphAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphAvailabilityZone = lens _dsphAvailabilityZone (\ s a -> s{_dsphAvailabilityZone = a})

-- | The date and time, up to the current date, from which to stop retrieving the price history data, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
dsphEndTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphEndTime = lens _dsphEndTime (\ s a -> s{_dsphEndTime = a}) . mapping _Time

-- | Filters the results by the specified basic product descriptions.
dsphProductDescriptions :: Lens' DescribeSpotPriceHistory [Text]
dsphProductDescriptions = lens _dsphProductDescriptions (\ s a -> s{_dsphProductDescriptions = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsphDryRun :: Lens' DescribeSpotPriceHistory (Maybe Bool)
dsphDryRun = lens _dsphDryRun (\ s a -> s{_dsphDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dsphMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Int)
dsphMaxResults = lens _dsphMaxResults (\ s a -> s{_dsphMaxResults = a})

instance AWSPager DescribeSpotPriceHistory where
        page rq rs
          | stop (rs ^. dsphrsNextToken) = Nothing
          | stop (rs ^. dsphrsSpotPriceHistory) = Nothing
          | otherwise =
            Just $ rq & dsphNextToken .~ rs ^. dsphrsNextToken

instance AWSRequest DescribeSpotPriceHistory where
        type Rs DescribeSpotPriceHistory =
             DescribeSpotPriceHistoryResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotPriceHistoryResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "spotPriceHistorySet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSpotPriceHistory where

instance NFData DescribeSpotPriceHistory where

instance ToHeaders DescribeSpotPriceHistory where
        toHeaders = const mempty

instance ToPath DescribeSpotPriceHistory where
        toPath = const "/"

instance ToQuery DescribeSpotPriceHistory where
        toQuery DescribeSpotPriceHistory'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotPriceHistory" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "InstanceType" <$> _dsphInstanceTypes),
               "StartTime" =: _dsphStartTime,
               toQuery (toQueryList "Filter" <$> _dsphFilters),
               "NextToken" =: _dsphNextToken,
               "AvailabilityZone" =: _dsphAvailabilityZone,
               "EndTime" =: _dsphEndTime,
               toQuery
                 (toQueryList "ProductDescription" <$>
                    _dsphProductDescriptions),
               "DryRun" =: _dsphDryRun,
               "MaxResults" =: _dsphMaxResults]

-- | Contains the output of DescribeSpotPriceHistory.
--
--
--
-- /See:/ 'describeSpotPriceHistoryResponse' smart constructor.
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'
  { _dsphrsNextToken        :: !(Maybe Text)
  , _dsphrsSpotPriceHistory :: !(Maybe [SpotPrice])
  , _dsphrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotPriceHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsphrsNextToken' - The token required to retrieve the next set of results. This value is null or an empty string when there are no more results to return.
--
-- * 'dsphrsSpotPriceHistory' - The historical Spot prices.
--
-- * 'dsphrsResponseStatus' - -- | The response status code.
describeSpotPriceHistoryResponse
    :: Int -- ^ 'dsphrsResponseStatus'
    -> DescribeSpotPriceHistoryResponse
describeSpotPriceHistoryResponse pResponseStatus_ =
  DescribeSpotPriceHistoryResponse'
    { _dsphrsNextToken = Nothing
    , _dsphrsSpotPriceHistory = Nothing
    , _dsphrsResponseStatus = pResponseStatus_
    }


-- | The token required to retrieve the next set of results. This value is null or an empty string when there are no more results to return.
dsphrsNextToken :: Lens' DescribeSpotPriceHistoryResponse (Maybe Text)
dsphrsNextToken = lens _dsphrsNextToken (\ s a -> s{_dsphrsNextToken = a})

-- | The historical Spot prices.
dsphrsSpotPriceHistory :: Lens' DescribeSpotPriceHistoryResponse [SpotPrice]
dsphrsSpotPriceHistory = lens _dsphrsSpotPriceHistory (\ s a -> s{_dsphrsSpotPriceHistory = a}) . _Default . _Coerce

-- | -- | The response status code.
dsphrsResponseStatus :: Lens' DescribeSpotPriceHistoryResponse Int
dsphrsResponseStatus = lens _dsphrsResponseStatus (\ s a -> s{_dsphrsResponseStatus = a})

instance NFData DescribeSpotPriceHistoryResponse
         where
