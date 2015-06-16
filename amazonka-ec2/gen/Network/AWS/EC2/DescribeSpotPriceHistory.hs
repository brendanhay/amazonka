{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the Spot Price history. The prices returned are listed in
-- chronological order, from the oldest to the most recent, for up to the
-- past 90 days. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-spot-instances-history.html Spot Instance Pricing History>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- When you specify a start and end time, this operation returns the prices
-- of the instance types within the time range that you specified and the
-- time when the price changed. The price is valid within the time period
-- that you specified; the response merely indicates the last time that the
-- price changed.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotPriceHistory.html>
module Network.AWS.EC2.DescribeSpotPriceHistory
    (
    -- * Request
      DescribeSpotPriceHistory
    -- ** Request constructor
    , describeSpotPriceHistory
    -- ** Request lenses
    , dsphInstanceTypes
    , dsphStartTime
    , dsphFilters
    , dsphNextToken
    , dsphAvailabilityZone
    , dsphEndTime
    , dsphProductDescriptions
    , dsphDryRun
    , dsphMaxResults

    -- * Response
    , DescribeSpotPriceHistoryResponse
    -- ** Response constructor
    , describeSpotPriceHistoryResponse
    -- ** Response lenses
    , dsphrNextToken
    , dsphrSpotPriceHistory
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeSpotPriceHistory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsphInstanceTypes'
--
-- * 'dsphStartTime'
--
-- * 'dsphFilters'
--
-- * 'dsphNextToken'
--
-- * 'dsphAvailabilityZone'
--
-- * 'dsphEndTime'
--
-- * 'dsphProductDescriptions'
--
-- * 'dsphDryRun'
--
-- * 'dsphMaxResults'
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'{_dsphInstanceTypes :: Maybe [InstanceType], _dsphStartTime :: Maybe ISO8601, _dsphFilters :: Maybe [Filter], _dsphNextToken :: Maybe Text, _dsphAvailabilityZone :: Maybe Text, _dsphEndTime :: Maybe ISO8601, _dsphProductDescriptions :: Maybe [Text], _dsphDryRun :: Maybe Bool, _dsphMaxResults :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeSpotPriceHistory' smart constructor.
describeSpotPriceHistory :: DescribeSpotPriceHistory
describeSpotPriceHistory = DescribeSpotPriceHistory'{_dsphInstanceTypes = Nothing, _dsphStartTime = Nothing, _dsphFilters = Nothing, _dsphNextToken = Nothing, _dsphAvailabilityZone = Nothing, _dsphEndTime = Nothing, _dsphProductDescriptions = Nothing, _dsphDryRun = Nothing, _dsphMaxResults = Nothing};

-- | Filters the results by the specified instance types.
dsphInstanceTypes :: Lens' DescribeSpotPriceHistory [InstanceType]
dsphInstanceTypes = lens _dsphInstanceTypes (\ s a -> s{_dsphInstanceTypes = a}) . _Default;

-- | The date and time, up to the past 90 days, from which to start
-- retrieving the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsphStartTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphStartTime = lens _dsphStartTime (\ s a -> s{_dsphStartTime = a}) . mapping _Time;

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone for which prices should
--     be returned.
--
-- -   @instance-type@ - The type of instance (for example, @m1.small@).
--
-- -   @product-description@ - The product description for the Spot Price
--     (@Linux\/UNIX@ | @SUSE Linux@ | @Windows@ |
--     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux (Amazon VPC)@ |
--     @Windows (Amazon VPC)@).
--
-- -   @spot-price@ - The Spot Price. The value must match exactly (or use
--     wildcards; greater than or less than comparison is not supported).
--
-- -   @timestamp@ - The timestamp of the Spot Price history, in UTC format
--     (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). You can use
--     wildcards (* and ?). Greater than or less than comparison is not
--     supported.
--
dsphFilters :: Lens' DescribeSpotPriceHistory [Filter]
dsphFilters = lens _dsphFilters (\ s a -> s{_dsphFilters = a}) . _Default;

-- | The token for the next set of results.
dsphNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphNextToken = lens _dsphNextToken (\ s a -> s{_dsphNextToken = a});

-- | Filters the results by the specified Availability Zone.
dsphAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphAvailabilityZone = lens _dsphAvailabilityZone (\ s a -> s{_dsphAvailabilityZone = a});

-- | The date and time, up to the current date, from which to stop retrieving
-- the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsphEndTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphEndTime = lens _dsphEndTime (\ s a -> s{_dsphEndTime = a}) . mapping _Time;

-- | Filters the results by the specified basic product descriptions.
dsphProductDescriptions :: Lens' DescribeSpotPriceHistory [Text]
dsphProductDescriptions = lens _dsphProductDescriptions (\ s a -> s{_dsphProductDescriptions = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsphDryRun :: Lens' DescribeSpotPriceHistory (Maybe Bool)
dsphDryRun = lens _dsphDryRun (\ s a -> s{_dsphDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
dsphMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Int)
dsphMaxResults = lens _dsphMaxResults (\ s a -> s{_dsphMaxResults = a});

instance AWSRequest DescribeSpotPriceHistory where
        type Sv DescribeSpotPriceHistory = EC2
        type Rs DescribeSpotPriceHistory =
             DescribeSpotPriceHistoryResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotPriceHistoryResponse' <$>
                   (x .@? "nextToken") <*>
                     (may (parseXMLList "item") x))

instance ToHeaders DescribeSpotPriceHistory where
        toHeaders = const mempty

instance ToPath DescribeSpotPriceHistory where
        toPath = const "/"

instance ToQuery DescribeSpotPriceHistory where
        toQuery DescribeSpotPriceHistory'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotPriceHistory" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
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

-- | /See:/ 'describeSpotPriceHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsphrNextToken'
--
-- * 'dsphrSpotPriceHistory'
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'{_dsphrNextToken :: Maybe Text, _dsphrSpotPriceHistory :: Maybe [SpotPrice]} deriving (Eq, Read, Show)

-- | 'DescribeSpotPriceHistoryResponse' smart constructor.
describeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse
describeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'{_dsphrNextToken = Nothing, _dsphrSpotPriceHistory = Nothing};

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsphrNextToken :: Lens' DescribeSpotPriceHistoryResponse (Maybe Text)
dsphrNextToken = lens _dsphrNextToken (\ s a -> s{_dsphrNextToken = a});

-- | The historical Spot Prices.
dsphrSpotPriceHistory :: Lens' DescribeSpotPriceHistoryResponse [SpotPrice]
dsphrSpotPriceHistory = lens _dsphrSpotPriceHistory (\ s a -> s{_dsphrSpotPriceHistory = a}) . _Default;
