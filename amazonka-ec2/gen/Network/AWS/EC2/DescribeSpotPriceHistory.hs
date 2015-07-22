{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the Spot Price history. The prices returned are listed in
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
    , dsphrqInstanceTypes
    , dsphrqStartTime
    , dsphrqFilters
    , dsphrqNextToken
    , dsphrqAvailabilityZone
    , dsphrqEndTime
    , dsphrqProductDescriptions
    , dsphrqDryRun
    , dsphrqMaxResults

    -- * Response
    , DescribeSpotPriceHistoryResponse
    -- ** Response constructor
    , describeSpotPriceHistoryResponse
    -- ** Response lenses
    , dsphrsNextToken
    , dsphrsSpotPriceHistory
    , dsphrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotPriceHistory.
--
-- /See:/ 'describeSpotPriceHistory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsphrqInstanceTypes'
--
-- * 'dsphrqStartTime'
--
-- * 'dsphrqFilters'
--
-- * 'dsphrqNextToken'
--
-- * 'dsphrqAvailabilityZone'
--
-- * 'dsphrqEndTime'
--
-- * 'dsphrqProductDescriptions'
--
-- * 'dsphrqDryRun'
--
-- * 'dsphrqMaxResults'
data DescribeSpotPriceHistory = DescribeSpotPriceHistory'
    { _dsphrqInstanceTypes       :: !(Maybe [InstanceType])
    , _dsphrqStartTime           :: !(Maybe ISO8601)
    , _dsphrqFilters             :: !(Maybe [Filter])
    , _dsphrqNextToken           :: !(Maybe Text)
    , _dsphrqAvailabilityZone    :: !(Maybe Text)
    , _dsphrqEndTime             :: !(Maybe ISO8601)
    , _dsphrqProductDescriptions :: !(Maybe [Text])
    , _dsphrqDryRun              :: !(Maybe Bool)
    , _dsphrqMaxResults          :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotPriceHistory' smart constructor.
describeSpotPriceHistory :: DescribeSpotPriceHistory
describeSpotPriceHistory =
    DescribeSpotPriceHistory'
    { _dsphrqInstanceTypes = Nothing
    , _dsphrqStartTime = Nothing
    , _dsphrqFilters = Nothing
    , _dsphrqNextToken = Nothing
    , _dsphrqAvailabilityZone = Nothing
    , _dsphrqEndTime = Nothing
    , _dsphrqProductDescriptions = Nothing
    , _dsphrqDryRun = Nothing
    , _dsphrqMaxResults = Nothing
    }

-- | Filters the results by the specified instance types.
dsphrqInstanceTypes :: Lens' DescribeSpotPriceHistory [InstanceType]
dsphrqInstanceTypes = lens _dsphrqInstanceTypes (\ s a -> s{_dsphrqInstanceTypes = a}) . _Default;

-- | The date and time, up to the past 90 days, from which to start
-- retrieving the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsphrqStartTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphrqStartTime = lens _dsphrqStartTime (\ s a -> s{_dsphrqStartTime = a}) . mapping _Time;

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
dsphrqFilters :: Lens' DescribeSpotPriceHistory [Filter]
dsphrqFilters = lens _dsphrqFilters (\ s a -> s{_dsphrqFilters = a}) . _Default;

-- | The token for the next set of results.
dsphrqNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphrqNextToken = lens _dsphrqNextToken (\ s a -> s{_dsphrqNextToken = a});

-- | Filters the results by the specified Availability Zone.
dsphrqAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphrqAvailabilityZone = lens _dsphrqAvailabilityZone (\ s a -> s{_dsphrqAvailabilityZone = a});

-- | The date and time, up to the current date, from which to stop retrieving
-- the price history data, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
dsphrqEndTime :: Lens' DescribeSpotPriceHistory (Maybe UTCTime)
dsphrqEndTime = lens _dsphrqEndTime (\ s a -> s{_dsphrqEndTime = a}) . mapping _Time;

-- | Filters the results by the specified basic product descriptions.
dsphrqProductDescriptions :: Lens' DescribeSpotPriceHistory [Text]
dsphrqProductDescriptions = lens _dsphrqProductDescriptions (\ s a -> s{_dsphrqProductDescriptions = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsphrqDryRun :: Lens' DescribeSpotPriceHistory (Maybe Bool)
dsphrqDryRun = lens _dsphrqDryRun (\ s a -> s{_dsphrqDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
dsphrqMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Int)
dsphrqMaxResults = lens _dsphrqMaxResults (\ s a -> s{_dsphrqMaxResults = a});

instance AWSPager DescribeSpotPriceHistory where
        page rq rs
          | stop (rs ^. dsphrsNextToken) = Nothing
          | stop (rs ^. dsphrsSpotPriceHistory) = Nothing
          | otherwise =
            Just $ rq & dsphrqNextToken .~ rs ^. dsphrsNextToken

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
                     (x .@? "spotPriceHistorySet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
                 (toQueryList "InstanceType" <$>
                    _dsphrqInstanceTypes),
               "StartTime" =: _dsphrqStartTime,
               toQuery (toQueryList "Filter" <$> _dsphrqFilters),
               "NextToken" =: _dsphrqNextToken,
               "AvailabilityZone" =: _dsphrqAvailabilityZone,
               "EndTime" =: _dsphrqEndTime,
               toQuery
                 (toQueryList "ProductDescription" <$>
                    _dsphrqProductDescriptions),
               "DryRun" =: _dsphrqDryRun,
               "MaxResults" =: _dsphrqMaxResults]

-- | Contains the output of DescribeSpotPriceHistory.
--
-- /See:/ 'describeSpotPriceHistoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsphrsNextToken'
--
-- * 'dsphrsSpotPriceHistory'
--
-- * 'dsphrsStatus'
data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse'
    { _dsphrsNextToken        :: !(Maybe Text)
    , _dsphrsSpotPriceHistory :: !(Maybe [SpotPrice])
    , _dsphrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotPriceHistoryResponse' smart constructor.
describeSpotPriceHistoryResponse :: Int -> DescribeSpotPriceHistoryResponse
describeSpotPriceHistoryResponse pStatus =
    DescribeSpotPriceHistoryResponse'
    { _dsphrsNextToken = Nothing
    , _dsphrsSpotPriceHistory = Nothing
    , _dsphrsStatus = pStatus
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsphrsNextToken :: Lens' DescribeSpotPriceHistoryResponse (Maybe Text)
dsphrsNextToken = lens _dsphrsNextToken (\ s a -> s{_dsphrsNextToken = a});

-- | The historical Spot Prices.
dsphrsSpotPriceHistory :: Lens' DescribeSpotPriceHistoryResponse [SpotPrice]
dsphrsSpotPriceHistory = lens _dsphrsSpotPriceHistory (\ s a -> s{_dsphrsSpotPriceHistory = a}) . _Default;

-- | FIXME: Undocumented member.
dsphrsStatus :: Lens' DescribeSpotPriceHistoryResponse Int
dsphrsStatus = lens _dsphrsStatus (\ s a -> s{_dsphrsStatus = a});
