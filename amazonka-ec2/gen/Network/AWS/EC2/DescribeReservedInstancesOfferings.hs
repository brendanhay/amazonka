{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesOfferings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes Reserved Instance offerings that are available for purchase.
-- With Reserved Instances, you purchase the right to launch instances for
-- a period of time. During that time period, you do not receive
-- insufficient capacity errors, and you pay a lower usage rate than the
-- rate charged for On-Demand instances for the actual time used.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesOfferings.html>
module Network.AWS.EC2.DescribeReservedInstancesOfferings
    (
    -- * Request
      DescribeReservedInstancesOfferings
    -- ** Request constructor
    , describeReservedInstancesOfferings
    -- ** Request lenses
    , driorqMaxDuration
    , driorqProductDescription
    , driorqIncludeMarketplace
    , driorqFilters
    , driorqInstanceType
    , driorqNextToken
    , driorqMinDuration
    , driorqAvailabilityZone
    , driorqOfferingType
    , driorqReservedInstancesOfferingIds
    , driorqInstanceTenancy
    , driorqDryRun
    , driorqMaxResults
    , driorqMaxInstanceCount

    -- * Response
    , DescribeReservedInstancesOfferingsResponse
    -- ** Response constructor
    , describeReservedInstancesOfferingsResponse
    -- ** Response lenses
    , driorsNextToken
    , driorsReservedInstancesOfferings
    , driorsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeReservedInstancesOfferings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'driorqMaxDuration'
--
-- * 'driorqProductDescription'
--
-- * 'driorqIncludeMarketplace'
--
-- * 'driorqFilters'
--
-- * 'driorqInstanceType'
--
-- * 'driorqNextToken'
--
-- * 'driorqMinDuration'
--
-- * 'driorqAvailabilityZone'
--
-- * 'driorqOfferingType'
--
-- * 'driorqReservedInstancesOfferingIds'
--
-- * 'driorqInstanceTenancy'
--
-- * 'driorqDryRun'
--
-- * 'driorqMaxResults'
--
-- * 'driorqMaxInstanceCount'
data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings'
    { _driorqMaxDuration                  :: !(Maybe Integer)
    , _driorqProductDescription           :: !(Maybe RIProductDescription)
    , _driorqIncludeMarketplace           :: !(Maybe Bool)
    , _driorqFilters                      :: !(Maybe [Filter])
    , _driorqInstanceType                 :: !(Maybe InstanceType)
    , _driorqNextToken                    :: !(Maybe Text)
    , _driorqMinDuration                  :: !(Maybe Integer)
    , _driorqAvailabilityZone             :: !(Maybe Text)
    , _driorqOfferingType                 :: !(Maybe OfferingTypeValues)
    , _driorqReservedInstancesOfferingIds :: !(Maybe [Text])
    , _driorqInstanceTenancy              :: !(Maybe Tenancy)
    , _driorqDryRun                       :: !(Maybe Bool)
    , _driorqMaxResults                   :: !(Maybe Int)
    , _driorqMaxInstanceCount             :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedInstancesOfferings' smart constructor.
describeReservedInstancesOfferings :: DescribeReservedInstancesOfferings
describeReservedInstancesOfferings =
    DescribeReservedInstancesOfferings'
    { _driorqMaxDuration = Nothing
    , _driorqProductDescription = Nothing
    , _driorqIncludeMarketplace = Nothing
    , _driorqFilters = Nothing
    , _driorqInstanceType = Nothing
    , _driorqNextToken = Nothing
    , _driorqMinDuration = Nothing
    , _driorqAvailabilityZone = Nothing
    , _driorqOfferingType = Nothing
    , _driorqReservedInstancesOfferingIds = Nothing
    , _driorqInstanceTenancy = Nothing
    , _driorqDryRun = Nothing
    , _driorqMaxResults = Nothing
    , _driorqMaxInstanceCount = Nothing
    }

-- | The maximum duration (in seconds) to filter when searching for
-- offerings.
--
-- Default: 94608000 (3 years)
driorqMaxDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
driorqMaxDuration = lens _driorqMaxDuration (\ s a -> s{_driorqMaxDuration = a});

-- | The Reserved Instance product platform description. Instances that
-- include @(Amazon VPC)@ in the description are for use with Amazon VPC.
driorqProductDescription :: Lens' DescribeReservedInstancesOfferings (Maybe RIProductDescription)
driorqProductDescription = lens _driorqProductDescription (\ s a -> s{_driorqProductDescription = a});

-- | Include Marketplace offerings in the response.
driorqIncludeMarketplace :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
driorqIncludeMarketplace = lens _driorqIncludeMarketplace (\ s a -> s{_driorqIncludeMarketplace = a});

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone where the Reserved
--     Instance can be used.
--
-- -   @duration@ - The duration of the Reserved Instance (for example, one
--     year or three years), in seconds (@31536000@ | @94608000@).
--
-- -   @fixed-price@ - The purchase price of the Reserved Instance (for
--     example, 9800.0).
--
-- -   @instance-type@ - The instance type on which the Reserved Instance
--     can be used.
--
-- -   @marketplace@ - Set to @true@ to show only Reserved Instance
--     Marketplace offerings. When this filter is not used, which is the
--     default behavior, all offerings from AWS and Reserved Instance
--     Marketplace are listed.
--
-- -   @product-description@ - The Reserved Instance product platform
--     description. Instances that include @(Amazon VPC)@ in the product
--     platform description will only be displayed to EC2-Classic account
--     holders and are for use with Amazon VPC. (@Linux\/UNIX@ |
--     @Linux\/UNIX (Amazon VPC)@ | @SUSE Linux@ |
--     @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ |
--     @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ |
--     @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ |
--     @Windows with SQL Server Standard (Amazon VPC)@ |
--     @Windows with SQL Server Web@ |
--     @ Windows with SQL Server Web (Amazon VPC)@ |
--     @Windows with SQL Server Enterprise@ |
--     @Windows with SQL Server Enterprise (Amazon VPC)@)
--
-- -   @reserved-instances-offering-id@ - The Reserved Instances offering
--     ID.
--
-- -   @usage-price@ - The usage price of the Reserved Instance, per hour
--     (for example, 0.84).
--
driorqFilters :: Lens' DescribeReservedInstancesOfferings [Filter]
driorqFilters = lens _driorqFilters (\ s a -> s{_driorqFilters = a}) . _Default;

-- | The instance type on which the Reserved Instance can be used. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
driorqInstanceType :: Lens' DescribeReservedInstancesOfferings (Maybe InstanceType)
driorqInstanceType = lens _driorqInstanceType (\ s a -> s{_driorqInstanceType = a});

-- | The token to retrieve the next page of results.
driorqNextToken :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
driorqNextToken = lens _driorqNextToken (\ s a -> s{_driorqNextToken = a});

-- | The minimum duration (in seconds) to filter when searching for
-- offerings.
--
-- Default: 2592000 (1 month)
driorqMinDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
driorqMinDuration = lens _driorqMinDuration (\ s a -> s{_driorqMinDuration = a});

-- | The Availability Zone in which the Reserved Instance can be used.
driorqAvailabilityZone :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
driorqAvailabilityZone = lens _driorqAvailabilityZone (\ s a -> s{_driorqAvailabilityZone = a});

-- | The Reserved Instance offering type. If you are using tools that predate
-- the 2011-11-01 API version, you only have access to the
-- @Medium Utilization@ Reserved Instance offering type.
driorqOfferingType :: Lens' DescribeReservedInstancesOfferings (Maybe OfferingTypeValues)
driorqOfferingType = lens _driorqOfferingType (\ s a -> s{_driorqOfferingType = a});

-- | One or more Reserved Instances offering IDs.
driorqReservedInstancesOfferingIds :: Lens' DescribeReservedInstancesOfferings [Text]
driorqReservedInstancesOfferingIds = lens _driorqReservedInstancesOfferingIds (\ s a -> s{_driorqReservedInstancesOfferingIds = a}) . _Default;

-- | The tenancy of the Reserved Instance offering. A Reserved Instance with
-- @dedicated@ tenancy runs on single-tenant hardware and can only be
-- launched within a VPC.
--
-- Default: @default@
driorqInstanceTenancy :: Lens' DescribeReservedInstancesOfferings (Maybe Tenancy)
driorqInstanceTenancy = lens _driorqInstanceTenancy (\ s a -> s{_driorqInstanceTenancy = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
driorqDryRun :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
driorqDryRun = lens _driorqDryRun (\ s a -> s{_driorqDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. The maximum
-- is 100.
--
-- Default: 100
driorqMaxResults :: Lens' DescribeReservedInstancesOfferings (Maybe Int)
driorqMaxResults = lens _driorqMaxResults (\ s a -> s{_driorqMaxResults = a});

-- | The maximum number of instances to filter when searching for offerings.
--
-- Default: 20
driorqMaxInstanceCount :: Lens' DescribeReservedInstancesOfferings (Maybe Int)
driorqMaxInstanceCount = lens _driorqMaxInstanceCount (\ s a -> s{_driorqMaxInstanceCount = a});

instance AWSPager DescribeReservedInstancesOfferings
         where
        page rq rs
          | stop (rs ^. driorsNextToken) = Nothing
          | stop (rs ^. driorsReservedInstancesOfferings) =
            Nothing
          | otherwise =
            Just $ rq & driorqNextToken .~ rs ^. driorsNextToken

instance AWSRequest
         DescribeReservedInstancesOfferings where
        type Sv DescribeReservedInstancesOfferings = EC2
        type Rs DescribeReservedInstancesOfferings =
             DescribeReservedInstancesOfferingsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeReservedInstancesOfferingsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "reservedInstancesOfferingsSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeReservedInstancesOfferings
         where
        toHeaders = const mempty

instance ToPath DescribeReservedInstancesOfferings
         where
        toPath = const "/"

instance ToQuery DescribeReservedInstancesOfferings
         where
        toQuery DescribeReservedInstancesOfferings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedInstancesOfferings" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "MaxDuration" =: _driorqMaxDuration,
               "ProductDescription" =: _driorqProductDescription,
               "IncludeMarketplace" =: _driorqIncludeMarketplace,
               toQuery (toQueryList "Filter" <$> _driorqFilters),
               "InstanceType" =: _driorqInstanceType,
               "NextToken" =: _driorqNextToken,
               "MinDuration" =: _driorqMinDuration,
               "AvailabilityZone" =: _driorqAvailabilityZone,
               "OfferingType" =: _driorqOfferingType,
               toQuery
                 (toQueryList "ReservedInstancesOfferingId" <$>
                    _driorqReservedInstancesOfferingIds),
               "InstanceTenancy" =: _driorqInstanceTenancy,
               "DryRun" =: _driorqDryRun,
               "MaxResults" =: _driorqMaxResults,
               "MaxInstanceCount" =: _driorqMaxInstanceCount]

-- | /See:/ 'describeReservedInstancesOfferingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'driorsNextToken'
--
-- * 'driorsReservedInstancesOfferings'
--
-- * 'driorsStatus'
data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse'
    { _driorsNextToken                  :: !(Maybe Text)
    , _driorsReservedInstancesOfferings :: !(Maybe [ReservedInstancesOffering])
    , _driorsStatus                     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedInstancesOfferingsResponse' smart constructor.
describeReservedInstancesOfferingsResponse :: Int -> DescribeReservedInstancesOfferingsResponse
describeReservedInstancesOfferingsResponse pStatus =
    DescribeReservedInstancesOfferingsResponse'
    { _driorsNextToken = Nothing
    , _driorsReservedInstancesOfferings = Nothing
    , _driorsStatus = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
driorsNextToken :: Lens' DescribeReservedInstancesOfferingsResponse (Maybe Text)
driorsNextToken = lens _driorsNextToken (\ s a -> s{_driorsNextToken = a});

-- | A list of Reserved Instances offerings.
driorsReservedInstancesOfferings :: Lens' DescribeReservedInstancesOfferingsResponse [ReservedInstancesOffering]
driorsReservedInstancesOfferings = lens _driorsReservedInstancesOfferings (\ s a -> s{_driorsReservedInstancesOfferings = a}) . _Default;

-- | FIXME: Undocumented member.
driorsStatus :: Lens' DescribeReservedInstancesOfferingsResponse Int
driorsStatus = lens _driorsStatus (\ s a -> s{_driorsStatus = a});
