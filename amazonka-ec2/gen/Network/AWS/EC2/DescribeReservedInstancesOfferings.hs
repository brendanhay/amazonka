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
-- Module      : Network.AWS.EC2.DescribeReservedInstancesOfferings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Reserved Instance offerings that are available for purchase. With Reserved Instances, you purchase the right to launch instances for a period of time. During that time period, you do not receive insufficient capacity errors, and you pay a lower usage rate than the rate charged for On-Demand instances for the actual time used.
--
--
-- If you have listed your own Reserved Instances for sale in the Reserved Instance Marketplace, they will be excluded from these results. This is to ensure that you do not purchase your own Reserved Instances.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeReservedInstancesOfferings
    (
    -- * Creating a Request
      describeReservedInstancesOfferings
    , DescribeReservedInstancesOfferings
    -- * Request Lenses
    , drioMaxDuration
    , drioProductDescription
    , drioFilters
    , drioIncludeMarketplace
    , drioInstanceType
    , drioNextToken
    , drioMinDuration
    , drioAvailabilityZone
    , drioOfferingType
    , drioReservedInstancesOfferingIds
    , drioInstanceTenancy
    , drioOfferingClass
    , drioMaxInstanceCount
    , drioDryRun
    , drioMaxResults

    -- * Destructuring the Response
    , describeReservedInstancesOfferingsResponse
    , DescribeReservedInstancesOfferingsResponse
    -- * Response Lenses
    , driorsNextToken
    , driorsReservedInstancesOfferings
    , driorsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeReservedInstancesOfferings.
--
--
--
-- /See:/ 'describeReservedInstancesOfferings' smart constructor.
data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings'
  { _drioMaxDuration                  :: !(Maybe Integer)
  , _drioProductDescription           :: !(Maybe RIProductDescription)
  , _drioFilters                      :: !(Maybe [Filter])
  , _drioIncludeMarketplace           :: !(Maybe Bool)
  , _drioInstanceType                 :: !(Maybe InstanceType)
  , _drioNextToken                    :: !(Maybe Text)
  , _drioMinDuration                  :: !(Maybe Integer)
  , _drioAvailabilityZone             :: !(Maybe Text)
  , _drioOfferingType                 :: !(Maybe OfferingTypeValues)
  , _drioReservedInstancesOfferingIds :: !(Maybe [Text])
  , _drioInstanceTenancy              :: !(Maybe Tenancy)
  , _drioOfferingClass                :: !(Maybe OfferingClassType)
  , _drioMaxInstanceCount             :: !(Maybe Int)
  , _drioDryRun                       :: !(Maybe Bool)
  , _drioMaxResults                   :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedInstancesOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drioMaxDuration' - The maximum duration (in seconds) to filter when searching for offerings. Default: 94608000 (3 years)
--
-- * 'drioProductDescription' - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the description are for use with Amazon VPC.
--
-- * 'drioFilters' - One or more filters.     * @availability-zone@ - The Availability Zone where the Reserved Instance can be used.     * @duration@ - The duration of the Reserved Instance (for example, one year or three years), in seconds (@31536000@ | @94608000@ ).     * @fixed-price@ - The purchase price of the Reserved Instance (for example, 9800.0).     * @instance-type@ - The instance type that is covered by the reservation.     * @marketplace@ - Set to @true@ to show only Reserved Instance Marketplace offerings. When this filter is not used, which is the default behavior, all offerings from both AWS and the Reserved Instance Marketplace are listed.     * @product-description@ - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the product platform description will only be displayed to EC2-Classic account holders and are for use with Amazon VPC. (@Linux/UNIX@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux@ | @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ | @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ | @Windows with SQL Server Standard (Amazon VPC)@ | @Windows with SQL Server Web@ | @Windows with SQL Server Web (Amazon VPC)@ | @Windows with SQL Server Enterprise@ | @Windows with SQL Server Enterprise (Amazon VPC)@ )      * @reserved-instances-offering-id@ - The Reserved Instances offering ID.     * @scope@ - The scope of the Reserved Instance (@Availability Zone@ or @Region@ ).     * @usage-price@ - The usage price of the Reserved Instance, per hour (for example, 0.84).
--
-- * 'drioIncludeMarketplace' - Include Reserved Instance Marketplace offerings in the response.
--
-- * 'drioInstanceType' - The instance type that the reservation will cover (for example, @m1.small@ ). For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'drioNextToken' - The token to retrieve the next page of results.
--
-- * 'drioMinDuration' - The minimum duration (in seconds) to filter when searching for offerings. Default: 2592000 (1 month)
--
-- * 'drioAvailabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- * 'drioOfferingType' - The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
--
-- * 'drioReservedInstancesOfferingIds' - One or more Reserved Instances offering IDs.
--
-- * 'drioInstanceTenancy' - The tenancy of the instances covered by the reservation. A Reserved Instance with a tenancy of @dedicated@ is applied to instances that run in a VPC on single-tenant hardware (i.e., Dedicated Instances). __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only. Default: @default@
--
-- * 'drioOfferingClass' - The offering class of the Reserved Instance. Can be @standard@ or @convertible@ .
--
-- * 'drioMaxInstanceCount' - The maximum number of instances to filter when searching for offerings. Default: 20
--
-- * 'drioDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'drioMaxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. The maximum is 100. Default: 100
describeReservedInstancesOfferings
    :: DescribeReservedInstancesOfferings
describeReservedInstancesOfferings =
  DescribeReservedInstancesOfferings'
    { _drioMaxDuration = Nothing
    , _drioProductDescription = Nothing
    , _drioFilters = Nothing
    , _drioIncludeMarketplace = Nothing
    , _drioInstanceType = Nothing
    , _drioNextToken = Nothing
    , _drioMinDuration = Nothing
    , _drioAvailabilityZone = Nothing
    , _drioOfferingType = Nothing
    , _drioReservedInstancesOfferingIds = Nothing
    , _drioInstanceTenancy = Nothing
    , _drioOfferingClass = Nothing
    , _drioMaxInstanceCount = Nothing
    , _drioDryRun = Nothing
    , _drioMaxResults = Nothing
    }


-- | The maximum duration (in seconds) to filter when searching for offerings. Default: 94608000 (3 years)
drioMaxDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMaxDuration = lens _drioMaxDuration (\ s a -> s{_drioMaxDuration = a})

-- | The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the description are for use with Amazon VPC.
drioProductDescription :: Lens' DescribeReservedInstancesOfferings (Maybe RIProductDescription)
drioProductDescription = lens _drioProductDescription (\ s a -> s{_drioProductDescription = a})

-- | One or more filters.     * @availability-zone@ - The Availability Zone where the Reserved Instance can be used.     * @duration@ - The duration of the Reserved Instance (for example, one year or three years), in seconds (@31536000@ | @94608000@ ).     * @fixed-price@ - The purchase price of the Reserved Instance (for example, 9800.0).     * @instance-type@ - The instance type that is covered by the reservation.     * @marketplace@ - Set to @true@ to show only Reserved Instance Marketplace offerings. When this filter is not used, which is the default behavior, all offerings from both AWS and the Reserved Instance Marketplace are listed.     * @product-description@ - The Reserved Instance product platform description. Instances that include @(Amazon VPC)@ in the product platform description will only be displayed to EC2-Classic account holders and are for use with Amazon VPC. (@Linux/UNIX@ | @Linux/UNIX (Amazon VPC)@ | @SUSE Linux@ | @SUSE Linux (Amazon VPC)@ | @Red Hat Enterprise Linux@ | @Red Hat Enterprise Linux (Amazon VPC)@ | @Windows@ | @Windows (Amazon VPC)@ | @Windows with SQL Server Standard@ | @Windows with SQL Server Standard (Amazon VPC)@ | @Windows with SQL Server Web@ | @Windows with SQL Server Web (Amazon VPC)@ | @Windows with SQL Server Enterprise@ | @Windows with SQL Server Enterprise (Amazon VPC)@ )      * @reserved-instances-offering-id@ - The Reserved Instances offering ID.     * @scope@ - The scope of the Reserved Instance (@Availability Zone@ or @Region@ ).     * @usage-price@ - The usage price of the Reserved Instance, per hour (for example, 0.84).
drioFilters :: Lens' DescribeReservedInstancesOfferings [Filter]
drioFilters = lens _drioFilters (\ s a -> s{_drioFilters = a}) . _Default . _Coerce

-- | Include Reserved Instance Marketplace offerings in the response.
drioIncludeMarketplace :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
drioIncludeMarketplace = lens _drioIncludeMarketplace (\ s a -> s{_drioIncludeMarketplace = a})

-- | The instance type that the reservation will cover (for example, @m1.small@ ). For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
drioInstanceType :: Lens' DescribeReservedInstancesOfferings (Maybe InstanceType)
drioInstanceType = lens _drioInstanceType (\ s a -> s{_drioInstanceType = a})

-- | The token to retrieve the next page of results.
drioNextToken :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
drioNextToken = lens _drioNextToken (\ s a -> s{_drioNextToken = a})

-- | The minimum duration (in seconds) to filter when searching for offerings. Default: 2592000 (1 month)
drioMinDuration :: Lens' DescribeReservedInstancesOfferings (Maybe Integer)
drioMinDuration = lens _drioMinDuration (\ s a -> s{_drioMinDuration = a})

-- | The Availability Zone in which the Reserved Instance can be used.
drioAvailabilityZone :: Lens' DescribeReservedInstancesOfferings (Maybe Text)
drioAvailabilityZone = lens _drioAvailabilityZone (\ s a -> s{_drioAvailabilityZone = a})

-- | The Reserved Instance offering type. If you are using tools that predate the 2011-11-01 API version, you only have access to the @Medium Utilization@ Reserved Instance offering type.
drioOfferingType :: Lens' DescribeReservedInstancesOfferings (Maybe OfferingTypeValues)
drioOfferingType = lens _drioOfferingType (\ s a -> s{_drioOfferingType = a})

-- | One or more Reserved Instances offering IDs.
drioReservedInstancesOfferingIds :: Lens' DescribeReservedInstancesOfferings [Text]
drioReservedInstancesOfferingIds = lens _drioReservedInstancesOfferingIds (\ s a -> s{_drioReservedInstancesOfferingIds = a}) . _Default . _Coerce

-- | The tenancy of the instances covered by the reservation. A Reserved Instance with a tenancy of @dedicated@ is applied to instances that run in a VPC on single-tenant hardware (i.e., Dedicated Instances). __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only. Default: @default@
drioInstanceTenancy :: Lens' DescribeReservedInstancesOfferings (Maybe Tenancy)
drioInstanceTenancy = lens _drioInstanceTenancy (\ s a -> s{_drioInstanceTenancy = a})

-- | The offering class of the Reserved Instance. Can be @standard@ or @convertible@ .
drioOfferingClass :: Lens' DescribeReservedInstancesOfferings (Maybe OfferingClassType)
drioOfferingClass = lens _drioOfferingClass (\ s a -> s{_drioOfferingClass = a})

-- | The maximum number of instances to filter when searching for offerings. Default: 20
drioMaxInstanceCount :: Lens' DescribeReservedInstancesOfferings (Maybe Int)
drioMaxInstanceCount = lens _drioMaxInstanceCount (\ s a -> s{_drioMaxInstanceCount = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
drioDryRun :: Lens' DescribeReservedInstancesOfferings (Maybe Bool)
drioDryRun = lens _drioDryRun (\ s a -> s{_drioDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. The maximum is 100. Default: 100
drioMaxResults :: Lens' DescribeReservedInstancesOfferings (Maybe Int)
drioMaxResults = lens _drioMaxResults (\ s a -> s{_drioMaxResults = a})

instance AWSPager DescribeReservedInstancesOfferings
         where
        page rq rs
          | stop (rs ^. driorsNextToken) = Nothing
          | stop (rs ^. driorsReservedInstancesOfferings) =
            Nothing
          | otherwise =
            Just $ rq & drioNextToken .~ rs ^. driorsNextToken

instance AWSRequest
           DescribeReservedInstancesOfferings
         where
        type Rs DescribeReservedInstancesOfferings =
             DescribeReservedInstancesOfferingsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeReservedInstancesOfferingsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "reservedInstancesOfferingsSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReservedInstancesOfferings
         where

instance NFData DescribeReservedInstancesOfferings
         where

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
               "Version" =: ("2016-11-15" :: ByteString),
               "MaxDuration" =: _drioMaxDuration,
               "ProductDescription" =: _drioProductDescription,
               toQuery (toQueryList "Filter" <$> _drioFilters),
               "IncludeMarketplace" =: _drioIncludeMarketplace,
               "InstanceType" =: _drioInstanceType,
               "NextToken" =: _drioNextToken,
               "MinDuration" =: _drioMinDuration,
               "AvailabilityZone" =: _drioAvailabilityZone,
               "OfferingType" =: _drioOfferingType,
               toQuery
                 (toQueryList "ReservedInstancesOfferingId" <$>
                    _drioReservedInstancesOfferingIds),
               "InstanceTenancy" =: _drioInstanceTenancy,
               "OfferingClass" =: _drioOfferingClass,
               "MaxInstanceCount" =: _drioMaxInstanceCount,
               "DryRun" =: _drioDryRun,
               "MaxResults" =: _drioMaxResults]

-- | Contains the output of DescribeReservedInstancesOfferings.
--
--
--
-- /See:/ 'describeReservedInstancesOfferingsResponse' smart constructor.
data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse'
  { _driorsNextToken                  :: !(Maybe Text)
  , _driorsReservedInstancesOfferings :: !(Maybe [ReservedInstancesOffering])
  , _driorsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedInstancesOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'driorsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'driorsReservedInstancesOfferings' - A list of Reserved Instances offerings.
--
-- * 'driorsResponseStatus' - -- | The response status code.
describeReservedInstancesOfferingsResponse
    :: Int -- ^ 'driorsResponseStatus'
    -> DescribeReservedInstancesOfferingsResponse
describeReservedInstancesOfferingsResponse pResponseStatus_ =
  DescribeReservedInstancesOfferingsResponse'
    { _driorsNextToken = Nothing
    , _driorsReservedInstancesOfferings = Nothing
    , _driorsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
driorsNextToken :: Lens' DescribeReservedInstancesOfferingsResponse (Maybe Text)
driorsNextToken = lens _driorsNextToken (\ s a -> s{_driorsNextToken = a})

-- | A list of Reserved Instances offerings.
driorsReservedInstancesOfferings :: Lens' DescribeReservedInstancesOfferingsResponse [ReservedInstancesOffering]
driorsReservedInstancesOfferings = lens _driorsReservedInstancesOfferings (\ s a -> s{_driorsReservedInstancesOfferings = a}) . _Default . _Coerce

-- | -- | The response status code.
driorsResponseStatus :: Lens' DescribeReservedInstancesOfferingsResponse Int
driorsResponseStatus = lens _driorsResponseStatus (\ s a -> s{_driorsResponseStatus = a})

instance NFData
           DescribeReservedInstancesOfferingsResponse
         where
