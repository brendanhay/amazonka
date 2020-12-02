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
-- Module      : Network.AWS.RDS.DescribeReservedDBInstancesOfferings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved DB instance offerings.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
    (
    -- * Creating a Request
      describeReservedDBInstancesOfferings
    , DescribeReservedDBInstancesOfferings
    -- * Request Lenses
    , drdioProductDescription
    , drdioFilters
    , drdioDBInstanceClass
    , drdioMarker
    , drdioMaxRecords
    , drdioMultiAZ
    , drdioReservedDBInstancesOfferingId
    , drdioOfferingType
    , drdioDuration

    -- * Destructuring the Response
    , describeReservedDBInstancesOfferingsResponse
    , DescribeReservedDBInstancesOfferingsResponse
    -- * Response Lenses
    , drdiorsMarker
    , drdiorsReservedDBInstancesOfferings
    , drdiorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeReservedDBInstancesOfferings' smart constructor.
data DescribeReservedDBInstancesOfferings = DescribeReservedDBInstancesOfferings'
  { _drdioProductDescription            :: !(Maybe Text)
  , _drdioFilters                       :: !(Maybe [Filter])
  , _drdioDBInstanceClass               :: !(Maybe Text)
  , _drdioMarker                        :: !(Maybe Text)
  , _drdioMaxRecords                    :: !(Maybe Int)
  , _drdioMultiAZ                       :: !(Maybe Bool)
  , _drdioReservedDBInstancesOfferingId :: !(Maybe Text)
  , _drdioOfferingType                  :: !(Maybe Text)
  , _drdioDuration                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedDBInstancesOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdioProductDescription' - Product description filter value. Specify this parameter to show only the available offerings matching the specified product description.
--
-- * 'drdioFilters' - This parameter is not currently supported.
--
-- * 'drdioDBInstanceClass' - The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
--
-- * 'drdioMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drdioMaxRecords' - The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so that the following results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'drdioMultiAZ' - The Multi-AZ filter value. Specify this parameter to show only the available offerings matching the specified Multi-AZ parameter.
--
-- * 'drdioReservedDBInstancesOfferingId' - The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier. Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
--
-- * 'drdioOfferingType' - The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type. Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
--
-- * 'drdioDuration' - Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration. Valid Values: @1 | 3 | 31536000 | 94608000@
describeReservedDBInstancesOfferings
    :: DescribeReservedDBInstancesOfferings
describeReservedDBInstancesOfferings =
  DescribeReservedDBInstancesOfferings'
    { _drdioProductDescription = Nothing
    , _drdioFilters = Nothing
    , _drdioDBInstanceClass = Nothing
    , _drdioMarker = Nothing
    , _drdioMaxRecords = Nothing
    , _drdioMultiAZ = Nothing
    , _drdioReservedDBInstancesOfferingId = Nothing
    , _drdioOfferingType = Nothing
    , _drdioDuration = Nothing
    }


-- | Product description filter value. Specify this parameter to show only the available offerings matching the specified product description.
drdioProductDescription :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioProductDescription = lens _drdioProductDescription (\ s a -> s{_drdioProductDescription = a})

-- | This parameter is not currently supported.
drdioFilters :: Lens' DescribeReservedDBInstancesOfferings [Filter]
drdioFilters = lens _drdioFilters (\ s a -> s{_drdioFilters = a}) . _Default . _Coerce

-- | The DB instance class filter value. Specify this parameter to show only the available offerings matching the specified DB instance class.
drdioDBInstanceClass :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioDBInstanceClass = lens _drdioDBInstanceClass (\ s a -> s{_drdioDBInstanceClass = a})

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drdioMarker :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioMarker = lens _drdioMarker (\ s a -> s{_drdioMarker = a})

-- | The maximum number of records to include in the response. If more than the @MaxRecords@ value is available, a pagination token called a marker is included in the response so that the following results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
drdioMaxRecords :: Lens' DescribeReservedDBInstancesOfferings (Maybe Int)
drdioMaxRecords = lens _drdioMaxRecords (\ s a -> s{_drdioMaxRecords = a})

-- | The Multi-AZ filter value. Specify this parameter to show only the available offerings matching the specified Multi-AZ parameter.
drdioMultiAZ :: Lens' DescribeReservedDBInstancesOfferings (Maybe Bool)
drdioMultiAZ = lens _drdioMultiAZ (\ s a -> s{_drdioMultiAZ = a})

-- | The offering identifier filter value. Specify this parameter to show only the available offering that matches the specified reservation identifier. Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
drdioReservedDBInstancesOfferingId :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioReservedDBInstancesOfferingId = lens _drdioReservedDBInstancesOfferingId (\ s a -> s{_drdioReservedDBInstancesOfferingId = a})

-- | The offering type filter value. Specify this parameter to show only the available offerings matching the specified offering type. Valid Values: @"Partial Upfront" | "All Upfront" | "No Upfront" @
drdioOfferingType :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioOfferingType = lens _drdioOfferingType (\ s a -> s{_drdioOfferingType = a})

-- | Duration filter value, specified in years or seconds. Specify this parameter to show only reservations for this duration. Valid Values: @1 | 3 | 31536000 | 94608000@
drdioDuration :: Lens' DescribeReservedDBInstancesOfferings (Maybe Text)
drdioDuration = lens _drdioDuration (\ s a -> s{_drdioDuration = a})

instance AWSPager
           DescribeReservedDBInstancesOfferings
         where
        page rq rs
          | stop (rs ^. drdiorsMarker) = Nothing
          | stop (rs ^. drdiorsReservedDBInstancesOfferings) =
            Nothing
          | otherwise =
            Just $ rq & drdioMarker .~ rs ^. drdiorsMarker

instance AWSRequest
           DescribeReservedDBInstancesOfferings
         where
        type Rs DescribeReservedDBInstancesOfferings =
             DescribeReservedDBInstancesOfferingsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeReservedDBInstancesOfferingsResult"
              (\ s h x ->
                 DescribeReservedDBInstancesOfferingsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "ReservedDBInstancesOfferings" .!@ mempty >>=
                        may (parseXMLList "ReservedDBInstancesOffering"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeReservedDBInstancesOfferings
         where

instance NFData DescribeReservedDBInstancesOfferings
         where

instance ToHeaders
           DescribeReservedDBInstancesOfferings
         where
        toHeaders = const mempty

instance ToPath DescribeReservedDBInstancesOfferings
         where
        toPath = const "/"

instance ToQuery DescribeReservedDBInstancesOfferings
         where
        toQuery DescribeReservedDBInstancesOfferings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedDBInstancesOfferings" ::
                    ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ProductDescription" =: _drdioProductDescription,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _drdioFilters),
               "DBInstanceClass" =: _drdioDBInstanceClass,
               "Marker" =: _drdioMarker,
               "MaxRecords" =: _drdioMaxRecords,
               "MultiAZ" =: _drdioMultiAZ,
               "ReservedDBInstancesOfferingId" =:
                 _drdioReservedDBInstancesOfferingId,
               "OfferingType" =: _drdioOfferingType,
               "Duration" =: _drdioDuration]

-- | Contains the result of a successful invocation of the 'DescribeReservedDBInstancesOfferings' action.
--
--
--
-- /See:/ 'describeReservedDBInstancesOfferingsResponse' smart constructor.
data DescribeReservedDBInstancesOfferingsResponse = DescribeReservedDBInstancesOfferingsResponse'
  { _drdiorsMarker :: !(Maybe Text)
  , _drdiorsReservedDBInstancesOfferings :: !(Maybe [ReservedDBInstancesOffering])
  , _drdiorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedDBInstancesOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdiorsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drdiorsReservedDBInstancesOfferings' - A list of reserved DB instance offerings.
--
-- * 'drdiorsResponseStatus' - -- | The response status code.
describeReservedDBInstancesOfferingsResponse
    :: Int -- ^ 'drdiorsResponseStatus'
    -> DescribeReservedDBInstancesOfferingsResponse
describeReservedDBInstancesOfferingsResponse pResponseStatus_ =
  DescribeReservedDBInstancesOfferingsResponse'
    { _drdiorsMarker = Nothing
    , _drdiorsReservedDBInstancesOfferings = Nothing
    , _drdiorsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drdiorsMarker :: Lens' DescribeReservedDBInstancesOfferingsResponse (Maybe Text)
drdiorsMarker = lens _drdiorsMarker (\ s a -> s{_drdiorsMarker = a})

-- | A list of reserved DB instance offerings.
drdiorsReservedDBInstancesOfferings :: Lens' DescribeReservedDBInstancesOfferingsResponse [ReservedDBInstancesOffering]
drdiorsReservedDBInstancesOfferings = lens _drdiorsReservedDBInstancesOfferings (\ s a -> s{_drdiorsReservedDBInstancesOfferings = a}) . _Default . _Coerce

-- | -- | The response status code.
drdiorsResponseStatus :: Lens' DescribeReservedDBInstancesOfferingsResponse Int
drdiorsResponseStatus = lens _drdiorsResponseStatus (\ s a -> s{_drdiorsResponseStatus = a})

instance NFData
           DescribeReservedDBInstancesOfferingsResponse
         where
