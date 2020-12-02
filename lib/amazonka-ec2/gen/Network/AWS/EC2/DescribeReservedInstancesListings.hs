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
-- Module      : Network.AWS.EC2.DescribeReservedInstancesListings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your account's Reserved Instance listings in the Reserved Instance Marketplace.
--
--
-- The Reserved Instance Marketplace matches sellers who want to resell Reserved Instance capacity that they no longer need with buyers who want to purchase additional capacity. Reserved Instances bought and sold through the Reserved Instance Marketplace work like any other Reserved Instances.
--
-- As a seller, you choose to list some or all of your Reserved Instances, and you specify the upfront price to receive for them. Your Reserved Instances are then listed in the Reserved Instance Marketplace and are available for purchase.
--
-- As a buyer, you specify the configuration of the Reserved Instance to purchase, and the Marketplace matches what you're searching for with what's available. The Marketplace first sells the lowest priced Reserved Instances to you, and continues to sell available Reserved Instance listings to you until your demand is met. You are charged based on the total price of all of the listings that you purchase.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeReservedInstancesListings
    (
    -- * Creating a Request
      describeReservedInstancesListings
    , DescribeReservedInstancesListings
    -- * Request Lenses
    , drilFilters
    , drilReservedInstancesId
    , drilReservedInstancesListingId

    -- * Destructuring the Response
    , describeReservedInstancesListingsResponse
    , DescribeReservedInstancesListingsResponse
    -- * Response Lenses
    , drilrsReservedInstancesListings
    , drilrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeReservedInstancesListings.
--
--
--
-- /See:/ 'describeReservedInstancesListings' smart constructor.
data DescribeReservedInstancesListings = DescribeReservedInstancesListings'
  { _drilFilters                    :: !(Maybe [Filter])
  , _drilReservedInstancesId        :: !(Maybe Text)
  , _drilReservedInstancesListingId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedInstancesListings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drilFilters' - One or more filters.     * @reserved-instances-id@ - The ID of the Reserved Instances.     * @reserved-instances-listing-id@ - The ID of the Reserved Instances listing.     * @status@ - The status of the Reserved Instance listing (@pending@ | @active@ | @cancelled@ | @closed@ ).     * @status-message@ - The reason for the status.
--
-- * 'drilReservedInstancesId' - One or more Reserved Instance IDs.
--
-- * 'drilReservedInstancesListingId' - One or more Reserved Instance listing IDs.
describeReservedInstancesListings
    :: DescribeReservedInstancesListings
describeReservedInstancesListings =
  DescribeReservedInstancesListings'
    { _drilFilters = Nothing
    , _drilReservedInstancesId = Nothing
    , _drilReservedInstancesListingId = Nothing
    }


-- | One or more filters.     * @reserved-instances-id@ - The ID of the Reserved Instances.     * @reserved-instances-listing-id@ - The ID of the Reserved Instances listing.     * @status@ - The status of the Reserved Instance listing (@pending@ | @active@ | @cancelled@ | @closed@ ).     * @status-message@ - The reason for the status.
drilFilters :: Lens' DescribeReservedInstancesListings [Filter]
drilFilters = lens _drilFilters (\ s a -> s{_drilFilters = a}) . _Default . _Coerce

-- | One or more Reserved Instance IDs.
drilReservedInstancesId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesId = lens _drilReservedInstancesId (\ s a -> s{_drilReservedInstancesId = a})

-- | One or more Reserved Instance listing IDs.
drilReservedInstancesListingId :: Lens' DescribeReservedInstancesListings (Maybe Text)
drilReservedInstancesListingId = lens _drilReservedInstancesListingId (\ s a -> s{_drilReservedInstancesListingId = a})

instance AWSRequest DescribeReservedInstancesListings
         where
        type Rs DescribeReservedInstancesListings =
             DescribeReservedInstancesListingsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeReservedInstancesListingsResponse' <$>
                   (x .@? "reservedInstancesListingsSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReservedInstancesListings
         where

instance NFData DescribeReservedInstancesListings
         where

instance ToHeaders DescribeReservedInstancesListings
         where
        toHeaders = const mempty

instance ToPath DescribeReservedInstancesListings
         where
        toPath = const "/"

instance ToQuery DescribeReservedInstancesListings
         where
        toQuery DescribeReservedInstancesListings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedInstancesListings" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _drilFilters),
               "ReservedInstancesId" =: _drilReservedInstancesId,
               "ReservedInstancesListingId" =:
                 _drilReservedInstancesListingId]

-- | Contains the output of DescribeReservedInstancesListings.
--
--
--
-- /See:/ 'describeReservedInstancesListingsResponse' smart constructor.
data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse'
  { _drilrsReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
  , _drilrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedInstancesListingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drilrsReservedInstancesListings' - Information about the Reserved Instance listing.
--
-- * 'drilrsResponseStatus' - -- | The response status code.
describeReservedInstancesListingsResponse
    :: Int -- ^ 'drilrsResponseStatus'
    -> DescribeReservedInstancesListingsResponse
describeReservedInstancesListingsResponse pResponseStatus_ =
  DescribeReservedInstancesListingsResponse'
    { _drilrsReservedInstancesListings = Nothing
    , _drilrsResponseStatus = pResponseStatus_
    }


-- | Information about the Reserved Instance listing.
drilrsReservedInstancesListings :: Lens' DescribeReservedInstancesListingsResponse [ReservedInstancesListing]
drilrsReservedInstancesListings = lens _drilrsReservedInstancesListings (\ s a -> s{_drilrsReservedInstancesListings = a}) . _Default . _Coerce

-- | -- | The response status code.
drilrsResponseStatus :: Lens' DescribeReservedInstancesListingsResponse Int
drilrsResponseStatus = lens _drilrsResponseStatus (\ s a -> s{_drilrsResponseStatus = a})

instance NFData
           DescribeReservedInstancesListingsResponse
         where
