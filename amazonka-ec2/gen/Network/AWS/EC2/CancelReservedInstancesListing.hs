{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
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

-- | Cancels the specified Reserved Instance listing in the Reserved Instance
-- Marketplace.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelReservedInstancesListing.html>
module Network.AWS.EC2.CancelReservedInstancesListing
    (
    -- * Request
      CancelReservedInstancesListing
    -- ** Request constructor
    , cancelReservedInstancesListing
    -- ** Request lenses
    , crilReservedInstancesListingId

    -- * Response
    , CancelReservedInstancesListingResponse
    -- ** Response constructor
    , cancelReservedInstancesListingResponse
    -- ** Response lenses
    , crilrReservedInstancesListings
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'cancelReservedInstancesListing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilReservedInstancesListingId'
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'{_crilReservedInstancesListingId :: Text} deriving (Eq, Read, Show)

-- | 'CancelReservedInstancesListing' smart constructor.
cancelReservedInstancesListing :: Text -> CancelReservedInstancesListing
cancelReservedInstancesListing pReservedInstancesListingId = CancelReservedInstancesListing'{_crilReservedInstancesListingId = pReservedInstancesListingId};

-- | The ID of the Reserved Instance listing.
crilReservedInstancesListingId :: Lens' CancelReservedInstancesListing Text
crilReservedInstancesListingId = lens _crilReservedInstancesListingId (\ s a -> s{_crilReservedInstancesListingId = a});

instance AWSRequest CancelReservedInstancesListing
         where
        type Sv CancelReservedInstancesListing = EC2
        type Rs CancelReservedInstancesListing =
             CancelReservedInstancesListingResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CancelReservedInstancesListingResponse' <$>
                   parseXMLList "item" x)

instance ToHeaders CancelReservedInstancesListing
         where
        toHeaders = const mempty

instance ToPath CancelReservedInstancesListing where
        toPath = const "/"

instance ToQuery CancelReservedInstancesListing where
        toQuery CancelReservedInstancesListing'{..}
          = mconcat
              ["Action" =:
                 ("CancelReservedInstancesListing" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "ReservedInstancesListingId" =:
                 _crilReservedInstancesListingId]

-- | /See:/ 'cancelReservedInstancesListingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilrReservedInstancesListings'
newtype CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'{_crilrReservedInstancesListings :: [ReservedInstancesListing]} deriving (Eq, Read, Show)

-- | 'CancelReservedInstancesListingResponse' smart constructor.
cancelReservedInstancesListingResponse :: CancelReservedInstancesListingResponse
cancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'{_crilrReservedInstancesListings = mempty};

-- | The Reserved Instance listing.
crilrReservedInstancesListings :: Lens' CancelReservedInstancesListingResponse [ReservedInstancesListing]
crilrReservedInstancesListings = lens _crilrReservedInstancesListings (\ s a -> s{_crilrReservedInstancesListings = a});
