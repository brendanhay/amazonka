{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Reserved Instance listing in the Reserved Instance
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
    , crilrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cancelReservedInstancesListing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilReservedInstancesListingId'
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'
    { _crilReservedInstancesListingId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelReservedInstancesListing' smart constructor.
cancelReservedInstancesListing :: Text -> CancelReservedInstancesListing
cancelReservedInstancesListing pReservedInstancesListingId =
    CancelReservedInstancesListing'
    { _crilReservedInstancesListingId = pReservedInstancesListingId
    }

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
                   (x .@? "reservedInstancesListingsSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
--
-- * 'crilrStatus'
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
    { _crilrReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
    , _crilrStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelReservedInstancesListingResponse' smart constructor.
cancelReservedInstancesListingResponse :: Int -> CancelReservedInstancesListingResponse
cancelReservedInstancesListingResponse pStatus =
    CancelReservedInstancesListingResponse'
    { _crilrReservedInstancesListings = Nothing
    , _crilrStatus = pStatus
    }

-- | The Reserved Instance listing.
crilrReservedInstancesListings :: Lens' CancelReservedInstancesListingResponse [ReservedInstancesListing]
crilrReservedInstancesListings = lens _crilrReservedInstancesListings (\ s a -> s{_crilrReservedInstancesListings = a}) . _Default;

-- | FIXME: Undocumented member.
crilrStatus :: Lens' CancelReservedInstancesListingResponse Int
crilrStatus = lens _crilrStatus (\ s a -> s{_crilrStatus = a});
