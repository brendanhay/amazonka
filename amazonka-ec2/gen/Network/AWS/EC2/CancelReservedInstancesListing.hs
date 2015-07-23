{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , crilrqReservedInstancesListingId

    -- * Response
    , CancelReservedInstancesListingResponse
    -- ** Response constructor
    , cancelReservedInstancesListingResponse
    -- ** Response lenses
    , crilrsReservedInstancesListings
    , crilrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cancelReservedInstancesListing' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilrqReservedInstancesListingId'
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'
    { _crilrqReservedInstancesListingId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelReservedInstancesListing' smart constructor.
cancelReservedInstancesListing :: Text -> CancelReservedInstancesListing
cancelReservedInstancesListing pReservedInstancesListingId_ =
    CancelReservedInstancesListing'
    { _crilrqReservedInstancesListingId = pReservedInstancesListingId_
    }

-- | The ID of the Reserved Instance listing.
crilrqReservedInstancesListingId :: Lens' CancelReservedInstancesListing Text
crilrqReservedInstancesListingId = lens _crilrqReservedInstancesListingId (\ s a -> s{_crilrqReservedInstancesListingId = a});

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
                 _crilrqReservedInstancesListingId]

-- | /See:/ 'cancelReservedInstancesListingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crilrsReservedInstancesListings'
--
-- * 'crilrsStatus'
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
    { _crilrsReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
    , _crilrsStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelReservedInstancesListingResponse' smart constructor.
cancelReservedInstancesListingResponse :: Int -> CancelReservedInstancesListingResponse
cancelReservedInstancesListingResponse pStatus_ =
    CancelReservedInstancesListingResponse'
    { _crilrsReservedInstancesListings = Nothing
    , _crilrsStatus = pStatus_
    }

-- | The Reserved Instance listing.
crilrsReservedInstancesListings :: Lens' CancelReservedInstancesListingResponse [ReservedInstancesListing]
crilrsReservedInstancesListings = lens _crilrsReservedInstancesListings (\ s a -> s{_crilrsReservedInstancesListings = a}) . _Default;

-- | FIXME: Undocumented member.
crilrsStatus :: Lens' CancelReservedInstancesListingResponse Int
crilrsStatus = lens _crilrsStatus (\ s a -> s{_crilrsStatus = a});
