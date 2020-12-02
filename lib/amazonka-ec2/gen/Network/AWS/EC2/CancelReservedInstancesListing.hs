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
-- Module      : Network.AWS.EC2.CancelReservedInstancesListing
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Reserved Instance listing in the Reserved Instance Marketplace.
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved Instance Marketplace> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CancelReservedInstancesListing
    (
    -- * Creating a Request
      cancelReservedInstancesListing
    , CancelReservedInstancesListing
    -- * Request Lenses
    , crilReservedInstancesListingId

    -- * Destructuring the Response
    , cancelReservedInstancesListingResponse
    , CancelReservedInstancesListingResponse
    -- * Response Lenses
    , crilrsReservedInstancesListings
    , crilrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CancelReservedInstancesListing.
--
--
--
-- /See:/ 'cancelReservedInstancesListing' smart constructor.
newtype CancelReservedInstancesListing = CancelReservedInstancesListing'
  { _crilReservedInstancesListingId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelReservedInstancesListing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crilReservedInstancesListingId' - The ID of the Reserved Instance listing.
cancelReservedInstancesListing
    :: Text -- ^ 'crilReservedInstancesListingId'
    -> CancelReservedInstancesListing
cancelReservedInstancesListing pReservedInstancesListingId_ =
  CancelReservedInstancesListing'
    {_crilReservedInstancesListingId = pReservedInstancesListingId_}


-- | The ID of the Reserved Instance listing.
crilReservedInstancesListingId :: Lens' CancelReservedInstancesListing Text
crilReservedInstancesListingId = lens _crilReservedInstancesListingId (\ s a -> s{_crilReservedInstancesListingId = a})

instance AWSRequest CancelReservedInstancesListing
         where
        type Rs CancelReservedInstancesListing =
             CancelReservedInstancesListingResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CancelReservedInstancesListingResponse' <$>
                   (x .@? "reservedInstancesListingsSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable CancelReservedInstancesListing
         where

instance NFData CancelReservedInstancesListing where

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
               "Version" =: ("2016-11-15" :: ByteString),
               "ReservedInstancesListingId" =:
                 _crilReservedInstancesListingId]

-- | Contains the output of CancelReservedInstancesListing.
--
--
--
-- /See:/ 'cancelReservedInstancesListingResponse' smart constructor.
data CancelReservedInstancesListingResponse = CancelReservedInstancesListingResponse'
  { _crilrsReservedInstancesListings :: !(Maybe [ReservedInstancesListing])
  , _crilrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelReservedInstancesListingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crilrsReservedInstancesListings' - The Reserved Instance listing.
--
-- * 'crilrsResponseStatus' - -- | The response status code.
cancelReservedInstancesListingResponse
    :: Int -- ^ 'crilrsResponseStatus'
    -> CancelReservedInstancesListingResponse
cancelReservedInstancesListingResponse pResponseStatus_ =
  CancelReservedInstancesListingResponse'
    { _crilrsReservedInstancesListings = Nothing
    , _crilrsResponseStatus = pResponseStatus_
    }


-- | The Reserved Instance listing.
crilrsReservedInstancesListings :: Lens' CancelReservedInstancesListingResponse [ReservedInstancesListing]
crilrsReservedInstancesListings = lens _crilrsReservedInstancesListings (\ s a -> s{_crilrsReservedInstancesListings = a}) . _Default . _Coerce

-- | -- | The response status code.
crilrsResponseStatus :: Lens' CancelReservedInstancesListingResponse Int
crilrsResponseStatus = lens _crilrsResponseStatus (\ s a -> s{_crilrsResponseStatus = a})

instance NFData
           CancelReservedInstancesListingResponse
         where
