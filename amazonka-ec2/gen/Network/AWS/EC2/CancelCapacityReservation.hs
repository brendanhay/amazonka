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
-- Module      : Network.AWS.EC2.CancelCapacityReservation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Capacity Reservation, releases the reserved capacity, and changes the Capacity Reservation's state to @cancelled@ .
--
--
-- Instances running in the reserved capacity continue running until you stop them. Stopped instances that target the Capacity Reservation can no longer launch. Modify these instances to either target a different Capacity Reservation, launch On-Demand Instance capacity, or run in any open Capacity Reservation that has matching attributes and sufficient capacity.
--
module Network.AWS.EC2.CancelCapacityReservation
    (
    -- * Creating a Request
      cancelCapacityReservation
    , CancelCapacityReservation
    -- * Request Lenses
    , canDryRun
    , canCapacityReservationId

    -- * Destructuring the Response
    , cancelCapacityReservationResponse
    , CancelCapacityReservationResponse
    -- * Response Lenses
    , canrsReturn
    , canrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelCapacityReservation' smart constructor.
data CancelCapacityReservation = CancelCapacityReservation'
  { _canDryRun                :: !(Maybe Bool)
  , _canCapacityReservationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCapacityReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'canDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'canCapacityReservationId' - The ID of the Capacity Reservation to be cancelled.
cancelCapacityReservation
    :: Text -- ^ 'canCapacityReservationId'
    -> CancelCapacityReservation
cancelCapacityReservation pCapacityReservationId_ =
  CancelCapacityReservation'
    {_canDryRun = Nothing, _canCapacityReservationId = pCapacityReservationId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
canDryRun :: Lens' CancelCapacityReservation (Maybe Bool)
canDryRun = lens _canDryRun (\ s a -> s{_canDryRun = a})

-- | The ID of the Capacity Reservation to be cancelled.
canCapacityReservationId :: Lens' CancelCapacityReservation Text
canCapacityReservationId = lens _canCapacityReservationId (\ s a -> s{_canCapacityReservationId = a})

instance AWSRequest CancelCapacityReservation where
        type Rs CancelCapacityReservation =
             CancelCapacityReservationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CancelCapacityReservationResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable CancelCapacityReservation where

instance NFData CancelCapacityReservation where

instance ToHeaders CancelCapacityReservation where
        toHeaders = const mempty

instance ToPath CancelCapacityReservation where
        toPath = const "/"

instance ToQuery CancelCapacityReservation where
        toQuery CancelCapacityReservation'{..}
          = mconcat
              ["Action" =:
                 ("CancelCapacityReservation" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _canDryRun,
               "CapacityReservationId" =: _canCapacityReservationId]

-- | /See:/ 'cancelCapacityReservationResponse' smart constructor.
data CancelCapacityReservationResponse = CancelCapacityReservationResponse'
  { _canrsReturn         :: !(Maybe Bool)
  , _canrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelCapacityReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'canrsReturn' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'canrsResponseStatus' - -- | The response status code.
cancelCapacityReservationResponse
    :: Int -- ^ 'canrsResponseStatus'
    -> CancelCapacityReservationResponse
cancelCapacityReservationResponse pResponseStatus_ =
  CancelCapacityReservationResponse'
    {_canrsReturn = Nothing, _canrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
canrsReturn :: Lens' CancelCapacityReservationResponse (Maybe Bool)
canrsReturn = lens _canrsReturn (\ s a -> s{_canrsReturn = a})

-- | -- | The response status code.
canrsResponseStatus :: Lens' CancelCapacityReservationResponse Int
canrsResponseStatus = lens _canrsResponseStatus (\ s a -> s{_canrsResponseStatus = a})

instance NFData CancelCapacityReservationResponse
         where
