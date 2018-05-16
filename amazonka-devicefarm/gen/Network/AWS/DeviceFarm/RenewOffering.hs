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
-- Module      : Network.AWS.DeviceFarm.RenewOffering
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Explicitly sets the quantity of devices to renew for an offering, starting from the @effectiveDate@ of the next period. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. Please contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you believe that you should be able to invoke this operation.
--
--
module Network.AWS.DeviceFarm.RenewOffering
    (
    -- * Creating a Request
      renewOffering
    , RenewOffering
    -- * Request Lenses
    , roQuantity
    , roOfferingId

    -- * Destructuring the Response
    , renewOfferingResponse
    , RenewOfferingResponse
    -- * Response Lenses
    , rorsOfferingTransaction
    , rorsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request representing an offering renewal.
--
--
--
-- /See:/ 'renewOffering' smart constructor.
data RenewOffering = RenewOffering'
  { _roQuantity   :: !(Maybe Int)
  , _roOfferingId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roQuantity' - The quantity requested in an offering renewal.
--
-- * 'roOfferingId' - The ID of a request to renew an offering.
renewOffering
    :: RenewOffering
renewOffering = RenewOffering' {_roQuantity = Nothing, _roOfferingId = Nothing}


-- | The quantity requested in an offering renewal.
roQuantity :: Lens' RenewOffering (Maybe Int)
roQuantity = lens _roQuantity (\ s a -> s{_roQuantity = a})

-- | The ID of a request to renew an offering.
roOfferingId :: Lens' RenewOffering (Maybe Text)
roOfferingId = lens _roOfferingId (\ s a -> s{_roOfferingId = a})

instance AWSRequest RenewOffering where
        type Rs RenewOffering = RenewOfferingResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 RenewOfferingResponse' <$>
                   (x .?> "offeringTransaction") <*>
                     (pure (fromEnum s)))

instance Hashable RenewOffering where

instance NFData RenewOffering where

instance ToHeaders RenewOffering where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.RenewOffering" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RenewOffering where
        toJSON RenewOffering'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _roQuantity,
                  ("offeringId" .=) <$> _roOfferingId])

instance ToPath RenewOffering where
        toPath = const "/"

instance ToQuery RenewOffering where
        toQuery = const mempty

-- | The result of a renewal offering.
--
--
--
-- /See:/ 'renewOfferingResponse' smart constructor.
data RenewOfferingResponse = RenewOfferingResponse'
  { _rorsOfferingTransaction :: !(Maybe OfferingTransaction)
  , _rorsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rorsOfferingTransaction' - Represents the status of the offering transaction for the renewal.
--
-- * 'rorsResponseStatus' - -- | The response status code.
renewOfferingResponse
    :: Int -- ^ 'rorsResponseStatus'
    -> RenewOfferingResponse
renewOfferingResponse pResponseStatus_ =
  RenewOfferingResponse'
    {_rorsOfferingTransaction = Nothing, _rorsResponseStatus = pResponseStatus_}


-- | Represents the status of the offering transaction for the renewal.
rorsOfferingTransaction :: Lens' RenewOfferingResponse (Maybe OfferingTransaction)
rorsOfferingTransaction = lens _rorsOfferingTransaction (\ s a -> s{_rorsOfferingTransaction = a})

-- | -- | The response status code.
rorsResponseStatus :: Lens' RenewOfferingResponse Int
rorsResponseStatus = lens _rorsResponseStatus (\ s a -> s{_rorsResponseStatus = a})

instance NFData RenewOfferingResponse where
