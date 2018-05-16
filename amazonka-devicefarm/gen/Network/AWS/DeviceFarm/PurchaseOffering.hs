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
-- Module      : Network.AWS.DeviceFarm.PurchaseOffering
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. Please contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> if you believe that you should be able to invoke this operation.
--
--
module Network.AWS.DeviceFarm.PurchaseOffering
    (
    -- * Creating a Request
      purchaseOffering
    , PurchaseOffering
    -- * Request Lenses
    , poQuantity
    , poOfferingId
    , poOfferingPromotionId

    -- * Destructuring the Response
    , purchaseOfferingResponse
    , PurchaseOfferingResponse
    -- * Response Lenses
    , porsOfferingTransaction
    , porsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request for a purchase offering.
--
--
--
-- /See:/ 'purchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { _poQuantity            :: !(Maybe Int)
  , _poOfferingId          :: !(Maybe Text)
  , _poOfferingPromotionId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poQuantity' - The number of device slots you wish to purchase in an offering request.
--
-- * 'poOfferingId' - The ID of the offering.
--
-- * 'poOfferingPromotionId' - The ID of the offering promotion to be applied to the purchase.
purchaseOffering
    :: PurchaseOffering
purchaseOffering =
  PurchaseOffering'
    { _poQuantity = Nothing
    , _poOfferingId = Nothing
    , _poOfferingPromotionId = Nothing
    }


-- | The number of device slots you wish to purchase in an offering request.
poQuantity :: Lens' PurchaseOffering (Maybe Int)
poQuantity = lens _poQuantity (\ s a -> s{_poQuantity = a})

-- | The ID of the offering.
poOfferingId :: Lens' PurchaseOffering (Maybe Text)
poOfferingId = lens _poOfferingId (\ s a -> s{_poOfferingId = a})

-- | The ID of the offering promotion to be applied to the purchase.
poOfferingPromotionId :: Lens' PurchaseOffering (Maybe Text)
poOfferingPromotionId = lens _poOfferingPromotionId (\ s a -> s{_poOfferingPromotionId = a})

instance AWSRequest PurchaseOffering where
        type Rs PurchaseOffering = PurchaseOfferingResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 PurchaseOfferingResponse' <$>
                   (x .?> "offeringTransaction") <*>
                     (pure (fromEnum s)))

instance Hashable PurchaseOffering where

instance NFData PurchaseOffering where

instance ToHeaders PurchaseOffering where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.PurchaseOffering" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PurchaseOffering where
        toJSON PurchaseOffering'{..}
          = object
              (catMaybes
                 [("quantity" .=) <$> _poQuantity,
                  ("offeringId" .=) <$> _poOfferingId,
                  ("offeringPromotionId" .=) <$>
                    _poOfferingPromotionId])

instance ToPath PurchaseOffering where
        toPath = const "/"

instance ToQuery PurchaseOffering where
        toQuery = const mempty

-- | The result of the purchase offering (e.g., success or failure).
--
--
--
-- /See:/ 'purchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { _porsOfferingTransaction :: !(Maybe OfferingTransaction)
  , _porsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'porsOfferingTransaction' - Represents the offering transaction for the purchase result.
--
-- * 'porsResponseStatus' - -- | The response status code.
purchaseOfferingResponse
    :: Int -- ^ 'porsResponseStatus'
    -> PurchaseOfferingResponse
purchaseOfferingResponse pResponseStatus_ =
  PurchaseOfferingResponse'
    {_porsOfferingTransaction = Nothing, _porsResponseStatus = pResponseStatus_}


-- | Represents the offering transaction for the purchase result.
porsOfferingTransaction :: Lens' PurchaseOfferingResponse (Maybe OfferingTransaction)
porsOfferingTransaction = lens _porsOfferingTransaction (\ s a -> s{_porsOfferingTransaction = a})

-- | -- | The response status code.
porsResponseStatus :: Lens' PurchaseOfferingResponse Int
porsResponseStatus = lens _porsResponseStatus (\ s a -> s{_porsResponseStatus = a})

instance NFData PurchaseOfferingResponse where
