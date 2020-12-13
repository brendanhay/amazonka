{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.PurchaseOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately purchases offerings for an AWS account. Offerings renew with the latest total purchased quantity for an offering, unless the renewal was overridden. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
module Network.AWS.DeviceFarm.PurchaseOffering
  ( -- * Creating a request
    PurchaseOffering (..),
    mkPurchaseOffering,

    -- ** Request lenses
    poQuantity,
    poOfferingId,
    poOfferingPromotionId,

    -- * Destructuring the response
    PurchaseOfferingResponse (..),
    mkPurchaseOfferingResponse,

    -- ** Response lenses
    porsOfferingTransaction,
    porsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request for a purchase offering.
--
-- /See:/ 'mkPurchaseOffering' smart constructor.
data PurchaseOffering = PurchaseOffering'
  { -- | The number of device slots to purchase in an offering request.
    quantity :: Lude.Maybe Lude.Int,
    -- | The ID of the offering.
    offeringId :: Lude.Maybe Lude.Text,
    -- | The ID of the offering promotion to be applied to the purchase.
    offeringPromotionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseOffering' with the minimum fields required to make a request.
--
-- * 'quantity' - The number of device slots to purchase in an offering request.
-- * 'offeringId' - The ID of the offering.
-- * 'offeringPromotionId' - The ID of the offering promotion to be applied to the purchase.
mkPurchaseOffering ::
  PurchaseOffering
mkPurchaseOffering =
  PurchaseOffering'
    { quantity = Lude.Nothing,
      offeringId = Lude.Nothing,
      offeringPromotionId = Lude.Nothing
    }

-- | The number of device slots to purchase in an offering request.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poQuantity :: Lens.Lens' PurchaseOffering (Lude.Maybe Lude.Int)
poQuantity = Lens.lens (quantity :: PurchaseOffering -> Lude.Maybe Lude.Int) (\s a -> s {quantity = a} :: PurchaseOffering)
{-# DEPRECATED poQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The ID of the offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOfferingId :: Lens.Lens' PurchaseOffering (Lude.Maybe Lude.Text)
poOfferingId = Lens.lens (offeringId :: PurchaseOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: PurchaseOffering)
{-# DEPRECATED poOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | The ID of the offering promotion to be applied to the purchase.
--
-- /Note:/ Consider using 'offeringPromotionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOfferingPromotionId :: Lens.Lens' PurchaseOffering (Lude.Maybe Lude.Text)
poOfferingPromotionId = Lens.lens (offeringPromotionId :: PurchaseOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringPromotionId = a} :: PurchaseOffering)
{-# DEPRECATED poOfferingPromotionId "Use generic-lens or generic-optics with 'offeringPromotionId' instead." #-}

instance Lude.AWSRequest PurchaseOffering where
  type Rs PurchaseOffering = PurchaseOfferingResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          PurchaseOfferingResponse'
            Lude.<$> (x Lude..?> "offeringTransaction")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseOffering where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.PurchaseOffering" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PurchaseOffering where
  toJSON PurchaseOffering' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("quantity" Lude..=) Lude.<$> quantity,
            ("offeringId" Lude..=) Lude.<$> offeringId,
            ("offeringPromotionId" Lude..=) Lude.<$> offeringPromotionId
          ]
      )

instance Lude.ToPath PurchaseOffering where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseOffering where
  toQuery = Lude.const Lude.mempty

-- | The result of the purchase offering (for example, success or failure).
--
-- /See:/ 'mkPurchaseOfferingResponse' smart constructor.
data PurchaseOfferingResponse = PurchaseOfferingResponse'
  { -- | Represents the offering transaction for the purchase result.
    offeringTransaction :: Lude.Maybe OfferingTransaction,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseOfferingResponse' with the minimum fields required to make a request.
--
-- * 'offeringTransaction' - Represents the offering transaction for the purchase result.
-- * 'responseStatus' - The response status code.
mkPurchaseOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseOfferingResponse
mkPurchaseOfferingResponse pResponseStatus_ =
  PurchaseOfferingResponse'
    { offeringTransaction = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the offering transaction for the purchase result.
--
-- /Note:/ Consider using 'offeringTransaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsOfferingTransaction :: Lens.Lens' PurchaseOfferingResponse (Lude.Maybe OfferingTransaction)
porsOfferingTransaction = Lens.lens (offeringTransaction :: PurchaseOfferingResponse -> Lude.Maybe OfferingTransaction) (\s a -> s {offeringTransaction = a} :: PurchaseOfferingResponse)
{-# DEPRECATED porsOfferingTransaction "Use generic-lens or generic-optics with 'offeringTransaction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsResponseStatus :: Lens.Lens' PurchaseOfferingResponse Lude.Int
porsResponseStatus = Lens.lens (responseStatus :: PurchaseOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseOfferingResponse)
{-# DEPRECATED porsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
