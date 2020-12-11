{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.RenewOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Explicitly sets the quantity of devices to renew for an offering, starting from the @effectiveDate@ of the next period. The API returns a @NotEligible@ error if the user is not permitted to invoke the operation. If you must be able to invoke this operation, contact <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support@amazon.com> .
module Network.AWS.DeviceFarm.RenewOffering
  ( -- * Creating a request
    RenewOffering (..),
    mkRenewOffering,

    -- ** Request lenses
    roQuantity,
    roOfferingId,

    -- * Destructuring the response
    RenewOfferingResponse (..),
    mkRenewOfferingResponse,

    -- ** Response lenses
    rorsOfferingTransaction,
    rorsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request that represents an offering renewal.
--
-- /See:/ 'mkRenewOffering' smart constructor.
data RenewOffering = RenewOffering'
  { quantity ::
      Lude.Maybe Lude.Int,
    offeringId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewOffering' with the minimum fields required to make a request.
--
-- * 'offeringId' - The ID of a request to renew an offering.
-- * 'quantity' - The quantity requested in an offering renewal.
mkRenewOffering ::
  RenewOffering
mkRenewOffering =
  RenewOffering'
    { quantity = Lude.Nothing,
      offeringId = Lude.Nothing
    }

-- | The quantity requested in an offering renewal.
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roQuantity :: Lens.Lens' RenewOffering (Lude.Maybe Lude.Int)
roQuantity = Lens.lens (quantity :: RenewOffering -> Lude.Maybe Lude.Int) (\s a -> s {quantity = a} :: RenewOffering)
{-# DEPRECATED roQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

-- | The ID of a request to renew an offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roOfferingId :: Lens.Lens' RenewOffering (Lude.Maybe Lude.Text)
roOfferingId = Lens.lens (offeringId :: RenewOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: RenewOffering)
{-# DEPRECATED roOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

instance Lude.AWSRequest RenewOffering where
  type Rs RenewOffering = RenewOfferingResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          RenewOfferingResponse'
            Lude.<$> (x Lude..?> "offeringTransaction")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RenewOffering where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.RenewOffering" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RenewOffering where
  toJSON RenewOffering' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("quantity" Lude..=) Lude.<$> quantity,
            ("offeringId" Lude..=) Lude.<$> offeringId
          ]
      )

instance Lude.ToPath RenewOffering where
  toPath = Lude.const "/"

instance Lude.ToQuery RenewOffering where
  toQuery = Lude.const Lude.mempty

-- | The result of a renewal offering.
--
-- /See:/ 'mkRenewOfferingResponse' smart constructor.
data RenewOfferingResponse = RenewOfferingResponse'
  { offeringTransaction ::
      Lude.Maybe OfferingTransaction,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewOfferingResponse' with the minimum fields required to make a request.
--
-- * 'offeringTransaction' - Represents the status of the offering transaction for the renewal.
-- * 'responseStatus' - The response status code.
mkRenewOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RenewOfferingResponse
mkRenewOfferingResponse pResponseStatus_ =
  RenewOfferingResponse'
    { offeringTransaction = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Represents the status of the offering transaction for the renewal.
--
-- /Note:/ Consider using 'offeringTransaction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rorsOfferingTransaction :: Lens.Lens' RenewOfferingResponse (Lude.Maybe OfferingTransaction)
rorsOfferingTransaction = Lens.lens (offeringTransaction :: RenewOfferingResponse -> Lude.Maybe OfferingTransaction) (\s a -> s {offeringTransaction = a} :: RenewOfferingResponse)
{-# DEPRECATED rorsOfferingTransaction "Use generic-lens or generic-optics with 'offeringTransaction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rorsResponseStatus :: Lens.Lens' RenewOfferingResponse Lude.Int
rorsResponseStatus = Lens.lens (responseStatus :: RenewOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RenewOfferingResponse)
{-# DEPRECATED rorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
