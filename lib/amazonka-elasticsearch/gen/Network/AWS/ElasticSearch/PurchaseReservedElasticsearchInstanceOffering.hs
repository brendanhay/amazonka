{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved Elasticsearch instances.
module Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
  ( -- * Creating a request
    PurchaseReservedElasticsearchInstanceOffering (..),
    mkPurchaseReservedElasticsearchInstanceOffering,

    -- ** Request lenses
    preioInstanceCount,
    preioReservedElasticsearchInstanceOfferingId,
    preioReservationName,

    -- * Destructuring the response
    PurchaseReservedElasticsearchInstanceOfferingResponse (..),
    mkPurchaseReservedElasticsearchInstanceOfferingResponse,

    -- ** Response lenses
    preiorsReservedElasticsearchInstanceId,
    preiorsReservationName,
    preiorsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for parameters to @PurchaseReservedElasticsearchInstanceOffering@
--
-- /See:/ 'mkPurchaseReservedElasticsearchInstanceOffering' smart constructor.
data PurchaseReservedElasticsearchInstanceOffering = PurchaseReservedElasticsearchInstanceOffering'
  { -- | The number of Elasticsearch instances to reserve.
    instanceCount :: Lude.Maybe Lude.Natural,
    -- | The ID of the reserved Elasticsearch instance offering to purchase.
    reservedElasticsearchInstanceOfferingId :: Lude.Text,
    -- | A customer-specified identifier to track this reservation.
    reservationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedElasticsearchInstanceOffering' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of Elasticsearch instances to reserve.
-- * 'reservedElasticsearchInstanceOfferingId' - The ID of the reserved Elasticsearch instance offering to purchase.
-- * 'reservationName' - A customer-specified identifier to track this reservation.
mkPurchaseReservedElasticsearchInstanceOffering ::
  -- | 'reservedElasticsearchInstanceOfferingId'
  Lude.Text ->
  -- | 'reservationName'
  Lude.Text ->
  PurchaseReservedElasticsearchInstanceOffering
mkPurchaseReservedElasticsearchInstanceOffering
  pReservedElasticsearchInstanceOfferingId_
  pReservationName_ =
    PurchaseReservedElasticsearchInstanceOffering'
      { instanceCount =
          Lude.Nothing,
        reservedElasticsearchInstanceOfferingId =
          pReservedElasticsearchInstanceOfferingId_,
        reservationName = pReservationName_
      }

-- | The number of Elasticsearch instances to reserve.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioInstanceCount :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering (Lude.Maybe Lude.Natural)
preioInstanceCount = Lens.lens (instanceCount :: PurchaseReservedElasticsearchInstanceOffering -> Lude.Maybe Lude.Natural) (\s a -> s {instanceCount = a} :: PurchaseReservedElasticsearchInstanceOffering)
{-# DEPRECATED preioInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ID of the reserved Elasticsearch instance offering to purchase.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioReservedElasticsearchInstanceOfferingId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Lude.Text
preioReservedElasticsearchInstanceOfferingId = Lens.lens (reservedElasticsearchInstanceOfferingId :: PurchaseReservedElasticsearchInstanceOffering -> Lude.Text) (\s a -> s {reservedElasticsearchInstanceOfferingId = a} :: PurchaseReservedElasticsearchInstanceOffering)
{-# DEPRECATED preioReservedElasticsearchInstanceOfferingId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceOfferingId' instead." #-}

-- | A customer-specified identifier to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preioReservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Lude.Text
preioReservationName = Lens.lens (reservationName :: PurchaseReservedElasticsearchInstanceOffering -> Lude.Text) (\s a -> s {reservationName = a} :: PurchaseReservedElasticsearchInstanceOffering)
{-# DEPRECATED preioReservationName "Use generic-lens or generic-optics with 'reservationName' instead." #-}

instance
  Lude.AWSRequest
    PurchaseReservedElasticsearchInstanceOffering
  where
  type
    Rs PurchaseReservedElasticsearchInstanceOffering =
      PurchaseReservedElasticsearchInstanceOfferingResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          PurchaseReservedElasticsearchInstanceOfferingResponse'
            Lude.<$> (x Lude..?> "ReservedElasticsearchInstanceId")
            Lude.<*> (x Lude..?> "ReservationName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    PurchaseReservedElasticsearchInstanceOffering
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PurchaseReservedElasticsearchInstanceOffering where
  toJSON PurchaseReservedElasticsearchInstanceOffering' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceCount" Lude..=) Lude.<$> instanceCount,
            Lude.Just
              ( "ReservedElasticsearchInstanceOfferingId"
                  Lude..= reservedElasticsearchInstanceOfferingId
              ),
            Lude.Just ("ReservationName" Lude..= reservationName)
          ]
      )

instance Lude.ToPath PurchaseReservedElasticsearchInstanceOffering where
  toPath =
    Lude.const "/2015-01-01/es/purchaseReservedInstanceOffering"

instance Lude.ToQuery PurchaseReservedElasticsearchInstanceOffering where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @PurchaseReservedElasticsearchInstanceOffering@ operation.
--
-- /See:/ 'mkPurchaseReservedElasticsearchInstanceOfferingResponse' smart constructor.
data PurchaseReservedElasticsearchInstanceOfferingResponse = PurchaseReservedElasticsearchInstanceOfferingResponse'
  { -- | Details of the reserved Elasticsearch instance which was purchased.
    reservedElasticsearchInstanceId :: Lude.Maybe Lude.Text,
    -- | The customer-specified identifier used to track this reservation.
    reservationName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedElasticsearchInstanceOfferingResponse' with the minimum fields required to make a request.
--
-- * 'reservedElasticsearchInstanceId' - Details of the reserved Elasticsearch instance which was purchased.
-- * 'reservationName' - The customer-specified identifier used to track this reservation.
-- * 'responseStatus' - The response status code.
mkPurchaseReservedElasticsearchInstanceOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseReservedElasticsearchInstanceOfferingResponse
mkPurchaseReservedElasticsearchInstanceOfferingResponse
  pResponseStatus_ =
    PurchaseReservedElasticsearchInstanceOfferingResponse'
      { reservedElasticsearchInstanceId =
          Lude.Nothing,
        reservationName = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Details of the reserved Elasticsearch instance which was purchased.
--
-- /Note:/ Consider using 'reservedElasticsearchInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorsReservedElasticsearchInstanceId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Lude.Maybe Lude.Text)
preiorsReservedElasticsearchInstanceId = Lens.lens (reservedElasticsearchInstanceId :: PurchaseReservedElasticsearchInstanceOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservedElasticsearchInstanceId = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)
{-# DEPRECATED preiorsReservedElasticsearchInstanceId "Use generic-lens or generic-optics with 'reservedElasticsearchInstanceId' instead." #-}

-- | The customer-specified identifier used to track this reservation.
--
-- /Note:/ Consider using 'reservationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorsReservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Lude.Maybe Lude.Text)
preiorsReservationName = Lens.lens (reservationName :: PurchaseReservedElasticsearchInstanceOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {reservationName = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)
{-# DEPRECATED preiorsReservationName "Use generic-lens or generic-optics with 'reservationName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preiorsResponseStatus :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse Lude.Int
preiorsResponseStatus = Lens.lens (responseStatus :: PurchaseReservedElasticsearchInstanceOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)
{-# DEPRECATED preiorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
