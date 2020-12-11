{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase a reserved cache node offering.
module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
  ( -- * Creating a request
    PurchaseReservedCacheNodesOffering (..),
    mkPurchaseReservedCacheNodesOffering,

    -- ** Request lenses
    prcnoCacheNodeCount,
    prcnoReservedCacheNodeId,
    prcnoReservedCacheNodesOfferingId,

    -- * Destructuring the response
    PurchaseReservedCacheNodesOfferingResponse (..),
    mkPurchaseReservedCacheNodesOfferingResponse,

    -- ** Response lenses
    prcnorsReservedCacheNode,
    prcnorsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @PurchaseReservedCacheNodesOffering@ operation.
--
-- /See:/ 'mkPurchaseReservedCacheNodesOffering' smart constructor.
data PurchaseReservedCacheNodesOffering = PurchaseReservedCacheNodesOffering'
  { cacheNodeCount ::
      Lude.Maybe Lude.Int,
    reservedCacheNodeId ::
      Lude.Maybe Lude.Text,
    reservedCacheNodesOfferingId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedCacheNodesOffering' with the minimum fields required to make a request.
--
-- * 'cacheNodeCount' - The number of cache node instances to reserve.
--
-- Default: @1@
-- * 'reservedCacheNodeId' - A customer-specified identifier to track this reservation.
--
-- Example: myreservationID
-- * 'reservedCacheNodesOfferingId' - The ID of the reserved cache node offering to purchase.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
mkPurchaseReservedCacheNodesOffering ::
  -- | 'reservedCacheNodesOfferingId'
  Lude.Text ->
  PurchaseReservedCacheNodesOffering
mkPurchaseReservedCacheNodesOffering pReservedCacheNodesOfferingId_ =
  PurchaseReservedCacheNodesOffering'
    { cacheNodeCount =
        Lude.Nothing,
      reservedCacheNodeId = Lude.Nothing,
      reservedCacheNodesOfferingId =
        pReservedCacheNodesOfferingId_
    }

-- | The number of cache node instances to reserve.
--
-- Default: @1@
--
-- /Note:/ Consider using 'cacheNodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnoCacheNodeCount :: Lens.Lens' PurchaseReservedCacheNodesOffering (Lude.Maybe Lude.Int)
prcnoCacheNodeCount = Lens.lens (cacheNodeCount :: PurchaseReservedCacheNodesOffering -> Lude.Maybe Lude.Int) (\s a -> s {cacheNodeCount = a} :: PurchaseReservedCacheNodesOffering)
{-# DEPRECATED prcnoCacheNodeCount "Use generic-lens or generic-optics with 'cacheNodeCount' instead." #-}

-- | A customer-specified identifier to track this reservation.
--
-- Example: myreservationID
--
-- /Note:/ Consider using 'reservedCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnoReservedCacheNodeId :: Lens.Lens' PurchaseReservedCacheNodesOffering (Lude.Maybe Lude.Text)
prcnoReservedCacheNodeId = Lens.lens (reservedCacheNodeId :: PurchaseReservedCacheNodesOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedCacheNodeId = a} :: PurchaseReservedCacheNodesOffering)
{-# DEPRECATED prcnoReservedCacheNodeId "Use generic-lens or generic-optics with 'reservedCacheNodeId' instead." #-}

-- | The ID of the reserved cache node offering to purchase.
--
-- Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
--
-- /Note:/ Consider using 'reservedCacheNodesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnoReservedCacheNodesOfferingId :: Lens.Lens' PurchaseReservedCacheNodesOffering Lude.Text
prcnoReservedCacheNodesOfferingId = Lens.lens (reservedCacheNodesOfferingId :: PurchaseReservedCacheNodesOffering -> Lude.Text) (\s a -> s {reservedCacheNodesOfferingId = a} :: PurchaseReservedCacheNodesOffering)
{-# DEPRECATED prcnoReservedCacheNodesOfferingId "Use generic-lens or generic-optics with 'reservedCacheNodesOfferingId' instead." #-}

instance Lude.AWSRequest PurchaseReservedCacheNodesOffering where
  type
    Rs PurchaseReservedCacheNodesOffering =
      PurchaseReservedCacheNodesOfferingResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "PurchaseReservedCacheNodesOfferingResult"
      ( \s h x ->
          PurchaseReservedCacheNodesOfferingResponse'
            Lude.<$> (x Lude..@? "ReservedCacheNode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseReservedCacheNodesOffering where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurchaseReservedCacheNodesOffering where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseReservedCacheNodesOffering where
  toQuery PurchaseReservedCacheNodesOffering' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PurchaseReservedCacheNodesOffering" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "CacheNodeCount" Lude.=: cacheNodeCount,
        "ReservedCacheNodeId" Lude.=: reservedCacheNodeId,
        "ReservedCacheNodesOfferingId"
          Lude.=: reservedCacheNodesOfferingId
      ]

-- | /See:/ 'mkPurchaseReservedCacheNodesOfferingResponse' smart constructor.
data PurchaseReservedCacheNodesOfferingResponse = PurchaseReservedCacheNodesOfferingResponse'
  { reservedCacheNode ::
      Lude.Maybe
        ReservedCacheNode,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedCacheNodesOfferingResponse' with the minimum fields required to make a request.
--
-- * 'reservedCacheNode' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPurchaseReservedCacheNodesOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseReservedCacheNodesOfferingResponse
mkPurchaseReservedCacheNodesOfferingResponse pResponseStatus_ =
  PurchaseReservedCacheNodesOfferingResponse'
    { reservedCacheNode =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservedCacheNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnorsReservedCacheNode :: Lens.Lens' PurchaseReservedCacheNodesOfferingResponse (Lude.Maybe ReservedCacheNode)
prcnorsReservedCacheNode = Lens.lens (reservedCacheNode :: PurchaseReservedCacheNodesOfferingResponse -> Lude.Maybe ReservedCacheNode) (\s a -> s {reservedCacheNode = a} :: PurchaseReservedCacheNodesOfferingResponse)
{-# DEPRECATED prcnorsReservedCacheNode "Use generic-lens or generic-optics with 'reservedCacheNode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prcnorsResponseStatus :: Lens.Lens' PurchaseReservedCacheNodesOfferingResponse Lude.Int
prcnorsResponseStatus = Lens.lens (responseStatus :: PurchaseReservedCacheNodesOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseReservedCacheNodesOfferingResponse)
{-# DEPRECATED prcnorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
