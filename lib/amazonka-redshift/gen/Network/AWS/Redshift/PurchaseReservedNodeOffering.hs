{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.PurchaseReservedNodeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved nodes. Amazon Redshift offers a predefined set of reserved node offerings. You can purchase one or more of the offerings. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings. You can call this API by providing a specific reserved node offering and the number of nodes you want to reserve.
--
-- For more information about reserved node offerings, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.PurchaseReservedNodeOffering
  ( -- * Creating a request
    PurchaseReservedNodeOffering (..),
    mkPurchaseReservedNodeOffering,

    -- ** Request lenses
    prnoNodeCount,
    prnoReservedNodeOfferingId,

    -- * Destructuring the response
    PurchaseReservedNodeOfferingResponse (..),
    mkPurchaseReservedNodeOfferingResponse,

    -- ** Response lenses
    prnorsReservedNode,
    prnorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkPurchaseReservedNodeOffering' smart constructor.
data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering'
  { -- | The number of reserved nodes that you want to purchase.
    --
    -- Default: @1@
    nodeCount :: Lude.Maybe Lude.Int,
    -- | The unique identifier of the reserved node offering you want to purchase.
    reservedNodeOfferingId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedNodeOffering' with the minimum fields required to make a request.
--
-- * 'nodeCount' - The number of reserved nodes that you want to purchase.
--
-- Default: @1@
-- * 'reservedNodeOfferingId' - The unique identifier of the reserved node offering you want to purchase.
mkPurchaseReservedNodeOffering ::
  -- | 'reservedNodeOfferingId'
  Lude.Text ->
  PurchaseReservedNodeOffering
mkPurchaseReservedNodeOffering pReservedNodeOfferingId_ =
  PurchaseReservedNodeOffering'
    { nodeCount = Lude.Nothing,
      reservedNodeOfferingId = pReservedNodeOfferingId_
    }

-- | The number of reserved nodes that you want to purchase.
--
-- Default: @1@
--
-- /Note:/ Consider using 'nodeCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnoNodeCount :: Lens.Lens' PurchaseReservedNodeOffering (Lude.Maybe Lude.Int)
prnoNodeCount = Lens.lens (nodeCount :: PurchaseReservedNodeOffering -> Lude.Maybe Lude.Int) (\s a -> s {nodeCount = a} :: PurchaseReservedNodeOffering)
{-# DEPRECATED prnoNodeCount "Use generic-lens or generic-optics with 'nodeCount' instead." #-}

-- | The unique identifier of the reserved node offering you want to purchase.
--
-- /Note:/ Consider using 'reservedNodeOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnoReservedNodeOfferingId :: Lens.Lens' PurchaseReservedNodeOffering Lude.Text
prnoReservedNodeOfferingId = Lens.lens (reservedNodeOfferingId :: PurchaseReservedNodeOffering -> Lude.Text) (\s a -> s {reservedNodeOfferingId = a} :: PurchaseReservedNodeOffering)
{-# DEPRECATED prnoReservedNodeOfferingId "Use generic-lens or generic-optics with 'reservedNodeOfferingId' instead." #-}

instance Lude.AWSRequest PurchaseReservedNodeOffering where
  type
    Rs PurchaseReservedNodeOffering =
      PurchaseReservedNodeOfferingResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "PurchaseReservedNodeOfferingResult"
      ( \s h x ->
          PurchaseReservedNodeOfferingResponse'
            Lude.<$> (x Lude..@? "ReservedNode") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseReservedNodeOffering where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurchaseReservedNodeOffering where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseReservedNodeOffering where
  toQuery PurchaseReservedNodeOffering' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PurchaseReservedNodeOffering" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "NodeCount" Lude.=: nodeCount,
        "ReservedNodeOfferingId" Lude.=: reservedNodeOfferingId
      ]

-- | /See:/ 'mkPurchaseReservedNodeOfferingResponse' smart constructor.
data PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse'
  { reservedNode :: Lude.Maybe ReservedNode,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedNodeOfferingResponse' with the minimum fields required to make a request.
--
-- * 'reservedNode' -
-- * 'responseStatus' - The response status code.
mkPurchaseReservedNodeOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseReservedNodeOfferingResponse
mkPurchaseReservedNodeOfferingResponse pResponseStatus_ =
  PurchaseReservedNodeOfferingResponse'
    { reservedNode =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservedNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnorsReservedNode :: Lens.Lens' PurchaseReservedNodeOfferingResponse (Lude.Maybe ReservedNode)
prnorsReservedNode = Lens.lens (reservedNode :: PurchaseReservedNodeOfferingResponse -> Lude.Maybe ReservedNode) (\s a -> s {reservedNode = a} :: PurchaseReservedNodeOfferingResponse)
{-# DEPRECATED prnorsReservedNode "Use generic-lens or generic-optics with 'reservedNode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prnorsResponseStatus :: Lens.Lens' PurchaseReservedNodeOfferingResponse Lude.Int
prnorsResponseStatus = Lens.lens (responseStatus :: PurchaseReservedNodeOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseReservedNodeOfferingResponse)
{-# DEPRECATED prnorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
