{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a reserved DB instance offering.
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
  ( -- * Creating a request
    PurchaseReservedDBInstancesOffering (..),
    mkPurchaseReservedDBInstancesOffering,

    -- ** Request lenses
    prdioDBInstanceCount,
    prdioReservedDBInstanceId,
    prdioReservedDBInstancesOfferingId,
    prdioTags,

    -- * Destructuring the response
    PurchaseReservedDBInstancesOfferingResponse (..),
    mkPurchaseReservedDBInstancesOfferingResponse,

    -- ** Response lenses
    prdiorsReservedDBInstance,
    prdiorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkPurchaseReservedDBInstancesOffering' smart constructor.
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'
  { -- | The number of instances to reserve.
    --
    -- Default: @1@
    dbInstanceCount :: Lude.Maybe Lude.Int,
    -- | Customer-specified identifier to track this reservation.
    --
    -- Example: myreservationID
    reservedDBInstanceId :: Lude.Maybe Lude.Text,
    -- | The ID of the Reserved DB instance offering to purchase.
    --
    -- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
    reservedDBInstancesOfferingId :: Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedDBInstancesOffering' with the minimum fields required to make a request.
--
-- * 'dbInstanceCount' - The number of instances to reserve.
--
-- Default: @1@
-- * 'reservedDBInstanceId' - Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
-- * 'reservedDBInstancesOfferingId' - The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
-- * 'tags' -
mkPurchaseReservedDBInstancesOffering ::
  -- | 'reservedDBInstancesOfferingId'
  Lude.Text ->
  PurchaseReservedDBInstancesOffering
mkPurchaseReservedDBInstancesOffering
  pReservedDBInstancesOfferingId_ =
    PurchaseReservedDBInstancesOffering'
      { dbInstanceCount =
          Lude.Nothing,
        reservedDBInstanceId = Lude.Nothing,
        reservedDBInstancesOfferingId =
          pReservedDBInstancesOfferingId_,
        tags = Lude.Nothing
      }

-- | The number of instances to reserve.
--
-- Default: @1@
--
-- /Note:/ Consider using 'dbInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdioDBInstanceCount :: Lens.Lens' PurchaseReservedDBInstancesOffering (Lude.Maybe Lude.Int)
prdioDBInstanceCount = Lens.lens (dbInstanceCount :: PurchaseReservedDBInstancesOffering -> Lude.Maybe Lude.Int) (\s a -> s {dbInstanceCount = a} :: PurchaseReservedDBInstancesOffering)
{-# DEPRECATED prdioDBInstanceCount "Use generic-lens or generic-optics with 'dbInstanceCount' instead." #-}

-- | Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdioReservedDBInstanceId :: Lens.Lens' PurchaseReservedDBInstancesOffering (Lude.Maybe Lude.Text)
prdioReservedDBInstanceId = Lens.lens (reservedDBInstanceId :: PurchaseReservedDBInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstanceId = a} :: PurchaseReservedDBInstancesOffering)
{-# DEPRECATED prdioReservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead." #-}

-- | The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdioReservedDBInstancesOfferingId :: Lens.Lens' PurchaseReservedDBInstancesOffering Lude.Text
prdioReservedDBInstancesOfferingId = Lens.lens (reservedDBInstancesOfferingId :: PurchaseReservedDBInstancesOffering -> Lude.Text) (\s a -> s {reservedDBInstancesOfferingId = a} :: PurchaseReservedDBInstancesOffering)
{-# DEPRECATED prdioReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdioTags :: Lens.Lens' PurchaseReservedDBInstancesOffering (Lude.Maybe [Tag])
prdioTags = Lens.lens (tags :: PurchaseReservedDBInstancesOffering -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PurchaseReservedDBInstancesOffering)
{-# DEPRECATED prdioTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest PurchaseReservedDBInstancesOffering where
  type
    Rs PurchaseReservedDBInstancesOffering =
      PurchaseReservedDBInstancesOfferingResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "PurchaseReservedDBInstancesOfferingResult"
      ( \s h x ->
          PurchaseReservedDBInstancesOfferingResponse'
            Lude.<$> (x Lude..@? "ReservedDBInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PurchaseReservedDBInstancesOffering where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PurchaseReservedDBInstancesOffering where
  toPath = Lude.const "/"

instance Lude.ToQuery PurchaseReservedDBInstancesOffering where
  toQuery PurchaseReservedDBInstancesOffering' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PurchaseReservedDBInstancesOffering" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBInstanceCount" Lude.=: dbInstanceCount,
        "ReservedDBInstanceId" Lude.=: reservedDBInstanceId,
        "ReservedDBInstancesOfferingId"
          Lude.=: reservedDBInstancesOfferingId,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]

-- | /See:/ 'mkPurchaseReservedDBInstancesOfferingResponse' smart constructor.
data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'
  { reservedDBInstance :: Lude.Maybe ReservedDBInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PurchaseReservedDBInstancesOfferingResponse' with the minimum fields required to make a request.
--
-- * 'reservedDBInstance' -
-- * 'responseStatus' - The response status code.
mkPurchaseReservedDBInstancesOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PurchaseReservedDBInstancesOfferingResponse
mkPurchaseReservedDBInstancesOfferingResponse pResponseStatus_ =
  PurchaseReservedDBInstancesOfferingResponse'
    { reservedDBInstance =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reservedDBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdiorsReservedDBInstance :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse (Lude.Maybe ReservedDBInstance)
prdiorsReservedDBInstance = Lens.lens (reservedDBInstance :: PurchaseReservedDBInstancesOfferingResponse -> Lude.Maybe ReservedDBInstance) (\s a -> s {reservedDBInstance = a} :: PurchaseReservedDBInstancesOfferingResponse)
{-# DEPRECATED prdiorsReservedDBInstance "Use generic-lens or generic-optics with 'reservedDBInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prdiorsResponseStatus :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse Lude.Int
prdiorsResponseStatus = Lens.lens (responseStatus :: PurchaseReservedDBInstancesOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PurchaseReservedDBInstancesOfferingResponse)
{-# DEPRECATED prdiorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
