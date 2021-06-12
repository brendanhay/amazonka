{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a reserved DB instance offering.
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
  ( -- * Creating a Request
    PurchaseReservedDBInstancesOffering (..),
    newPurchaseReservedDBInstancesOffering,

    -- * Request Lenses
    purchaseReservedDBInstancesOffering_dbInstanceCount,
    purchaseReservedDBInstancesOffering_tags,
    purchaseReservedDBInstancesOffering_reservedDBInstanceId,
    purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId,

    -- * Destructuring the Response
    PurchaseReservedDBInstancesOfferingResponse (..),
    newPurchaseReservedDBInstancesOfferingResponse,

    -- * Response Lenses
    purchaseReservedDBInstancesOfferingResponse_reservedDBInstance,
    purchaseReservedDBInstancesOfferingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newPurchaseReservedDBInstancesOffering' smart constructor.
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'
  { -- | The number of instances to reserve.
    --
    -- Default: @1@
    dbInstanceCount :: Core.Maybe Core.Int,
    tags :: Core.Maybe [Tag],
    -- | Customer-specified identifier to track this reservation.
    --
    -- Example: myreservationID
    reservedDBInstanceId :: Core.Maybe Core.Text,
    -- | The ID of the Reserved DB instance offering to purchase.
    --
    -- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
    reservedDBInstancesOfferingId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseReservedDBInstancesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceCount', 'purchaseReservedDBInstancesOffering_dbInstanceCount' - The number of instances to reserve.
--
-- Default: @1@
--
-- 'tags', 'purchaseReservedDBInstancesOffering_tags' - Undocumented member.
--
-- 'reservedDBInstanceId', 'purchaseReservedDBInstancesOffering_reservedDBInstanceId' - Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
--
-- 'reservedDBInstancesOfferingId', 'purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId' - The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
newPurchaseReservedDBInstancesOffering ::
  -- | 'reservedDBInstancesOfferingId'
  Core.Text ->
  PurchaseReservedDBInstancesOffering
newPurchaseReservedDBInstancesOffering
  pReservedDBInstancesOfferingId_ =
    PurchaseReservedDBInstancesOffering'
      { dbInstanceCount =
          Core.Nothing,
        tags = Core.Nothing,
        reservedDBInstanceId = Core.Nothing,
        reservedDBInstancesOfferingId =
          pReservedDBInstancesOfferingId_
      }

-- | The number of instances to reserve.
--
-- Default: @1@
purchaseReservedDBInstancesOffering_dbInstanceCount :: Lens.Lens' PurchaseReservedDBInstancesOffering (Core.Maybe Core.Int)
purchaseReservedDBInstancesOffering_dbInstanceCount = Lens.lens (\PurchaseReservedDBInstancesOffering' {dbInstanceCount} -> dbInstanceCount) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {dbInstanceCount = a} :: PurchaseReservedDBInstancesOffering)

-- | Undocumented member.
purchaseReservedDBInstancesOffering_tags :: Lens.Lens' PurchaseReservedDBInstancesOffering (Core.Maybe [Tag])
purchaseReservedDBInstancesOffering_tags = Lens.lens (\PurchaseReservedDBInstancesOffering' {tags} -> tags) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {tags = a} :: PurchaseReservedDBInstancesOffering) Core.. Lens.mapping Lens._Coerce

-- | Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
purchaseReservedDBInstancesOffering_reservedDBInstanceId :: Lens.Lens' PurchaseReservedDBInstancesOffering (Core.Maybe Core.Text)
purchaseReservedDBInstancesOffering_reservedDBInstanceId = Lens.lens (\PurchaseReservedDBInstancesOffering' {reservedDBInstanceId} -> reservedDBInstanceId) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {reservedDBInstanceId = a} :: PurchaseReservedDBInstancesOffering)

-- | The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId :: Lens.Lens' PurchaseReservedDBInstancesOffering Core.Text
purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId = Lens.lens (\PurchaseReservedDBInstancesOffering' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {reservedDBInstancesOfferingId = a} :: PurchaseReservedDBInstancesOffering)

instance
  Core.AWSRequest
    PurchaseReservedDBInstancesOffering
  where
  type
    AWSResponse PurchaseReservedDBInstancesOffering =
      PurchaseReservedDBInstancesOfferingResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PurchaseReservedDBInstancesOfferingResult"
      ( \s h x ->
          PurchaseReservedDBInstancesOfferingResponse'
            Core.<$> (x Core..@? "ReservedDBInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    PurchaseReservedDBInstancesOffering

instance
  Core.NFData
    PurchaseReservedDBInstancesOffering

instance
  Core.ToHeaders
    PurchaseReservedDBInstancesOffering
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    PurchaseReservedDBInstancesOffering
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    PurchaseReservedDBInstancesOffering
  where
  toQuery PurchaseReservedDBInstancesOffering' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "PurchaseReservedDBInstancesOffering" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBInstanceCount" Core.=: dbInstanceCount,
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "ReservedDBInstanceId" Core.=: reservedDBInstanceId,
        "ReservedDBInstancesOfferingId"
          Core.=: reservedDBInstancesOfferingId
      ]

-- | /See:/ 'newPurchaseReservedDBInstancesOfferingResponse' smart constructor.
data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'
  { reservedDBInstance :: Core.Maybe ReservedDBInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseReservedDBInstancesOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedDBInstance', 'purchaseReservedDBInstancesOfferingResponse_reservedDBInstance' - Undocumented member.
--
-- 'httpStatus', 'purchaseReservedDBInstancesOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedDBInstancesOfferingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PurchaseReservedDBInstancesOfferingResponse
newPurchaseReservedDBInstancesOfferingResponse
  pHttpStatus_ =
    PurchaseReservedDBInstancesOfferingResponse'
      { reservedDBInstance =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
purchaseReservedDBInstancesOfferingResponse_reservedDBInstance :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse (Core.Maybe ReservedDBInstance)
purchaseReservedDBInstancesOfferingResponse_reservedDBInstance = Lens.lens (\PurchaseReservedDBInstancesOfferingResponse' {reservedDBInstance} -> reservedDBInstance) (\s@PurchaseReservedDBInstancesOfferingResponse' {} a -> s {reservedDBInstance = a} :: PurchaseReservedDBInstancesOfferingResponse)

-- | The response's http status code.
purchaseReservedDBInstancesOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse Core.Int
purchaseReservedDBInstancesOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedDBInstancesOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedDBInstancesOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedDBInstancesOfferingResponse)

instance
  Core.NFData
    PurchaseReservedDBInstancesOfferingResponse
