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
-- Module      : Amazonka.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Purchases a reserved DB instance offering.
module Amazonka.RDS.PurchaseReservedDBInstancesOffering
  ( -- * Creating a Request
    PurchaseReservedDBInstancesOffering (..),
    newPurchaseReservedDBInstancesOffering,

    -- * Request Lenses
    purchaseReservedDBInstancesOffering_dbInstanceCount,
    purchaseReservedDBInstancesOffering_reservedDBInstanceId,
    purchaseReservedDBInstancesOffering_tags,
    purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId,

    -- * Destructuring the Response
    PurchaseReservedDBInstancesOfferingResponse (..),
    newPurchaseReservedDBInstancesOfferingResponse,

    -- * Response Lenses
    purchaseReservedDBInstancesOfferingResponse_reservedDBInstance,
    purchaseReservedDBInstancesOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newPurchaseReservedDBInstancesOffering' smart constructor.
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering'
  { -- | The number of instances to reserve.
    --
    -- Default: @1@
    dbInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | Customer-specified identifier to track this reservation.
    --
    -- Example: myreservationID
    reservedDBInstanceId :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Reserved DB instance offering to purchase.
    --
    -- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
    reservedDBInstancesOfferingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'reservedDBInstanceId', 'purchaseReservedDBInstancesOffering_reservedDBInstanceId' - Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
--
-- 'tags', 'purchaseReservedDBInstancesOffering_tags' - Undocumented member.
--
-- 'reservedDBInstancesOfferingId', 'purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId' - The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
newPurchaseReservedDBInstancesOffering ::
  -- | 'reservedDBInstancesOfferingId'
  Prelude.Text ->
  PurchaseReservedDBInstancesOffering
newPurchaseReservedDBInstancesOffering
  pReservedDBInstancesOfferingId_ =
    PurchaseReservedDBInstancesOffering'
      { dbInstanceCount =
          Prelude.Nothing,
        reservedDBInstanceId = Prelude.Nothing,
        tags = Prelude.Nothing,
        reservedDBInstancesOfferingId =
          pReservedDBInstancesOfferingId_
      }

-- | The number of instances to reserve.
--
-- Default: @1@
purchaseReservedDBInstancesOffering_dbInstanceCount :: Lens.Lens' PurchaseReservedDBInstancesOffering (Prelude.Maybe Prelude.Int)
purchaseReservedDBInstancesOffering_dbInstanceCount = Lens.lens (\PurchaseReservedDBInstancesOffering' {dbInstanceCount} -> dbInstanceCount) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {dbInstanceCount = a} :: PurchaseReservedDBInstancesOffering)

-- | Customer-specified identifier to track this reservation.
--
-- Example: myreservationID
purchaseReservedDBInstancesOffering_reservedDBInstanceId :: Lens.Lens' PurchaseReservedDBInstancesOffering (Prelude.Maybe Prelude.Text)
purchaseReservedDBInstancesOffering_reservedDBInstanceId = Lens.lens (\PurchaseReservedDBInstancesOffering' {reservedDBInstanceId} -> reservedDBInstanceId) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {reservedDBInstanceId = a} :: PurchaseReservedDBInstancesOffering)

-- | Undocumented member.
purchaseReservedDBInstancesOffering_tags :: Lens.Lens' PurchaseReservedDBInstancesOffering (Prelude.Maybe [Tag])
purchaseReservedDBInstancesOffering_tags = Lens.lens (\PurchaseReservedDBInstancesOffering' {tags} -> tags) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {tags = a} :: PurchaseReservedDBInstancesOffering) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Reserved DB instance offering to purchase.
--
-- Example: 438012d3-4052-4cc7-b2e3-8d3372e0e706
purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId :: Lens.Lens' PurchaseReservedDBInstancesOffering Prelude.Text
purchaseReservedDBInstancesOffering_reservedDBInstancesOfferingId = Lens.lens (\PurchaseReservedDBInstancesOffering' {reservedDBInstancesOfferingId} -> reservedDBInstancesOfferingId) (\s@PurchaseReservedDBInstancesOffering' {} a -> s {reservedDBInstancesOfferingId = a} :: PurchaseReservedDBInstancesOffering)

instance
  Core.AWSRequest
    PurchaseReservedDBInstancesOffering
  where
  type
    AWSResponse PurchaseReservedDBInstancesOffering =
      PurchaseReservedDBInstancesOfferingResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PurchaseReservedDBInstancesOfferingResult"
      ( \s h x ->
          PurchaseReservedDBInstancesOfferingResponse'
            Prelude.<$> (x Data..@? "ReservedDBInstance")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PurchaseReservedDBInstancesOffering
  where
  hashWithSalt
    _salt
    PurchaseReservedDBInstancesOffering' {..} =
      _salt `Prelude.hashWithSalt` dbInstanceCount
        `Prelude.hashWithSalt` reservedDBInstanceId
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` reservedDBInstancesOfferingId

instance
  Prelude.NFData
    PurchaseReservedDBInstancesOffering
  where
  rnf PurchaseReservedDBInstancesOffering' {..} =
    Prelude.rnf dbInstanceCount
      `Prelude.seq` Prelude.rnf reservedDBInstanceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf reservedDBInstancesOfferingId

instance
  Data.ToHeaders
    PurchaseReservedDBInstancesOffering
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    PurchaseReservedDBInstancesOffering
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    PurchaseReservedDBInstancesOffering
  where
  toQuery PurchaseReservedDBInstancesOffering' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "PurchaseReservedDBInstancesOffering" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceCount" Data.=: dbInstanceCount,
        "ReservedDBInstanceId" Data.=: reservedDBInstanceId,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "ReservedDBInstancesOfferingId"
          Data.=: reservedDBInstancesOfferingId
      ]

-- | /See:/ 'newPurchaseReservedDBInstancesOfferingResponse' smart constructor.
data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse'
  { reservedDBInstance :: Prelude.Maybe ReservedDBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PurchaseReservedDBInstancesOfferingResponse
newPurchaseReservedDBInstancesOfferingResponse
  pHttpStatus_ =
    PurchaseReservedDBInstancesOfferingResponse'
      { reservedDBInstance =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
purchaseReservedDBInstancesOfferingResponse_reservedDBInstance :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse (Prelude.Maybe ReservedDBInstance)
purchaseReservedDBInstancesOfferingResponse_reservedDBInstance = Lens.lens (\PurchaseReservedDBInstancesOfferingResponse' {reservedDBInstance} -> reservedDBInstance) (\s@PurchaseReservedDBInstancesOfferingResponse' {} a -> s {reservedDBInstance = a} :: PurchaseReservedDBInstancesOfferingResponse)

-- | The response's http status code.
purchaseReservedDBInstancesOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedDBInstancesOfferingResponse Prelude.Int
purchaseReservedDBInstancesOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedDBInstancesOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedDBInstancesOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedDBInstancesOfferingResponse)

instance
  Prelude.NFData
    PurchaseReservedDBInstancesOfferingResponse
  where
  rnf PurchaseReservedDBInstancesOfferingResponse' {..} =
    Prelude.rnf reservedDBInstance
      `Prelude.seq` Prelude.rnf httpStatus
