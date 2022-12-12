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
-- Module      : Amazonka.OpenSearch.PurchaseReservedInstanceOffering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase Amazon OpenSearch Service Reserved Instances.
module Amazonka.OpenSearch.PurchaseReservedInstanceOffering
  ( -- * Creating a Request
    PurchaseReservedInstanceOffering (..),
    newPurchaseReservedInstanceOffering,

    -- * Request Lenses
    purchaseReservedInstanceOffering_instanceCount,
    purchaseReservedInstanceOffering_reservedInstanceOfferingId,
    purchaseReservedInstanceOffering_reservationName,

    -- * Destructuring the Response
    PurchaseReservedInstanceOfferingResponse (..),
    newPurchaseReservedInstanceOfferingResponse,

    -- * Response Lenses
    purchaseReservedInstanceOfferingResponse_reservationName,
    purchaseReservedInstanceOfferingResponse_reservedInstanceId,
    purchaseReservedInstanceOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for request parameters to the
-- @PurchaseReservedInstanceOffering@ operation.
--
-- /See:/ 'newPurchaseReservedInstanceOffering' smart constructor.
data PurchaseReservedInstanceOffering = PurchaseReservedInstanceOffering'
  { -- | The number of OpenSearch instances to reserve.
    instanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Reserved Instance offering to purchase.
    reservedInstanceOfferingId :: Prelude.Text,
    -- | A customer-specified identifier to track this reservation.
    reservationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedInstanceOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'purchaseReservedInstanceOffering_instanceCount' - The number of OpenSearch instances to reserve.
--
-- 'reservedInstanceOfferingId', 'purchaseReservedInstanceOffering_reservedInstanceOfferingId' - The ID of the Reserved Instance offering to purchase.
--
-- 'reservationName', 'purchaseReservedInstanceOffering_reservationName' - A customer-specified identifier to track this reservation.
newPurchaseReservedInstanceOffering ::
  -- | 'reservedInstanceOfferingId'
  Prelude.Text ->
  -- | 'reservationName'
  Prelude.Text ->
  PurchaseReservedInstanceOffering
newPurchaseReservedInstanceOffering
  pReservedInstanceOfferingId_
  pReservationName_ =
    PurchaseReservedInstanceOffering'
      { instanceCount =
          Prelude.Nothing,
        reservedInstanceOfferingId =
          pReservedInstanceOfferingId_,
        reservationName = pReservationName_
      }

-- | The number of OpenSearch instances to reserve.
purchaseReservedInstanceOffering_instanceCount :: Lens.Lens' PurchaseReservedInstanceOffering (Prelude.Maybe Prelude.Natural)
purchaseReservedInstanceOffering_instanceCount = Lens.lens (\PurchaseReservedInstanceOffering' {instanceCount} -> instanceCount) (\s@PurchaseReservedInstanceOffering' {} a -> s {instanceCount = a} :: PurchaseReservedInstanceOffering)

-- | The ID of the Reserved Instance offering to purchase.
purchaseReservedInstanceOffering_reservedInstanceOfferingId :: Lens.Lens' PurchaseReservedInstanceOffering Prelude.Text
purchaseReservedInstanceOffering_reservedInstanceOfferingId = Lens.lens (\PurchaseReservedInstanceOffering' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@PurchaseReservedInstanceOffering' {} a -> s {reservedInstanceOfferingId = a} :: PurchaseReservedInstanceOffering)

-- | A customer-specified identifier to track this reservation.
purchaseReservedInstanceOffering_reservationName :: Lens.Lens' PurchaseReservedInstanceOffering Prelude.Text
purchaseReservedInstanceOffering_reservationName = Lens.lens (\PurchaseReservedInstanceOffering' {reservationName} -> reservationName) (\s@PurchaseReservedInstanceOffering' {} a -> s {reservationName = a} :: PurchaseReservedInstanceOffering)

instance
  Core.AWSRequest
    PurchaseReservedInstanceOffering
  where
  type
    AWSResponse PurchaseReservedInstanceOffering =
      PurchaseReservedInstanceOfferingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseReservedInstanceOfferingResponse'
            Prelude.<$> (x Data..?> "ReservationName")
            Prelude.<*> (x Data..?> "ReservedInstanceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PurchaseReservedInstanceOffering
  where
  hashWithSalt
    _salt
    PurchaseReservedInstanceOffering' {..} =
      _salt `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` reservedInstanceOfferingId
        `Prelude.hashWithSalt` reservationName

instance
  Prelude.NFData
    PurchaseReservedInstanceOffering
  where
  rnf PurchaseReservedInstanceOffering' {..} =
    Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId
      `Prelude.seq` Prelude.rnf reservationName

instance
  Data.ToHeaders
    PurchaseReservedInstanceOffering
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PurchaseReservedInstanceOffering where
  toJSON PurchaseReservedInstanceOffering' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceCount" Data..=) Prelude.<$> instanceCount,
            Prelude.Just
              ( "ReservedInstanceOfferingId"
                  Data..= reservedInstanceOfferingId
              ),
            Prelude.Just
              ("ReservationName" Data..= reservationName)
          ]
      )

instance Data.ToPath PurchaseReservedInstanceOffering where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/purchaseReservedInstanceOffering"

instance
  Data.ToQuery
    PurchaseReservedInstanceOffering
  where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @PurchaseReservedInstanceOffering@ operation.
--
-- /See:/ 'newPurchaseReservedInstanceOfferingResponse' smart constructor.
data PurchaseReservedInstanceOfferingResponse = PurchaseReservedInstanceOfferingResponse'
  { -- | The customer-specified identifier used to track this reservation.
    reservationName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Reserved Instance offering that was purchased.
    reservedInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedInstanceOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationName', 'purchaseReservedInstanceOfferingResponse_reservationName' - The customer-specified identifier used to track this reservation.
--
-- 'reservedInstanceId', 'purchaseReservedInstanceOfferingResponse_reservedInstanceId' - The ID of the Reserved Instance offering that was purchased.
--
-- 'httpStatus', 'purchaseReservedInstanceOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedInstanceOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseReservedInstanceOfferingResponse
newPurchaseReservedInstanceOfferingResponse
  pHttpStatus_ =
    PurchaseReservedInstanceOfferingResponse'
      { reservationName =
          Prelude.Nothing,
        reservedInstanceId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The customer-specified identifier used to track this reservation.
purchaseReservedInstanceOfferingResponse_reservationName :: Lens.Lens' PurchaseReservedInstanceOfferingResponse (Prelude.Maybe Prelude.Text)
purchaseReservedInstanceOfferingResponse_reservationName = Lens.lens (\PurchaseReservedInstanceOfferingResponse' {reservationName} -> reservationName) (\s@PurchaseReservedInstanceOfferingResponse' {} a -> s {reservationName = a} :: PurchaseReservedInstanceOfferingResponse)

-- | The ID of the Reserved Instance offering that was purchased.
purchaseReservedInstanceOfferingResponse_reservedInstanceId :: Lens.Lens' PurchaseReservedInstanceOfferingResponse (Prelude.Maybe Prelude.Text)
purchaseReservedInstanceOfferingResponse_reservedInstanceId = Lens.lens (\PurchaseReservedInstanceOfferingResponse' {reservedInstanceId} -> reservedInstanceId) (\s@PurchaseReservedInstanceOfferingResponse' {} a -> s {reservedInstanceId = a} :: PurchaseReservedInstanceOfferingResponse)

-- | The response's http status code.
purchaseReservedInstanceOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedInstanceOfferingResponse Prelude.Int
purchaseReservedInstanceOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedInstanceOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedInstanceOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedInstanceOfferingResponse)

instance
  Prelude.NFData
    PurchaseReservedInstanceOfferingResponse
  where
  rnf PurchaseReservedInstanceOfferingResponse' {..} =
    Prelude.rnf reservationName
      `Prelude.seq` Prelude.rnf reservedInstanceId
      `Prelude.seq` Prelude.rnf httpStatus
