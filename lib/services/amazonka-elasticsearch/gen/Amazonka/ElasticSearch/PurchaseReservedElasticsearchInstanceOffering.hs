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
-- Module      : Amazonka.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved Elasticsearch instances.
module Amazonka.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
  ( -- * Creating a Request
    PurchaseReservedElasticsearchInstanceOffering (..),
    newPurchaseReservedElasticsearchInstanceOffering,

    -- * Request Lenses
    purchaseReservedElasticsearchInstanceOffering_instanceCount,
    purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId,
    purchaseReservedElasticsearchInstanceOffering_reservationName,

    -- * Destructuring the Response
    PurchaseReservedElasticsearchInstanceOfferingResponse (..),
    newPurchaseReservedElasticsearchInstanceOfferingResponse,

    -- * Response Lenses
    purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for parameters to
-- @PurchaseReservedElasticsearchInstanceOffering@
--
-- /See:/ 'newPurchaseReservedElasticsearchInstanceOffering' smart constructor.
data PurchaseReservedElasticsearchInstanceOffering = PurchaseReservedElasticsearchInstanceOffering'
  { -- | The number of Elasticsearch instances to reserve.
    instanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the reserved Elasticsearch instance offering to purchase.
    reservedElasticsearchInstanceOfferingId :: Prelude.Text,
    -- | A customer-specified identifier to track this reservation.
    reservationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedElasticsearchInstanceOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'purchaseReservedElasticsearchInstanceOffering_instanceCount' - The number of Elasticsearch instances to reserve.
--
-- 'reservedElasticsearchInstanceOfferingId', 'purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId' - The ID of the reserved Elasticsearch instance offering to purchase.
--
-- 'reservationName', 'purchaseReservedElasticsearchInstanceOffering_reservationName' - A customer-specified identifier to track this reservation.
newPurchaseReservedElasticsearchInstanceOffering ::
  -- | 'reservedElasticsearchInstanceOfferingId'
  Prelude.Text ->
  -- | 'reservationName'
  Prelude.Text ->
  PurchaseReservedElasticsearchInstanceOffering
newPurchaseReservedElasticsearchInstanceOffering
  pReservedElasticsearchInstanceOfferingId_
  pReservationName_ =
    PurchaseReservedElasticsearchInstanceOffering'
      { instanceCount =
          Prelude.Nothing,
        reservedElasticsearchInstanceOfferingId =
          pReservedElasticsearchInstanceOfferingId_,
        reservationName =
          pReservationName_
      }

-- | The number of Elasticsearch instances to reserve.
purchaseReservedElasticsearchInstanceOffering_instanceCount :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering (Prelude.Maybe Prelude.Natural)
purchaseReservedElasticsearchInstanceOffering_instanceCount = Lens.lens (\PurchaseReservedElasticsearchInstanceOffering' {instanceCount} -> instanceCount) (\s@PurchaseReservedElasticsearchInstanceOffering' {} a -> s {instanceCount = a} :: PurchaseReservedElasticsearchInstanceOffering)

-- | The ID of the reserved Elasticsearch instance offering to purchase.
purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Prelude.Text
purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId = Lens.lens (\PurchaseReservedElasticsearchInstanceOffering' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@PurchaseReservedElasticsearchInstanceOffering' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: PurchaseReservedElasticsearchInstanceOffering)

-- | A customer-specified identifier to track this reservation.
purchaseReservedElasticsearchInstanceOffering_reservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Prelude.Text
purchaseReservedElasticsearchInstanceOffering_reservationName = Lens.lens (\PurchaseReservedElasticsearchInstanceOffering' {reservationName} -> reservationName) (\s@PurchaseReservedElasticsearchInstanceOffering' {} a -> s {reservationName = a} :: PurchaseReservedElasticsearchInstanceOffering)

instance
  Core.AWSRequest
    PurchaseReservedElasticsearchInstanceOffering
  where
  type
    AWSResponse
      PurchaseReservedElasticsearchInstanceOffering =
      PurchaseReservedElasticsearchInstanceOfferingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseReservedElasticsearchInstanceOfferingResponse'
            Prelude.<$> (x Data..?> "ReservedElasticsearchInstanceId")
              Prelude.<*> (x Data..?> "ReservationName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PurchaseReservedElasticsearchInstanceOffering
  where
  hashWithSalt
    _salt
    PurchaseReservedElasticsearchInstanceOffering' {..} =
      _salt `Prelude.hashWithSalt` instanceCount
        `Prelude.hashWithSalt` reservedElasticsearchInstanceOfferingId
        `Prelude.hashWithSalt` reservationName

instance
  Prelude.NFData
    PurchaseReservedElasticsearchInstanceOffering
  where
  rnf
    PurchaseReservedElasticsearchInstanceOffering' {..} =
      Prelude.rnf instanceCount
        `Prelude.seq` Prelude.rnf reservedElasticsearchInstanceOfferingId
        `Prelude.seq` Prelude.rnf reservationName

instance
  Data.ToHeaders
    PurchaseReservedElasticsearchInstanceOffering
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PurchaseReservedElasticsearchInstanceOffering
  where
  toJSON
    PurchaseReservedElasticsearchInstanceOffering' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("InstanceCount" Data..=) Prelude.<$> instanceCount,
              Prelude.Just
                ( "ReservedElasticsearchInstanceOfferingId"
                    Data..= reservedElasticsearchInstanceOfferingId
                ),
              Prelude.Just
                ("ReservationName" Data..= reservationName)
            ]
        )

instance
  Data.ToPath
    PurchaseReservedElasticsearchInstanceOffering
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/purchaseReservedInstanceOffering"

instance
  Data.ToQuery
    PurchaseReservedElasticsearchInstanceOffering
  where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a
-- @PurchaseReservedElasticsearchInstanceOffering@ operation.
--
-- /See:/ 'newPurchaseReservedElasticsearchInstanceOfferingResponse' smart constructor.
data PurchaseReservedElasticsearchInstanceOfferingResponse = PurchaseReservedElasticsearchInstanceOfferingResponse'
  { -- | Details of the reserved Elasticsearch instance which was purchased.
    reservedElasticsearchInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The customer-specified identifier used to track this reservation.
    reservationName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedElasticsearchInstanceOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedElasticsearchInstanceId', 'purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId' - Details of the reserved Elasticsearch instance which was purchased.
--
-- 'reservationName', 'purchaseReservedElasticsearchInstanceOfferingResponse_reservationName' - The customer-specified identifier used to track this reservation.
--
-- 'httpStatus', 'purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedElasticsearchInstanceOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseReservedElasticsearchInstanceOfferingResponse
newPurchaseReservedElasticsearchInstanceOfferingResponse
  pHttpStatus_ =
    PurchaseReservedElasticsearchInstanceOfferingResponse'
      { reservedElasticsearchInstanceId =
          Prelude.Nothing,
        reservationName =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Details of the reserved Elasticsearch instance which was purchased.
purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Prelude.Maybe Prelude.Text)
purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId = Lens.lens (\PurchaseReservedElasticsearchInstanceOfferingResponse' {reservedElasticsearchInstanceId} -> reservedElasticsearchInstanceId) (\s@PurchaseReservedElasticsearchInstanceOfferingResponse' {} a -> s {reservedElasticsearchInstanceId = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)

-- | The customer-specified identifier used to track this reservation.
purchaseReservedElasticsearchInstanceOfferingResponse_reservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Prelude.Maybe Prelude.Text)
purchaseReservedElasticsearchInstanceOfferingResponse_reservationName = Lens.lens (\PurchaseReservedElasticsearchInstanceOfferingResponse' {reservationName} -> reservationName) (\s@PurchaseReservedElasticsearchInstanceOfferingResponse' {} a -> s {reservationName = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)

-- | The response's http status code.
purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse Prelude.Int
purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedElasticsearchInstanceOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedElasticsearchInstanceOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)

instance
  Prelude.NFData
    PurchaseReservedElasticsearchInstanceOfferingResponse
  where
  rnf
    PurchaseReservedElasticsearchInstanceOfferingResponse' {..} =
      Prelude.rnf reservedElasticsearchInstanceId
        `Prelude.seq` Prelude.rnf reservationName
        `Prelude.seq` Prelude.rnf httpStatus
