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
-- Module      : Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved Elasticsearch instances.
module Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
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
    purchaseReservedElasticsearchInstanceOfferingResponse_reservationName,
    purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId,
    purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for parameters to
-- @PurchaseReservedElasticsearchInstanceOffering@
--
-- /See:/ 'newPurchaseReservedElasticsearchInstanceOffering' smart constructor.
data PurchaseReservedElasticsearchInstanceOffering = PurchaseReservedElasticsearchInstanceOffering'
  { -- | The number of Elasticsearch instances to reserve.
    instanceCount :: Core.Maybe Core.Natural,
    -- | The ID of the reserved Elasticsearch instance offering to purchase.
    reservedElasticsearchInstanceOfferingId :: Core.Text,
    -- | A customer-specified identifier to track this reservation.
    reservationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'reservationName'
  Core.Text ->
  PurchaseReservedElasticsearchInstanceOffering
newPurchaseReservedElasticsearchInstanceOffering
  pReservedElasticsearchInstanceOfferingId_
  pReservationName_ =
    PurchaseReservedElasticsearchInstanceOffering'
      { instanceCount =
          Core.Nothing,
        reservedElasticsearchInstanceOfferingId =
          pReservedElasticsearchInstanceOfferingId_,
        reservationName =
          pReservationName_
      }

-- | The number of Elasticsearch instances to reserve.
purchaseReservedElasticsearchInstanceOffering_instanceCount :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering (Core.Maybe Core.Natural)
purchaseReservedElasticsearchInstanceOffering_instanceCount = Lens.lens (\PurchaseReservedElasticsearchInstanceOffering' {instanceCount} -> instanceCount) (\s@PurchaseReservedElasticsearchInstanceOffering' {} a -> s {instanceCount = a} :: PurchaseReservedElasticsearchInstanceOffering)

-- | The ID of the reserved Elasticsearch instance offering to purchase.
purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Core.Text
purchaseReservedElasticsearchInstanceOffering_reservedElasticsearchInstanceOfferingId = Lens.lens (\PurchaseReservedElasticsearchInstanceOffering' {reservedElasticsearchInstanceOfferingId} -> reservedElasticsearchInstanceOfferingId) (\s@PurchaseReservedElasticsearchInstanceOffering' {} a -> s {reservedElasticsearchInstanceOfferingId = a} :: PurchaseReservedElasticsearchInstanceOffering)

-- | A customer-specified identifier to track this reservation.
purchaseReservedElasticsearchInstanceOffering_reservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOffering Core.Text
purchaseReservedElasticsearchInstanceOffering_reservationName = Lens.lens (\PurchaseReservedElasticsearchInstanceOffering' {reservationName} -> reservationName) (\s@PurchaseReservedElasticsearchInstanceOffering' {} a -> s {reservationName = a} :: PurchaseReservedElasticsearchInstanceOffering)

instance
  Core.AWSRequest
    PurchaseReservedElasticsearchInstanceOffering
  where
  type
    AWSResponse
      PurchaseReservedElasticsearchInstanceOffering =
      PurchaseReservedElasticsearchInstanceOfferingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseReservedElasticsearchInstanceOfferingResponse'
            Core.<$> (x Core..?> "ReservationName")
              Core.<*> (x Core..?> "ReservedElasticsearchInstanceId")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    PurchaseReservedElasticsearchInstanceOffering

instance
  Core.NFData
    PurchaseReservedElasticsearchInstanceOffering

instance
  Core.ToHeaders
    PurchaseReservedElasticsearchInstanceOffering
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToJSON
    PurchaseReservedElasticsearchInstanceOffering
  where
  toJSON
    PurchaseReservedElasticsearchInstanceOffering' {..} =
      Core.object
        ( Core.catMaybes
            [ ("InstanceCount" Core..=) Core.<$> instanceCount,
              Core.Just
                ( "ReservedElasticsearchInstanceOfferingId"
                    Core..= reservedElasticsearchInstanceOfferingId
                ),
              Core.Just
                ("ReservationName" Core..= reservationName)
            ]
        )

instance
  Core.ToPath
    PurchaseReservedElasticsearchInstanceOffering
  where
  toPath =
    Core.const
      "/2015-01-01/es/purchaseReservedInstanceOffering"

instance
  Core.ToQuery
    PurchaseReservedElasticsearchInstanceOffering
  where
  toQuery = Core.const Core.mempty

-- | Represents the output of a
-- @PurchaseReservedElasticsearchInstanceOffering@ operation.
--
-- /See:/ 'newPurchaseReservedElasticsearchInstanceOfferingResponse' smart constructor.
data PurchaseReservedElasticsearchInstanceOfferingResponse = PurchaseReservedElasticsearchInstanceOfferingResponse'
  { -- | The customer-specified identifier used to track this reservation.
    reservationName :: Core.Maybe Core.Text,
    -- | Details of the reserved Elasticsearch instance which was purchased.
    reservedElasticsearchInstanceId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PurchaseReservedElasticsearchInstanceOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservationName', 'purchaseReservedElasticsearchInstanceOfferingResponse_reservationName' - The customer-specified identifier used to track this reservation.
--
-- 'reservedElasticsearchInstanceId', 'purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId' - Details of the reserved Elasticsearch instance which was purchased.
--
-- 'httpStatus', 'purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedElasticsearchInstanceOfferingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PurchaseReservedElasticsearchInstanceOfferingResponse
newPurchaseReservedElasticsearchInstanceOfferingResponse
  pHttpStatus_ =
    PurchaseReservedElasticsearchInstanceOfferingResponse'
      { reservationName =
          Core.Nothing,
        reservedElasticsearchInstanceId =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The customer-specified identifier used to track this reservation.
purchaseReservedElasticsearchInstanceOfferingResponse_reservationName :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Core.Maybe Core.Text)
purchaseReservedElasticsearchInstanceOfferingResponse_reservationName = Lens.lens (\PurchaseReservedElasticsearchInstanceOfferingResponse' {reservationName} -> reservationName) (\s@PurchaseReservedElasticsearchInstanceOfferingResponse' {} a -> s {reservationName = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)

-- | Details of the reserved Elasticsearch instance which was purchased.
purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Core.Maybe Core.Text)
purchaseReservedElasticsearchInstanceOfferingResponse_reservedElasticsearchInstanceId = Lens.lens (\PurchaseReservedElasticsearchInstanceOfferingResponse' {reservedElasticsearchInstanceId} -> reservedElasticsearchInstanceId) (\s@PurchaseReservedElasticsearchInstanceOfferingResponse' {} a -> s {reservedElasticsearchInstanceId = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)

-- | The response's http status code.
purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedElasticsearchInstanceOfferingResponse Core.Int
purchaseReservedElasticsearchInstanceOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedElasticsearchInstanceOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedElasticsearchInstanceOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedElasticsearchInstanceOfferingResponse)

instance
  Core.NFData
    PurchaseReservedElasticsearchInstanceOfferingResponse
