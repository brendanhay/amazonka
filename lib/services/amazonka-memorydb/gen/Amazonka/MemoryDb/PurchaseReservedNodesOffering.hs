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
-- Module      : Amazonka.MemoryDb.PurchaseReservedNodesOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase a reserved node offering. Reserved nodes are not
-- eligible for cancellation and are non-refundable.
module Amazonka.MemoryDb.PurchaseReservedNodesOffering
  ( -- * Creating a Request
    PurchaseReservedNodesOffering (..),
    newPurchaseReservedNodesOffering,

    -- * Request Lenses
    purchaseReservedNodesOffering_nodeCount,
    purchaseReservedNodesOffering_reservationId,
    purchaseReservedNodesOffering_tags,
    purchaseReservedNodesOffering_reservedNodesOfferingId,

    -- * Destructuring the Response
    PurchaseReservedNodesOfferingResponse (..),
    newPurchaseReservedNodesOfferingResponse,

    -- * Response Lenses
    purchaseReservedNodesOfferingResponse_reservedNode,
    purchaseReservedNodesOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPurchaseReservedNodesOffering' smart constructor.
data PurchaseReservedNodesOffering = PurchaseReservedNodesOffering'
  { -- | The number of node instances to reserve.
    nodeCount :: Prelude.Maybe Prelude.Int,
    -- | A customer-specified identifier to track this reservation.
    reservationId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to be added to this resource. A tag is a key-value pair.
    -- A tag key must be accompanied by a tag value, although null is accepted.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the reserved node offering to purchase.
    reservedNodesOfferingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedNodesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeCount', 'purchaseReservedNodesOffering_nodeCount' - The number of node instances to reserve.
--
-- 'reservationId', 'purchaseReservedNodesOffering_reservationId' - A customer-specified identifier to track this reservation.
--
-- 'tags', 'purchaseReservedNodesOffering_tags' - A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
--
-- 'reservedNodesOfferingId', 'purchaseReservedNodesOffering_reservedNodesOfferingId' - The ID of the reserved node offering to purchase.
newPurchaseReservedNodesOffering ::
  -- | 'reservedNodesOfferingId'
  Prelude.Text ->
  PurchaseReservedNodesOffering
newPurchaseReservedNodesOffering
  pReservedNodesOfferingId_ =
    PurchaseReservedNodesOffering'
      { nodeCount =
          Prelude.Nothing,
        reservationId = Prelude.Nothing,
        tags = Prelude.Nothing,
        reservedNodesOfferingId =
          pReservedNodesOfferingId_
      }

-- | The number of node instances to reserve.
purchaseReservedNodesOffering_nodeCount :: Lens.Lens' PurchaseReservedNodesOffering (Prelude.Maybe Prelude.Int)
purchaseReservedNodesOffering_nodeCount = Lens.lens (\PurchaseReservedNodesOffering' {nodeCount} -> nodeCount) (\s@PurchaseReservedNodesOffering' {} a -> s {nodeCount = a} :: PurchaseReservedNodesOffering)

-- | A customer-specified identifier to track this reservation.
purchaseReservedNodesOffering_reservationId :: Lens.Lens' PurchaseReservedNodesOffering (Prelude.Maybe Prelude.Text)
purchaseReservedNodesOffering_reservationId = Lens.lens (\PurchaseReservedNodesOffering' {reservationId} -> reservationId) (\s@PurchaseReservedNodesOffering' {} a -> s {reservationId = a} :: PurchaseReservedNodesOffering)

-- | A list of tags to be added to this resource. A tag is a key-value pair.
-- A tag key must be accompanied by a tag value, although null is accepted.
purchaseReservedNodesOffering_tags :: Lens.Lens' PurchaseReservedNodesOffering (Prelude.Maybe [Tag])
purchaseReservedNodesOffering_tags = Lens.lens (\PurchaseReservedNodesOffering' {tags} -> tags) (\s@PurchaseReservedNodesOffering' {} a -> s {tags = a} :: PurchaseReservedNodesOffering) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the reserved node offering to purchase.
purchaseReservedNodesOffering_reservedNodesOfferingId :: Lens.Lens' PurchaseReservedNodesOffering Prelude.Text
purchaseReservedNodesOffering_reservedNodesOfferingId = Lens.lens (\PurchaseReservedNodesOffering' {reservedNodesOfferingId} -> reservedNodesOfferingId) (\s@PurchaseReservedNodesOffering' {} a -> s {reservedNodesOfferingId = a} :: PurchaseReservedNodesOffering)

instance
  Core.AWSRequest
    PurchaseReservedNodesOffering
  where
  type
    AWSResponse PurchaseReservedNodesOffering =
      PurchaseReservedNodesOfferingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PurchaseReservedNodesOfferingResponse'
            Prelude.<$> (x Data..?> "ReservedNode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PurchaseReservedNodesOffering
  where
  hashWithSalt _salt PurchaseReservedNodesOffering' {..} =
    _salt
      `Prelude.hashWithSalt` nodeCount
      `Prelude.hashWithSalt` reservationId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` reservedNodesOfferingId

instance Prelude.NFData PurchaseReservedNodesOffering where
  rnf PurchaseReservedNodesOffering' {..} =
    Prelude.rnf nodeCount
      `Prelude.seq` Prelude.rnf reservationId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf reservedNodesOfferingId

instance Data.ToHeaders PurchaseReservedNodesOffering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.PurchaseReservedNodesOffering" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PurchaseReservedNodesOffering where
  toJSON PurchaseReservedNodesOffering' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NodeCount" Data..=) Prelude.<$> nodeCount,
            ("ReservationId" Data..=) Prelude.<$> reservationId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "ReservedNodesOfferingId"
                  Data..= reservedNodesOfferingId
              )
          ]
      )

instance Data.ToPath PurchaseReservedNodesOffering where
  toPath = Prelude.const "/"

instance Data.ToQuery PurchaseReservedNodesOffering where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPurchaseReservedNodesOfferingResponse' smart constructor.
data PurchaseReservedNodesOfferingResponse = PurchaseReservedNodesOfferingResponse'
  { -- | Represents the output of a @PurchaseReservedNodesOffering@ operation.
    reservedNode :: Prelude.Maybe ReservedNode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedNodesOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNode', 'purchaseReservedNodesOfferingResponse_reservedNode' - Represents the output of a @PurchaseReservedNodesOffering@ operation.
--
-- 'httpStatus', 'purchaseReservedNodesOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedNodesOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseReservedNodesOfferingResponse
newPurchaseReservedNodesOfferingResponse pHttpStatus_ =
  PurchaseReservedNodesOfferingResponse'
    { reservedNode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the output of a @PurchaseReservedNodesOffering@ operation.
purchaseReservedNodesOfferingResponse_reservedNode :: Lens.Lens' PurchaseReservedNodesOfferingResponse (Prelude.Maybe ReservedNode)
purchaseReservedNodesOfferingResponse_reservedNode = Lens.lens (\PurchaseReservedNodesOfferingResponse' {reservedNode} -> reservedNode) (\s@PurchaseReservedNodesOfferingResponse' {} a -> s {reservedNode = a} :: PurchaseReservedNodesOfferingResponse)

-- | The response's http status code.
purchaseReservedNodesOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedNodesOfferingResponse Prelude.Int
purchaseReservedNodesOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedNodesOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedNodesOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedNodesOfferingResponse)

instance
  Prelude.NFData
    PurchaseReservedNodesOfferingResponse
  where
  rnf PurchaseReservedNodesOfferingResponse' {..} =
    Prelude.rnf reservedNode
      `Prelude.seq` Prelude.rnf httpStatus
