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
-- Module      : Amazonka.Redshift.PurchaseReservedNodeOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved nodes. Amazon Redshift offers a
-- predefined set of reserved node offerings. You can purchase one or more
-- of the offerings. You can call the DescribeReservedNodeOfferings API to
-- obtain the available reserved node offerings. You can call this API by
-- providing a specific reserved node offering and the number of nodes you
-- want to reserve.
--
-- For more information about reserved node offerings, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes>
-- in the /Amazon Redshift Cluster Management Guide/.
module Amazonka.Redshift.PurchaseReservedNodeOffering
  ( -- * Creating a Request
    PurchaseReservedNodeOffering (..),
    newPurchaseReservedNodeOffering,

    -- * Request Lenses
    purchaseReservedNodeOffering_nodeCount,
    purchaseReservedNodeOffering_reservedNodeOfferingId,

    -- * Destructuring the Response
    PurchaseReservedNodeOfferingResponse (..),
    newPurchaseReservedNodeOfferingResponse,

    -- * Response Lenses
    purchaseReservedNodeOfferingResponse_reservedNode,
    purchaseReservedNodeOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newPurchaseReservedNodeOffering' smart constructor.
data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering'
  { -- | The number of reserved nodes that you want to purchase.
    --
    -- Default: @1@
    nodeCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the reserved node offering you want to
    -- purchase.
    reservedNodeOfferingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedNodeOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeCount', 'purchaseReservedNodeOffering_nodeCount' - The number of reserved nodes that you want to purchase.
--
-- Default: @1@
--
-- 'reservedNodeOfferingId', 'purchaseReservedNodeOffering_reservedNodeOfferingId' - The unique identifier of the reserved node offering you want to
-- purchase.
newPurchaseReservedNodeOffering ::
  -- | 'reservedNodeOfferingId'
  Prelude.Text ->
  PurchaseReservedNodeOffering
newPurchaseReservedNodeOffering
  pReservedNodeOfferingId_ =
    PurchaseReservedNodeOffering'
      { nodeCount =
          Prelude.Nothing,
        reservedNodeOfferingId =
          pReservedNodeOfferingId_
      }

-- | The number of reserved nodes that you want to purchase.
--
-- Default: @1@
purchaseReservedNodeOffering_nodeCount :: Lens.Lens' PurchaseReservedNodeOffering (Prelude.Maybe Prelude.Int)
purchaseReservedNodeOffering_nodeCount = Lens.lens (\PurchaseReservedNodeOffering' {nodeCount} -> nodeCount) (\s@PurchaseReservedNodeOffering' {} a -> s {nodeCount = a} :: PurchaseReservedNodeOffering)

-- | The unique identifier of the reserved node offering you want to
-- purchase.
purchaseReservedNodeOffering_reservedNodeOfferingId :: Lens.Lens' PurchaseReservedNodeOffering Prelude.Text
purchaseReservedNodeOffering_reservedNodeOfferingId = Lens.lens (\PurchaseReservedNodeOffering' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@PurchaseReservedNodeOffering' {} a -> s {reservedNodeOfferingId = a} :: PurchaseReservedNodeOffering)

instance Core.AWSRequest PurchaseReservedNodeOffering where
  type
    AWSResponse PurchaseReservedNodeOffering =
      PurchaseReservedNodeOfferingResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PurchaseReservedNodeOfferingResult"
      ( \s h x ->
          PurchaseReservedNodeOfferingResponse'
            Prelude.<$> (x Data..@? "ReservedNode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PurchaseReservedNodeOffering
  where
  hashWithSalt _salt PurchaseReservedNodeOffering' {..} =
    _salt `Prelude.hashWithSalt` nodeCount
      `Prelude.hashWithSalt` reservedNodeOfferingId

instance Prelude.NFData PurchaseReservedNodeOffering where
  rnf PurchaseReservedNodeOffering' {..} =
    Prelude.rnf nodeCount
      `Prelude.seq` Prelude.rnf reservedNodeOfferingId

instance Data.ToHeaders PurchaseReservedNodeOffering where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PurchaseReservedNodeOffering where
  toPath = Prelude.const "/"

instance Data.ToQuery PurchaseReservedNodeOffering where
  toQuery PurchaseReservedNodeOffering' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "PurchaseReservedNodeOffering" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "NodeCount" Data.=: nodeCount,
        "ReservedNodeOfferingId"
          Data.=: reservedNodeOfferingId
      ]

-- | /See:/ 'newPurchaseReservedNodeOfferingResponse' smart constructor.
data PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse'
  { reservedNode :: Prelude.Maybe ReservedNode,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PurchaseReservedNodeOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNode', 'purchaseReservedNodeOfferingResponse_reservedNode' - Undocumented member.
--
-- 'httpStatus', 'purchaseReservedNodeOfferingResponse_httpStatus' - The response's http status code.
newPurchaseReservedNodeOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PurchaseReservedNodeOfferingResponse
newPurchaseReservedNodeOfferingResponse pHttpStatus_ =
  PurchaseReservedNodeOfferingResponse'
    { reservedNode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
purchaseReservedNodeOfferingResponse_reservedNode :: Lens.Lens' PurchaseReservedNodeOfferingResponse (Prelude.Maybe ReservedNode)
purchaseReservedNodeOfferingResponse_reservedNode = Lens.lens (\PurchaseReservedNodeOfferingResponse' {reservedNode} -> reservedNode) (\s@PurchaseReservedNodeOfferingResponse' {} a -> s {reservedNode = a} :: PurchaseReservedNodeOfferingResponse)

-- | The response's http status code.
purchaseReservedNodeOfferingResponse_httpStatus :: Lens.Lens' PurchaseReservedNodeOfferingResponse Prelude.Int
purchaseReservedNodeOfferingResponse_httpStatus = Lens.lens (\PurchaseReservedNodeOfferingResponse' {httpStatus} -> httpStatus) (\s@PurchaseReservedNodeOfferingResponse' {} a -> s {httpStatus = a} :: PurchaseReservedNodeOfferingResponse)

instance
  Prelude.NFData
    PurchaseReservedNodeOfferingResponse
  where
  rnf PurchaseReservedNodeOfferingResponse' {..} =
    Prelude.rnf reservedNode
      `Prelude.seq` Prelude.rnf httpStatus
