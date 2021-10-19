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
-- Module      : Network.AWS.EC2.DeleteQueuedReservedInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queued purchases for the specified Reserved Instances.
module Network.AWS.EC2.DeleteQueuedReservedInstances
  ( -- * Creating a Request
    DeleteQueuedReservedInstances (..),
    newDeleteQueuedReservedInstances,

    -- * Request Lenses
    deleteQueuedReservedInstances_dryRun,
    deleteQueuedReservedInstances_reservedInstancesIds,

    -- * Destructuring the Response
    DeleteQueuedReservedInstancesResponse (..),
    newDeleteQueuedReservedInstancesResponse,

    -- * Response Lenses
    deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQueuedReservedInstances' smart constructor.
data DeleteQueuedReservedInstances = DeleteQueuedReservedInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the Reserved Instances.
    reservedInstancesIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueuedReservedInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteQueuedReservedInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'reservedInstancesIds', 'deleteQueuedReservedInstances_reservedInstancesIds' - The IDs of the Reserved Instances.
newDeleteQueuedReservedInstances ::
  -- | 'reservedInstancesIds'
  Prelude.NonEmpty Prelude.Text ->
  DeleteQueuedReservedInstances
newDeleteQueuedReservedInstances
  pReservedInstancesIds_ =
    DeleteQueuedReservedInstances'
      { dryRun =
          Prelude.Nothing,
        reservedInstancesIds =
          Lens.coerced Lens.# pReservedInstancesIds_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteQueuedReservedInstances_dryRun :: Lens.Lens' DeleteQueuedReservedInstances (Prelude.Maybe Prelude.Bool)
deleteQueuedReservedInstances_dryRun = Lens.lens (\DeleteQueuedReservedInstances' {dryRun} -> dryRun) (\s@DeleteQueuedReservedInstances' {} a -> s {dryRun = a} :: DeleteQueuedReservedInstances)

-- | The IDs of the Reserved Instances.
deleteQueuedReservedInstances_reservedInstancesIds :: Lens.Lens' DeleteQueuedReservedInstances (Prelude.NonEmpty Prelude.Text)
deleteQueuedReservedInstances_reservedInstancesIds = Lens.lens (\DeleteQueuedReservedInstances' {reservedInstancesIds} -> reservedInstancesIds) (\s@DeleteQueuedReservedInstances' {} a -> s {reservedInstancesIds = a} :: DeleteQueuedReservedInstances) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DeleteQueuedReservedInstances
  where
  type
    AWSResponse DeleteQueuedReservedInstances =
      DeleteQueuedReservedInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteQueuedReservedInstancesResponse'
            Prelude.<$> ( x Core..@? "failedQueuedPurchaseDeletionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "successfulQueuedPurchaseDeletionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteQueuedReservedInstances

instance Prelude.NFData DeleteQueuedReservedInstances

instance Core.ToHeaders DeleteQueuedReservedInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteQueuedReservedInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteQueuedReservedInstances where
  toQuery DeleteQueuedReservedInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteQueuedReservedInstances" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList
          "ReservedInstancesId"
          reservedInstancesIds
      ]

-- | /See:/ 'newDeleteQueuedReservedInstancesResponse' smart constructor.
data DeleteQueuedReservedInstancesResponse = DeleteQueuedReservedInstancesResponse'
  { -- | Information about the queued purchases that could not be deleted.
    failedQueuedPurchaseDeletions :: Prelude.Maybe [FailedQueuedPurchaseDeletion],
    -- | Information about the queued purchases that were successfully deleted.
    successfulQueuedPurchaseDeletions :: Prelude.Maybe [SuccessfulQueuedPurchaseDeletion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueuedReservedInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedQueuedPurchaseDeletions', 'deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions' - Information about the queued purchases that could not be deleted.
--
-- 'successfulQueuedPurchaseDeletions', 'deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions' - Information about the queued purchases that were successfully deleted.
--
-- 'httpStatus', 'deleteQueuedReservedInstancesResponse_httpStatus' - The response's http status code.
newDeleteQueuedReservedInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteQueuedReservedInstancesResponse
newDeleteQueuedReservedInstancesResponse pHttpStatus_ =
  DeleteQueuedReservedInstancesResponse'
    { failedQueuedPurchaseDeletions =
        Prelude.Nothing,
      successfulQueuedPurchaseDeletions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the queued purchases that could not be deleted.
deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Prelude.Maybe [FailedQueuedPurchaseDeletion])
deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions = Lens.lens (\DeleteQueuedReservedInstancesResponse' {failedQueuedPurchaseDeletions} -> failedQueuedPurchaseDeletions) (\s@DeleteQueuedReservedInstancesResponse' {} a -> s {failedQueuedPurchaseDeletions = a} :: DeleteQueuedReservedInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the queued purchases that were successfully deleted.
deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Prelude.Maybe [SuccessfulQueuedPurchaseDeletion])
deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions = Lens.lens (\DeleteQueuedReservedInstancesResponse' {successfulQueuedPurchaseDeletions} -> successfulQueuedPurchaseDeletions) (\s@DeleteQueuedReservedInstancesResponse' {} a -> s {successfulQueuedPurchaseDeletions = a} :: DeleteQueuedReservedInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteQueuedReservedInstancesResponse_httpStatus :: Lens.Lens' DeleteQueuedReservedInstancesResponse Prelude.Int
deleteQueuedReservedInstancesResponse_httpStatus = Lens.lens (\DeleteQueuedReservedInstancesResponse' {httpStatus} -> httpStatus) (\s@DeleteQueuedReservedInstancesResponse' {} a -> s {httpStatus = a} :: DeleteQueuedReservedInstancesResponse)

instance
  Prelude.NFData
    DeleteQueuedReservedInstancesResponse
