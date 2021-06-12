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
    deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions,
    deleteQueuedReservedInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQueuedReservedInstances' smart constructor.
data DeleteQueuedReservedInstances = DeleteQueuedReservedInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the Reserved Instances.
    reservedInstancesIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DeleteQueuedReservedInstances
newDeleteQueuedReservedInstances
  pReservedInstancesIds_ =
    DeleteQueuedReservedInstances'
      { dryRun =
          Core.Nothing,
        reservedInstancesIds =
          Lens._Coerce Lens.# pReservedInstancesIds_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteQueuedReservedInstances_dryRun :: Lens.Lens' DeleteQueuedReservedInstances (Core.Maybe Core.Bool)
deleteQueuedReservedInstances_dryRun = Lens.lens (\DeleteQueuedReservedInstances' {dryRun} -> dryRun) (\s@DeleteQueuedReservedInstances' {} a -> s {dryRun = a} :: DeleteQueuedReservedInstances)

-- | The IDs of the Reserved Instances.
deleteQueuedReservedInstances_reservedInstancesIds :: Lens.Lens' DeleteQueuedReservedInstances (Core.NonEmpty Core.Text)
deleteQueuedReservedInstances_reservedInstancesIds = Lens.lens (\DeleteQueuedReservedInstances' {reservedInstancesIds} -> reservedInstancesIds) (\s@DeleteQueuedReservedInstances' {} a -> s {reservedInstancesIds = a} :: DeleteQueuedReservedInstances) Core.. Lens._Coerce

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
            Core.<$> ( x Core..@? "successfulQueuedPurchaseDeletionSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> ( x Core..@? "failedQueuedPurchaseDeletionSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteQueuedReservedInstances

instance Core.NFData DeleteQueuedReservedInstances

instance Core.ToHeaders DeleteQueuedReservedInstances where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteQueuedReservedInstances where
  toPath = Core.const "/"

instance Core.ToQuery DeleteQueuedReservedInstances where
  toQuery DeleteQueuedReservedInstances' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteQueuedReservedInstances" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList
          "ReservedInstancesId"
          reservedInstancesIds
      ]

-- | /See:/ 'newDeleteQueuedReservedInstancesResponse' smart constructor.
data DeleteQueuedReservedInstancesResponse = DeleteQueuedReservedInstancesResponse'
  { -- | Information about the queued purchases that were successfully deleted.
    successfulQueuedPurchaseDeletions :: Core.Maybe [SuccessfulQueuedPurchaseDeletion],
    -- | Information about the queued purchases that could not be deleted.
    failedQueuedPurchaseDeletions :: Core.Maybe [FailedQueuedPurchaseDeletion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteQueuedReservedInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfulQueuedPurchaseDeletions', 'deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions' - Information about the queued purchases that were successfully deleted.
--
-- 'failedQueuedPurchaseDeletions', 'deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions' - Information about the queued purchases that could not be deleted.
--
-- 'httpStatus', 'deleteQueuedReservedInstancesResponse_httpStatus' - The response's http status code.
newDeleteQueuedReservedInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteQueuedReservedInstancesResponse
newDeleteQueuedReservedInstancesResponse pHttpStatus_ =
  DeleteQueuedReservedInstancesResponse'
    { successfulQueuedPurchaseDeletions =
        Core.Nothing,
      failedQueuedPurchaseDeletions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the queued purchases that were successfully deleted.
deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Core.Maybe [SuccessfulQueuedPurchaseDeletion])
deleteQueuedReservedInstancesResponse_successfulQueuedPurchaseDeletions = Lens.lens (\DeleteQueuedReservedInstancesResponse' {successfulQueuedPurchaseDeletions} -> successfulQueuedPurchaseDeletions) (\s@DeleteQueuedReservedInstancesResponse' {} a -> s {successfulQueuedPurchaseDeletions = a} :: DeleteQueuedReservedInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the queued purchases that could not be deleted.
deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions :: Lens.Lens' DeleteQueuedReservedInstancesResponse (Core.Maybe [FailedQueuedPurchaseDeletion])
deleteQueuedReservedInstancesResponse_failedQueuedPurchaseDeletions = Lens.lens (\DeleteQueuedReservedInstancesResponse' {failedQueuedPurchaseDeletions} -> failedQueuedPurchaseDeletions) (\s@DeleteQueuedReservedInstancesResponse' {} a -> s {failedQueuedPurchaseDeletions = a} :: DeleteQueuedReservedInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteQueuedReservedInstancesResponse_httpStatus :: Lens.Lens' DeleteQueuedReservedInstancesResponse Core.Int
deleteQueuedReservedInstancesResponse_httpStatus = Lens.lens (\DeleteQueuedReservedInstancesResponse' {httpStatus} -> httpStatus) (\s@DeleteQueuedReservedInstancesResponse' {} a -> s {httpStatus = a} :: DeleteQueuedReservedInstancesResponse)

instance
  Core.NFData
    DeleteQueuedReservedInstancesResponse
