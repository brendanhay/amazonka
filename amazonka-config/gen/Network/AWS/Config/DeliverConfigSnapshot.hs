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
-- Module      : Network.AWS.Config.DeliverConfigSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules delivery of a configuration snapshot to the Amazon S3 bucket
-- in the specified delivery channel. After the delivery has started, AWS
-- Config sends the following notifications using an Amazon SNS topic that
-- you have specified.
--
-- -   Notification of the start of the delivery.
--
-- -   Notification of the completion of the delivery, if the delivery was
--     successfully completed.
--
-- -   Notification of delivery failure, if the delivery failed.
module Network.AWS.Config.DeliverConfigSnapshot
  ( -- * Creating a Request
    DeliverConfigSnapshot (..),
    newDeliverConfigSnapshot,

    -- * Request Lenses
    deliverConfigSnapshot_deliveryChannelName,

    -- * Destructuring the Response
    DeliverConfigSnapshotResponse (..),
    newDeliverConfigSnapshotResponse,

    -- * Response Lenses
    deliverConfigSnapshotResponse_configSnapshotId,
    deliverConfigSnapshotResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeliverConfigSnapshot action.
--
-- /See:/ 'newDeliverConfigSnapshot' smart constructor.
data DeliverConfigSnapshot = DeliverConfigSnapshot'
  { -- | The name of the delivery channel through which the snapshot is
    -- delivered.
    deliveryChannelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeliverConfigSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannelName', 'deliverConfigSnapshot_deliveryChannelName' - The name of the delivery channel through which the snapshot is
-- delivered.
newDeliverConfigSnapshot ::
  -- | 'deliveryChannelName'
  Core.Text ->
  DeliverConfigSnapshot
newDeliverConfigSnapshot pDeliveryChannelName_ =
  DeliverConfigSnapshot'
    { deliveryChannelName =
        pDeliveryChannelName_
    }

-- | The name of the delivery channel through which the snapshot is
-- delivered.
deliverConfigSnapshot_deliveryChannelName :: Lens.Lens' DeliverConfigSnapshot Core.Text
deliverConfigSnapshot_deliveryChannelName = Lens.lens (\DeliverConfigSnapshot' {deliveryChannelName} -> deliveryChannelName) (\s@DeliverConfigSnapshot' {} a -> s {deliveryChannelName = a} :: DeliverConfigSnapshot)

instance Core.AWSRequest DeliverConfigSnapshot where
  type
    AWSResponse DeliverConfigSnapshot =
      DeliverConfigSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeliverConfigSnapshotResponse'
            Core.<$> (x Core..?> "configSnapshotId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeliverConfigSnapshot

instance Core.NFData DeliverConfigSnapshot

instance Core.ToHeaders DeliverConfigSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeliverConfigSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeliverConfigSnapshot where
  toJSON DeliverConfigSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("deliveryChannelName" Core..= deliveryChannelName)
          ]
      )

instance Core.ToPath DeliverConfigSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery DeliverConfigSnapshot where
  toQuery = Core.const Core.mempty

-- | The output for the DeliverConfigSnapshot action, in JSON format.
--
-- /See:/ 'newDeliverConfigSnapshotResponse' smart constructor.
data DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse'
  { -- | The ID of the snapshot that is being created.
    configSnapshotId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeliverConfigSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configSnapshotId', 'deliverConfigSnapshotResponse_configSnapshotId' - The ID of the snapshot that is being created.
--
-- 'httpStatus', 'deliverConfigSnapshotResponse_httpStatus' - The response's http status code.
newDeliverConfigSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeliverConfigSnapshotResponse
newDeliverConfigSnapshotResponse pHttpStatus_ =
  DeliverConfigSnapshotResponse'
    { configSnapshotId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the snapshot that is being created.
deliverConfigSnapshotResponse_configSnapshotId :: Lens.Lens' DeliverConfigSnapshotResponse (Core.Maybe Core.Text)
deliverConfigSnapshotResponse_configSnapshotId = Lens.lens (\DeliverConfigSnapshotResponse' {configSnapshotId} -> configSnapshotId) (\s@DeliverConfigSnapshotResponse' {} a -> s {configSnapshotId = a} :: DeliverConfigSnapshotResponse)

-- | The response's http status code.
deliverConfigSnapshotResponse_httpStatus :: Lens.Lens' DeliverConfigSnapshotResponse Core.Int
deliverConfigSnapshotResponse_httpStatus = Lens.lens (\DeliverConfigSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeliverConfigSnapshotResponse' {} a -> s {httpStatus = a} :: DeliverConfigSnapshotResponse)

instance Core.NFData DeliverConfigSnapshotResponse
