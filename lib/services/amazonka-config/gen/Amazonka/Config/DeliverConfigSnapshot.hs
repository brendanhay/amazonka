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
-- Module      : Amazonka.Config.DeliverConfigSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules delivery of a configuration snapshot to the Amazon S3 bucket
-- in the specified delivery channel. After the delivery has started,
-- Config sends the following notifications using an Amazon SNS topic that
-- you have specified.
--
-- -   Notification of the start of the delivery.
--
-- -   Notification of the completion of the delivery, if the delivery was
--     successfully completed.
--
-- -   Notification of delivery failure, if the delivery failed.
module Amazonka.Config.DeliverConfigSnapshot
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeliverConfigSnapshot action.
--
-- /See:/ 'newDeliverConfigSnapshot' smart constructor.
data DeliverConfigSnapshot = DeliverConfigSnapshot'
  { -- | The name of the delivery channel through which the snapshot is
    -- delivered.
    deliveryChannelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeliverConfigSnapshot
newDeliverConfigSnapshot pDeliveryChannelName_ =
  DeliverConfigSnapshot'
    { deliveryChannelName =
        pDeliveryChannelName_
    }

-- | The name of the delivery channel through which the snapshot is
-- delivered.
deliverConfigSnapshot_deliveryChannelName :: Lens.Lens' DeliverConfigSnapshot Prelude.Text
deliverConfigSnapshot_deliveryChannelName = Lens.lens (\DeliverConfigSnapshot' {deliveryChannelName} -> deliveryChannelName) (\s@DeliverConfigSnapshot' {} a -> s {deliveryChannelName = a} :: DeliverConfigSnapshot)

instance Core.AWSRequest DeliverConfigSnapshot where
  type
    AWSResponse DeliverConfigSnapshot =
      DeliverConfigSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeliverConfigSnapshotResponse'
            Prelude.<$> (x Data..?> "configSnapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeliverConfigSnapshot where
  hashWithSalt _salt DeliverConfigSnapshot' {..} =
    _salt `Prelude.hashWithSalt` deliveryChannelName

instance Prelude.NFData DeliverConfigSnapshot where
  rnf DeliverConfigSnapshot' {..} =
    Prelude.rnf deliveryChannelName

instance Data.ToHeaders DeliverConfigSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeliverConfigSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeliverConfigSnapshot where
  toJSON DeliverConfigSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("deliveryChannelName" Data..= deliveryChannelName)
          ]
      )

instance Data.ToPath DeliverConfigSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery DeliverConfigSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeliverConfigSnapshot action, in JSON format.
--
-- /See:/ 'newDeliverConfigSnapshotResponse' smart constructor.
data DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse'
  { -- | The ID of the snapshot that is being created.
    configSnapshotId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeliverConfigSnapshotResponse
newDeliverConfigSnapshotResponse pHttpStatus_ =
  DeliverConfigSnapshotResponse'
    { configSnapshotId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the snapshot that is being created.
deliverConfigSnapshotResponse_configSnapshotId :: Lens.Lens' DeliverConfigSnapshotResponse (Prelude.Maybe Prelude.Text)
deliverConfigSnapshotResponse_configSnapshotId = Lens.lens (\DeliverConfigSnapshotResponse' {configSnapshotId} -> configSnapshotId) (\s@DeliverConfigSnapshotResponse' {} a -> s {configSnapshotId = a} :: DeliverConfigSnapshotResponse)

-- | The response's http status code.
deliverConfigSnapshotResponse_httpStatus :: Lens.Lens' DeliverConfigSnapshotResponse Prelude.Int
deliverConfigSnapshotResponse_httpStatus = Lens.lens (\DeliverConfigSnapshotResponse' {httpStatus} -> httpStatus) (\s@DeliverConfigSnapshotResponse' {} a -> s {httpStatus = a} :: DeliverConfigSnapshotResponse)

instance Prelude.NFData DeliverConfigSnapshotResponse where
  rnf DeliverConfigSnapshotResponse' {..} =
    Prelude.rnf configSnapshotId `Prelude.seq`
      Prelude.rnf httpStatus
