{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.UpdateQueueStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the status of the queue.
module Network.AWS.Connect.UpdateQueueStatus
  ( -- * Creating a Request
    UpdateQueueStatus (..),
    newUpdateQueueStatus,

    -- * Request Lenses
    updateQueueStatus_instanceId,
    updateQueueStatus_queueId,
    updateQueueStatus_status,

    -- * Destructuring the Response
    UpdateQueueStatusResponse (..),
    newUpdateQueueStatusResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueStatus' smart constructor.
data UpdateQueueStatus = UpdateQueueStatus'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The status of the queue.
    status :: QueueStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQueueStatus_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'updateQueueStatus_queueId' - The identifier for the queue.
--
-- 'status', 'updateQueueStatus_status' - The status of the queue.
newUpdateQueueStatus ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'queueId'
  Prelude.Text ->
  -- | 'status'
  QueueStatus ->
  UpdateQueueStatus
newUpdateQueueStatus pInstanceId_ pQueueId_ pStatus_ =
  UpdateQueueStatus'
    { instanceId = pInstanceId_,
      queueId = pQueueId_,
      status = pStatus_
    }

-- | The identifier of the Amazon Connect instance.
updateQueueStatus_instanceId :: Lens.Lens' UpdateQueueStatus Prelude.Text
updateQueueStatus_instanceId = Lens.lens (\UpdateQueueStatus' {instanceId} -> instanceId) (\s@UpdateQueueStatus' {} a -> s {instanceId = a} :: UpdateQueueStatus)

-- | The identifier for the queue.
updateQueueStatus_queueId :: Lens.Lens' UpdateQueueStatus Prelude.Text
updateQueueStatus_queueId = Lens.lens (\UpdateQueueStatus' {queueId} -> queueId) (\s@UpdateQueueStatus' {} a -> s {queueId = a} :: UpdateQueueStatus)

-- | The status of the queue.
updateQueueStatus_status :: Lens.Lens' UpdateQueueStatus QueueStatus
updateQueueStatus_status = Lens.lens (\UpdateQueueStatus' {status} -> status) (\s@UpdateQueueStatus' {} a -> s {status = a} :: UpdateQueueStatus)

instance Prelude.AWSRequest UpdateQueueStatus where
  type Rs UpdateQueueStatus = UpdateQueueStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateQueueStatusResponse'

instance Prelude.Hashable UpdateQueueStatus

instance Prelude.NFData UpdateQueueStatus

instance Prelude.ToHeaders UpdateQueueStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateQueueStatus where
  toJSON UpdateQueueStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Status" Prelude..= status)]
      )

instance Prelude.ToPath UpdateQueueStatus where
  toPath UpdateQueueStatus' {..} =
    Prelude.mconcat
      [ "/queues/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS queueId,
        "/status"
      ]

instance Prelude.ToQuery UpdateQueueStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueStatusResponse' smart constructor.
data UpdateQueueStatusResponse = UpdateQueueStatusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueueStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueStatusResponse ::
  UpdateQueueStatusResponse
newUpdateQueueStatusResponse =
  UpdateQueueStatusResponse'

instance Prelude.NFData UpdateQueueStatusResponse
