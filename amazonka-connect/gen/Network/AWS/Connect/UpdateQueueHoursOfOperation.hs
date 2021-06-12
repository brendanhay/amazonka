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
-- Module      : Network.AWS.Connect.UpdateQueueHoursOfOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the hours of operation for the specified queue.
module Network.AWS.Connect.UpdateQueueHoursOfOperation
  ( -- * Creating a Request
    UpdateQueueHoursOfOperation (..),
    newUpdateQueueHoursOfOperation,

    -- * Request Lenses
    updateQueueHoursOfOperation_instanceId,
    updateQueueHoursOfOperation_queueId,
    updateQueueHoursOfOperation_hoursOfOperationId,

    -- * Destructuring the Response
    UpdateQueueHoursOfOperationResponse (..),
    newUpdateQueueHoursOfOperationResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueueHoursOfOperation' smart constructor.
data UpdateQueueHoursOfOperation = UpdateQueueHoursOfOperation'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the queue.
    queueId :: Core.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueHoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateQueueHoursOfOperation_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'queueId', 'updateQueueHoursOfOperation_queueId' - The identifier for the queue.
--
-- 'hoursOfOperationId', 'updateQueueHoursOfOperation_hoursOfOperationId' - The identifier for the hours of operation.
newUpdateQueueHoursOfOperation ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'queueId'
  Core.Text ->
  -- | 'hoursOfOperationId'
  Core.Text ->
  UpdateQueueHoursOfOperation
newUpdateQueueHoursOfOperation
  pInstanceId_
  pQueueId_
  pHoursOfOperationId_ =
    UpdateQueueHoursOfOperation'
      { instanceId =
          pInstanceId_,
        queueId = pQueueId_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | The identifier of the Amazon Connect instance.
updateQueueHoursOfOperation_instanceId :: Lens.Lens' UpdateQueueHoursOfOperation Core.Text
updateQueueHoursOfOperation_instanceId = Lens.lens (\UpdateQueueHoursOfOperation' {instanceId} -> instanceId) (\s@UpdateQueueHoursOfOperation' {} a -> s {instanceId = a} :: UpdateQueueHoursOfOperation)

-- | The identifier for the queue.
updateQueueHoursOfOperation_queueId :: Lens.Lens' UpdateQueueHoursOfOperation Core.Text
updateQueueHoursOfOperation_queueId = Lens.lens (\UpdateQueueHoursOfOperation' {queueId} -> queueId) (\s@UpdateQueueHoursOfOperation' {} a -> s {queueId = a} :: UpdateQueueHoursOfOperation)

-- | The identifier for the hours of operation.
updateQueueHoursOfOperation_hoursOfOperationId :: Lens.Lens' UpdateQueueHoursOfOperation Core.Text
updateQueueHoursOfOperation_hoursOfOperationId = Lens.lens (\UpdateQueueHoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@UpdateQueueHoursOfOperation' {} a -> s {hoursOfOperationId = a} :: UpdateQueueHoursOfOperation)

instance Core.AWSRequest UpdateQueueHoursOfOperation where
  type
    AWSResponse UpdateQueueHoursOfOperation =
      UpdateQueueHoursOfOperationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQueueHoursOfOperationResponse'

instance Core.Hashable UpdateQueueHoursOfOperation

instance Core.NFData UpdateQueueHoursOfOperation

instance Core.ToHeaders UpdateQueueHoursOfOperation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQueueHoursOfOperation where
  toJSON UpdateQueueHoursOfOperation' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("HoursOfOperationId" Core..= hoursOfOperationId)
          ]
      )

instance Core.ToPath UpdateQueueHoursOfOperation where
  toPath UpdateQueueHoursOfOperation' {..} =
    Core.mconcat
      [ "/queues/",
        Core.toBS instanceId,
        "/",
        Core.toBS queueId,
        "/hours-of-operation"
      ]

instance Core.ToQuery UpdateQueueHoursOfOperation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQueueHoursOfOperationResponse' smart constructor.
data UpdateQueueHoursOfOperationResponse = UpdateQueueHoursOfOperationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueHoursOfOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQueueHoursOfOperationResponse ::
  UpdateQueueHoursOfOperationResponse
newUpdateQueueHoursOfOperationResponse =
  UpdateQueueHoursOfOperationResponse'

instance
  Core.NFData
    UpdateQueueHoursOfOperationResponse
