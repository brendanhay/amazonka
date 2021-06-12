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
-- Module      : Network.AWS.MediaConvert.UpdateQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing queues.
module Network.AWS.MediaConvert.UpdateQueue
  ( -- * Creating a Request
    UpdateQueue (..),
    newUpdateQueue,

    -- * Request Lenses
    updateQueue_status,
    updateQueue_reservationPlanSettings,
    updateQueue_description,
    updateQueue_name,

    -- * Destructuring the Response
    UpdateQueueResponse (..),
    newUpdateQueueResponse,

    -- * Response Lenses
    updateQueueResponse_queue,
    updateQueueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQueue' smart constructor.
data UpdateQueue = UpdateQueue'
  { -- | Pause or activate a queue by changing its status between ACTIVE and
    -- PAUSED. If you pause a queue, jobs in that queue won\'t begin. Jobs that
    -- are running when you pause the queue continue to run until they finish
    -- or result in an error.
    status :: Core.Maybe QueueStatus,
    -- | The new details of your pricing plan for your reserved queue. When you
    -- set up a new pricing plan to replace an expired one, you enter into
    -- another 12-month commitment. When you add capacity to your queue by
    -- increasing the number of RTS, you extend the term of your commitment to
    -- 12 months from when you add capacity. After you make these commitments,
    -- you can\'t cancel them.
    reservationPlanSettings :: Core.Maybe ReservationPlanSettings,
    -- | The new description for the queue, if you are changing it.
    description :: Core.Maybe Core.Text,
    -- | The name of the queue that you are modifying.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateQueue_status' - Pause or activate a queue by changing its status between ACTIVE and
-- PAUSED. If you pause a queue, jobs in that queue won\'t begin. Jobs that
-- are running when you pause the queue continue to run until they finish
-- or result in an error.
--
-- 'reservationPlanSettings', 'updateQueue_reservationPlanSettings' - The new details of your pricing plan for your reserved queue. When you
-- set up a new pricing plan to replace an expired one, you enter into
-- another 12-month commitment. When you add capacity to your queue by
-- increasing the number of RTS, you extend the term of your commitment to
-- 12 months from when you add capacity. After you make these commitments,
-- you can\'t cancel them.
--
-- 'description', 'updateQueue_description' - The new description for the queue, if you are changing it.
--
-- 'name', 'updateQueue_name' - The name of the queue that you are modifying.
newUpdateQueue ::
  -- | 'name'
  Core.Text ->
  UpdateQueue
newUpdateQueue pName_ =
  UpdateQueue'
    { status = Core.Nothing,
      reservationPlanSettings = Core.Nothing,
      description = Core.Nothing,
      name = pName_
    }

-- | Pause or activate a queue by changing its status between ACTIVE and
-- PAUSED. If you pause a queue, jobs in that queue won\'t begin. Jobs that
-- are running when you pause the queue continue to run until they finish
-- or result in an error.
updateQueue_status :: Lens.Lens' UpdateQueue (Core.Maybe QueueStatus)
updateQueue_status = Lens.lens (\UpdateQueue' {status} -> status) (\s@UpdateQueue' {} a -> s {status = a} :: UpdateQueue)

-- | The new details of your pricing plan for your reserved queue. When you
-- set up a new pricing plan to replace an expired one, you enter into
-- another 12-month commitment. When you add capacity to your queue by
-- increasing the number of RTS, you extend the term of your commitment to
-- 12 months from when you add capacity. After you make these commitments,
-- you can\'t cancel them.
updateQueue_reservationPlanSettings :: Lens.Lens' UpdateQueue (Core.Maybe ReservationPlanSettings)
updateQueue_reservationPlanSettings = Lens.lens (\UpdateQueue' {reservationPlanSettings} -> reservationPlanSettings) (\s@UpdateQueue' {} a -> s {reservationPlanSettings = a} :: UpdateQueue)

-- | The new description for the queue, if you are changing it.
updateQueue_description :: Lens.Lens' UpdateQueue (Core.Maybe Core.Text)
updateQueue_description = Lens.lens (\UpdateQueue' {description} -> description) (\s@UpdateQueue' {} a -> s {description = a} :: UpdateQueue)

-- | The name of the queue that you are modifying.
updateQueue_name :: Lens.Lens' UpdateQueue Core.Text
updateQueue_name = Lens.lens (\UpdateQueue' {name} -> name) (\s@UpdateQueue' {} a -> s {name = a} :: UpdateQueue)

instance Core.AWSRequest UpdateQueue where
  type AWSResponse UpdateQueue = UpdateQueueResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateQueueResponse'
            Core.<$> (x Core..?> "queue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateQueue

instance Core.NFData UpdateQueue

instance Core.ToHeaders UpdateQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateQueue where
  toJSON UpdateQueue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("status" Core..=) Core.<$> status,
            ("reservationPlanSettings" Core..=)
              Core.<$> reservationPlanSettings,
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateQueue where
  toPath UpdateQueue' {..} =
    Core.mconcat
      ["/2017-08-29/queues/", Core.toBS name]

instance Core.ToQuery UpdateQueue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateQueueResponse' smart constructor.
data UpdateQueueResponse = UpdateQueueResponse'
  { -- | You can use queues to manage the resources that are available to your
    -- AWS account for running multiple transcoding jobs at the same time. If
    -- you don\'t specify a queue, the service sends all jobs through the
    -- default queue. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
    queue :: Core.Maybe Queue,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queue', 'updateQueueResponse_queue' - You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
--
-- 'httpStatus', 'updateQueueResponse_httpStatus' - The response's http status code.
newUpdateQueueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateQueueResponse
newUpdateQueueResponse pHttpStatus_ =
  UpdateQueueResponse'
    { queue = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
updateQueueResponse_queue :: Lens.Lens' UpdateQueueResponse (Core.Maybe Queue)
updateQueueResponse_queue = Lens.lens (\UpdateQueueResponse' {queue} -> queue) (\s@UpdateQueueResponse' {} a -> s {queue = a} :: UpdateQueueResponse)

-- | The response's http status code.
updateQueueResponse_httpStatus :: Lens.Lens' UpdateQueueResponse Core.Int
updateQueueResponse_httpStatus = Lens.lens (\UpdateQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateQueueResponse' {} a -> s {httpStatus = a} :: UpdateQueueResponse)

instance Core.NFData UpdateQueueResponse
