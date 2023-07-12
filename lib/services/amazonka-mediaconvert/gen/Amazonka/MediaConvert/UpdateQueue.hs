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
-- Module      : Amazonka.MediaConvert.UpdateQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing queues.
module Amazonka.MediaConvert.UpdateQueue
  ( -- * Creating a Request
    UpdateQueue (..),
    newUpdateQueue,

    -- * Request Lenses
    updateQueue_description,
    updateQueue_reservationPlanSettings,
    updateQueue_status,
    updateQueue_name,

    -- * Destructuring the Response
    UpdateQueueResponse (..),
    newUpdateQueueResponse,

    -- * Response Lenses
    updateQueueResponse_queue,
    updateQueueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQueue' smart constructor.
data UpdateQueue = UpdateQueue'
  { -- | The new description for the queue, if you are changing it.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new details of your pricing plan for your reserved queue. When you
    -- set up a new pricing plan to replace an expired one, you enter into
    -- another 12-month commitment. When you add capacity to your queue by
    -- increasing the number of RTS, you extend the term of your commitment to
    -- 12 months from when you add capacity. After you make these commitments,
    -- you can\'t cancel them.
    reservationPlanSettings :: Prelude.Maybe ReservationPlanSettings,
    -- | Pause or activate a queue by changing its status between ACTIVE and
    -- PAUSED. If you pause a queue, jobs in that queue won\'t begin. Jobs that
    -- are running when you pause the queue continue to run until they finish
    -- or result in an error.
    status :: Prelude.Maybe QueueStatus,
    -- | The name of the queue that you are modifying.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateQueue_description' - The new description for the queue, if you are changing it.
--
-- 'reservationPlanSettings', 'updateQueue_reservationPlanSettings' - The new details of your pricing plan for your reserved queue. When you
-- set up a new pricing plan to replace an expired one, you enter into
-- another 12-month commitment. When you add capacity to your queue by
-- increasing the number of RTS, you extend the term of your commitment to
-- 12 months from when you add capacity. After you make these commitments,
-- you can\'t cancel them.
--
-- 'status', 'updateQueue_status' - Pause or activate a queue by changing its status between ACTIVE and
-- PAUSED. If you pause a queue, jobs in that queue won\'t begin. Jobs that
-- are running when you pause the queue continue to run until they finish
-- or result in an error.
--
-- 'name', 'updateQueue_name' - The name of the queue that you are modifying.
newUpdateQueue ::
  -- | 'name'
  Prelude.Text ->
  UpdateQueue
newUpdateQueue pName_ =
  UpdateQueue'
    { description = Prelude.Nothing,
      reservationPlanSettings = Prelude.Nothing,
      status = Prelude.Nothing,
      name = pName_
    }

-- | The new description for the queue, if you are changing it.
updateQueue_description :: Lens.Lens' UpdateQueue (Prelude.Maybe Prelude.Text)
updateQueue_description = Lens.lens (\UpdateQueue' {description} -> description) (\s@UpdateQueue' {} a -> s {description = a} :: UpdateQueue)

-- | The new details of your pricing plan for your reserved queue. When you
-- set up a new pricing plan to replace an expired one, you enter into
-- another 12-month commitment. When you add capacity to your queue by
-- increasing the number of RTS, you extend the term of your commitment to
-- 12 months from when you add capacity. After you make these commitments,
-- you can\'t cancel them.
updateQueue_reservationPlanSettings :: Lens.Lens' UpdateQueue (Prelude.Maybe ReservationPlanSettings)
updateQueue_reservationPlanSettings = Lens.lens (\UpdateQueue' {reservationPlanSettings} -> reservationPlanSettings) (\s@UpdateQueue' {} a -> s {reservationPlanSettings = a} :: UpdateQueue)

-- | Pause or activate a queue by changing its status between ACTIVE and
-- PAUSED. If you pause a queue, jobs in that queue won\'t begin. Jobs that
-- are running when you pause the queue continue to run until they finish
-- or result in an error.
updateQueue_status :: Lens.Lens' UpdateQueue (Prelude.Maybe QueueStatus)
updateQueue_status = Lens.lens (\UpdateQueue' {status} -> status) (\s@UpdateQueue' {} a -> s {status = a} :: UpdateQueue)

-- | The name of the queue that you are modifying.
updateQueue_name :: Lens.Lens' UpdateQueue Prelude.Text
updateQueue_name = Lens.lens (\UpdateQueue' {name} -> name) (\s@UpdateQueue' {} a -> s {name = a} :: UpdateQueue)

instance Core.AWSRequest UpdateQueue where
  type AWSResponse UpdateQueue = UpdateQueueResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateQueueResponse'
            Prelude.<$> (x Data..?> "queue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateQueue where
  hashWithSalt _salt UpdateQueue' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reservationPlanSettings
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateQueue where
  rnf UpdateQueue' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf reservationPlanSettings
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateQueue where
  toJSON UpdateQueue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("reservationPlanSettings" Data..=)
              Prelude.<$> reservationPlanSettings,
            ("status" Data..=) Prelude.<$> status
          ]
      )

instance Data.ToPath UpdateQueue where
  toPath UpdateQueue' {..} =
    Prelude.mconcat
      ["/2017-08-29/queues/", Data.toBS name]

instance Data.ToQuery UpdateQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQueueResponse' smart constructor.
data UpdateQueueResponse = UpdateQueueResponse'
  { -- | You can use queues to manage the resources that are available to your
    -- AWS account for running multiple transcoding jobs at the same time. If
    -- you don\'t specify a queue, the service sends all jobs through the
    -- default queue. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
    queue :: Prelude.Maybe Queue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateQueueResponse
newUpdateQueueResponse pHttpStatus_ =
  UpdateQueueResponse'
    { queue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
updateQueueResponse_queue :: Lens.Lens' UpdateQueueResponse (Prelude.Maybe Queue)
updateQueueResponse_queue = Lens.lens (\UpdateQueueResponse' {queue} -> queue) (\s@UpdateQueueResponse' {} a -> s {queue = a} :: UpdateQueueResponse)

-- | The response's http status code.
updateQueueResponse_httpStatus :: Lens.Lens' UpdateQueueResponse Prelude.Int
updateQueueResponse_httpStatus = Lens.lens (\UpdateQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateQueueResponse' {} a -> s {httpStatus = a} :: UpdateQueueResponse)

instance Prelude.NFData UpdateQueueResponse where
  rnf UpdateQueueResponse' {..} =
    Prelude.rnf queue
      `Prelude.seq` Prelude.rnf httpStatus
