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
-- Module      : Network.AWS.MediaConvert.CreateQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new transcoding queue. For information about queues, see
-- Working With Queues in the User Guide at
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html
module Network.AWS.MediaConvert.CreateQueue
  ( -- * Creating a Request
    CreateQueue (..),
    newCreateQueue,

    -- * Request Lenses
    createQueue_status,
    createQueue_tags,
    createQueue_reservationPlanSettings,
    createQueue_description,
    createQueue_pricingPlan,
    createQueue_name,

    -- * Destructuring the Response
    CreateQueueResponse (..),
    newCreateQueueResponse,

    -- * Response Lenses
    createQueueResponse_queue,
    createQueueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateQueue' smart constructor.
data CreateQueue = CreateQueue'
  { -- | Initial state of the queue. If you create a paused queue, then jobs in
    -- that queue won\'t begin.
    status :: Prelude.Maybe QueueStatus,
    -- | The tags that you want to add to the resource. You can tag resources
    -- with a key-value pair or with only a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Details about the pricing plan for your reserved queue. Required for
    -- reserved queues and not applicable to on-demand queues.
    reservationPlanSettings :: Prelude.Maybe ReservationPlanSettings,
    -- | Optional. A description of the queue that you are creating.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the pricing plan for the queue is on-demand or
    -- reserved. For on-demand, you pay per minute, billed in increments of .01
    -- minute. For reserved, you pay for the transcoding capacity of the entire
    -- queue, regardless of how much or how little you use it. Reserved pricing
    -- requires a 12-month commitment. When you use the API to create a queue,
    -- the default is on-demand.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The name of the queue that you are creating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createQueue_status' - Initial state of the queue. If you create a paused queue, then jobs in
-- that queue won\'t begin.
--
-- 'tags', 'createQueue_tags' - The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
--
-- 'reservationPlanSettings', 'createQueue_reservationPlanSettings' - Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
--
-- 'description', 'createQueue_description' - Optional. A description of the queue that you are creating.
--
-- 'pricingPlan', 'createQueue_pricingPlan' - Specifies whether the pricing plan for the queue is on-demand or
-- reserved. For on-demand, you pay per minute, billed in increments of .01
-- minute. For reserved, you pay for the transcoding capacity of the entire
-- queue, regardless of how much or how little you use it. Reserved pricing
-- requires a 12-month commitment. When you use the API to create a queue,
-- the default is on-demand.
--
-- 'name', 'createQueue_name' - The name of the queue that you are creating.
newCreateQueue ::
  -- | 'name'
  Prelude.Text ->
  CreateQueue
newCreateQueue pName_ =
  CreateQueue'
    { status = Prelude.Nothing,
      tags = Prelude.Nothing,
      reservationPlanSettings = Prelude.Nothing,
      description = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      name = pName_
    }

-- | Initial state of the queue. If you create a paused queue, then jobs in
-- that queue won\'t begin.
createQueue_status :: Lens.Lens' CreateQueue (Prelude.Maybe QueueStatus)
createQueue_status = Lens.lens (\CreateQueue' {status} -> status) (\s@CreateQueue' {} a -> s {status = a} :: CreateQueue)

-- | The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
createQueue_tags :: Lens.Lens' CreateQueue (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createQueue_tags = Lens.lens (\CreateQueue' {tags} -> tags) (\s@CreateQueue' {} a -> s {tags = a} :: CreateQueue) Prelude.. Lens.mapping Lens._Coerce

-- | Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
createQueue_reservationPlanSettings :: Lens.Lens' CreateQueue (Prelude.Maybe ReservationPlanSettings)
createQueue_reservationPlanSettings = Lens.lens (\CreateQueue' {reservationPlanSettings} -> reservationPlanSettings) (\s@CreateQueue' {} a -> s {reservationPlanSettings = a} :: CreateQueue)

-- | Optional. A description of the queue that you are creating.
createQueue_description :: Lens.Lens' CreateQueue (Prelude.Maybe Prelude.Text)
createQueue_description = Lens.lens (\CreateQueue' {description} -> description) (\s@CreateQueue' {} a -> s {description = a} :: CreateQueue)

-- | Specifies whether the pricing plan for the queue is on-demand or
-- reserved. For on-demand, you pay per minute, billed in increments of .01
-- minute. For reserved, you pay for the transcoding capacity of the entire
-- queue, regardless of how much or how little you use it. Reserved pricing
-- requires a 12-month commitment. When you use the API to create a queue,
-- the default is on-demand.
createQueue_pricingPlan :: Lens.Lens' CreateQueue (Prelude.Maybe PricingPlan)
createQueue_pricingPlan = Lens.lens (\CreateQueue' {pricingPlan} -> pricingPlan) (\s@CreateQueue' {} a -> s {pricingPlan = a} :: CreateQueue)

-- | The name of the queue that you are creating.
createQueue_name :: Lens.Lens' CreateQueue Prelude.Text
createQueue_name = Lens.lens (\CreateQueue' {name} -> name) (\s@CreateQueue' {} a -> s {name = a} :: CreateQueue)

instance Core.AWSRequest CreateQueue where
  type AWSResponse CreateQueue = CreateQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQueueResponse'
            Prelude.<$> (x Core..?> "queue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateQueue

instance Prelude.NFData CreateQueue

instance Core.ToHeaders CreateQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateQueue where
  toJSON CreateQueue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("tags" Core..=) Prelude.<$> tags,
            ("reservationPlanSettings" Core..=)
              Prelude.<$> reservationPlanSettings,
            ("description" Core..=) Prelude.<$> description,
            ("pricingPlan" Core..=) Prelude.<$> pricingPlan,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateQueue where
  toPath = Prelude.const "/2017-08-29/queues"

instance Core.ToQuery CreateQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateQueueResponse' smart constructor.
data CreateQueueResponse = CreateQueueResponse'
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
-- Create a value of 'CreateQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queue', 'createQueueResponse_queue' - You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
--
-- 'httpStatus', 'createQueueResponse_httpStatus' - The response's http status code.
newCreateQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateQueueResponse
newCreateQueueResponse pHttpStatus_ =
  CreateQueueResponse'
    { queue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | You can use queues to manage the resources that are available to your
-- AWS account for running multiple transcoding jobs at the same time. If
-- you don\'t specify a queue, the service sends all jobs through the
-- default queue. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-queues.html.
createQueueResponse_queue :: Lens.Lens' CreateQueueResponse (Prelude.Maybe Queue)
createQueueResponse_queue = Lens.lens (\CreateQueueResponse' {queue} -> queue) (\s@CreateQueueResponse' {} a -> s {queue = a} :: CreateQueueResponse)

-- | The response's http status code.
createQueueResponse_httpStatus :: Lens.Lens' CreateQueueResponse Prelude.Int
createQueueResponse_httpStatus = Lens.lens (\CreateQueueResponse' {httpStatus} -> httpStatus) (\s@CreateQueueResponse' {} a -> s {httpStatus = a} :: CreateQueueResponse)

instance Prelude.NFData CreateQueueResponse
