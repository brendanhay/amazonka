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
-- Module      : Network.AWS.MediaLive.BatchUpdateSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a channel schedule
module Network.AWS.MediaLive.BatchUpdateSchedule
  ( -- * Creating a Request
    BatchUpdateSchedule (..),
    newBatchUpdateSchedule,

    -- * Request Lenses
    batchUpdateSchedule_deletes,
    batchUpdateSchedule_creates,
    batchUpdateSchedule_channelId,

    -- * Destructuring the Response
    BatchUpdateScheduleResponse (..),
    newBatchUpdateScheduleResponse,

    -- * Response Lenses
    batchUpdateScheduleResponse_deletes,
    batchUpdateScheduleResponse_creates,
    batchUpdateScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | List of actions to create and list of actions to delete.
--
-- /See:/ 'newBatchUpdateSchedule' smart constructor.
data BatchUpdateSchedule = BatchUpdateSchedule'
  { -- | Schedule actions to delete from the schedule.
    deletes :: Core.Maybe BatchScheduleActionDeleteRequest,
    -- | Schedule actions to create in the schedule.
    creates :: Core.Maybe BatchScheduleActionCreateRequest,
    -- | Id of the channel whose schedule is being updated.
    channelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchUpdateSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletes', 'batchUpdateSchedule_deletes' - Schedule actions to delete from the schedule.
--
-- 'creates', 'batchUpdateSchedule_creates' - Schedule actions to create in the schedule.
--
-- 'channelId', 'batchUpdateSchedule_channelId' - Id of the channel whose schedule is being updated.
newBatchUpdateSchedule ::
  -- | 'channelId'
  Core.Text ->
  BatchUpdateSchedule
newBatchUpdateSchedule pChannelId_ =
  BatchUpdateSchedule'
    { deletes = Core.Nothing,
      creates = Core.Nothing,
      channelId = pChannelId_
    }

-- | Schedule actions to delete from the schedule.
batchUpdateSchedule_deletes :: Lens.Lens' BatchUpdateSchedule (Core.Maybe BatchScheduleActionDeleteRequest)
batchUpdateSchedule_deletes = Lens.lens (\BatchUpdateSchedule' {deletes} -> deletes) (\s@BatchUpdateSchedule' {} a -> s {deletes = a} :: BatchUpdateSchedule)

-- | Schedule actions to create in the schedule.
batchUpdateSchedule_creates :: Lens.Lens' BatchUpdateSchedule (Core.Maybe BatchScheduleActionCreateRequest)
batchUpdateSchedule_creates = Lens.lens (\BatchUpdateSchedule' {creates} -> creates) (\s@BatchUpdateSchedule' {} a -> s {creates = a} :: BatchUpdateSchedule)

-- | Id of the channel whose schedule is being updated.
batchUpdateSchedule_channelId :: Lens.Lens' BatchUpdateSchedule Core.Text
batchUpdateSchedule_channelId = Lens.lens (\BatchUpdateSchedule' {channelId} -> channelId) (\s@BatchUpdateSchedule' {} a -> s {channelId = a} :: BatchUpdateSchedule)

instance Core.AWSRequest BatchUpdateSchedule where
  type
    AWSResponse BatchUpdateSchedule =
      BatchUpdateScheduleResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateScheduleResponse'
            Core.<$> (x Core..?> "deletes")
            Core.<*> (x Core..?> "creates")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchUpdateSchedule

instance Core.NFData BatchUpdateSchedule

instance Core.ToHeaders BatchUpdateSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchUpdateSchedule where
  toJSON BatchUpdateSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deletes" Core..=) Core.<$> deletes,
            ("creates" Core..=) Core.<$> creates
          ]
      )

instance Core.ToPath BatchUpdateSchedule where
  toPath BatchUpdateSchedule' {..} =
    Core.mconcat
      ["/prod/channels/", Core.toBS channelId, "/schedule"]

instance Core.ToQuery BatchUpdateSchedule where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for BatchUpdateScheduleResponse
--
-- /See:/ 'newBatchUpdateScheduleResponse' smart constructor.
data BatchUpdateScheduleResponse = BatchUpdateScheduleResponse'
  { -- | Schedule actions deleted from the schedule.
    deletes :: Core.Maybe BatchScheduleActionDeleteResult,
    -- | Schedule actions created in the schedule.
    creates :: Core.Maybe BatchScheduleActionCreateResult,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchUpdateScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletes', 'batchUpdateScheduleResponse_deletes' - Schedule actions deleted from the schedule.
--
-- 'creates', 'batchUpdateScheduleResponse_creates' - Schedule actions created in the schedule.
--
-- 'httpStatus', 'batchUpdateScheduleResponse_httpStatus' - The response's http status code.
newBatchUpdateScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchUpdateScheduleResponse
newBatchUpdateScheduleResponse pHttpStatus_ =
  BatchUpdateScheduleResponse'
    { deletes =
        Core.Nothing,
      creates = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Schedule actions deleted from the schedule.
batchUpdateScheduleResponse_deletes :: Lens.Lens' BatchUpdateScheduleResponse (Core.Maybe BatchScheduleActionDeleteResult)
batchUpdateScheduleResponse_deletes = Lens.lens (\BatchUpdateScheduleResponse' {deletes} -> deletes) (\s@BatchUpdateScheduleResponse' {} a -> s {deletes = a} :: BatchUpdateScheduleResponse)

-- | Schedule actions created in the schedule.
batchUpdateScheduleResponse_creates :: Lens.Lens' BatchUpdateScheduleResponse (Core.Maybe BatchScheduleActionCreateResult)
batchUpdateScheduleResponse_creates = Lens.lens (\BatchUpdateScheduleResponse' {creates} -> creates) (\s@BatchUpdateScheduleResponse' {} a -> s {creates = a} :: BatchUpdateScheduleResponse)

-- | The response's http status code.
batchUpdateScheduleResponse_httpStatus :: Lens.Lens' BatchUpdateScheduleResponse Core.Int
batchUpdateScheduleResponse_httpStatus = Lens.lens (\BatchUpdateScheduleResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateScheduleResponse' {} a -> s {httpStatus = a} :: BatchUpdateScheduleResponse)

instance Core.NFData BatchUpdateScheduleResponse
