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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | List of actions to create and list of actions to delete.
--
-- /See:/ 'newBatchUpdateSchedule' smart constructor.
data BatchUpdateSchedule = BatchUpdateSchedule'
  { -- | Schedule actions to delete from the schedule.
    deletes :: Prelude.Maybe BatchScheduleActionDeleteRequest,
    -- | Schedule actions to create in the schedule.
    creates :: Prelude.Maybe BatchScheduleActionCreateRequest,
    -- | Id of the channel whose schedule is being updated.
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  BatchUpdateSchedule
newBatchUpdateSchedule pChannelId_ =
  BatchUpdateSchedule'
    { deletes = Prelude.Nothing,
      creates = Prelude.Nothing,
      channelId = pChannelId_
    }

-- | Schedule actions to delete from the schedule.
batchUpdateSchedule_deletes :: Lens.Lens' BatchUpdateSchedule (Prelude.Maybe BatchScheduleActionDeleteRequest)
batchUpdateSchedule_deletes = Lens.lens (\BatchUpdateSchedule' {deletes} -> deletes) (\s@BatchUpdateSchedule' {} a -> s {deletes = a} :: BatchUpdateSchedule)

-- | Schedule actions to create in the schedule.
batchUpdateSchedule_creates :: Lens.Lens' BatchUpdateSchedule (Prelude.Maybe BatchScheduleActionCreateRequest)
batchUpdateSchedule_creates = Lens.lens (\BatchUpdateSchedule' {creates} -> creates) (\s@BatchUpdateSchedule' {} a -> s {creates = a} :: BatchUpdateSchedule)

-- | Id of the channel whose schedule is being updated.
batchUpdateSchedule_channelId :: Lens.Lens' BatchUpdateSchedule Prelude.Text
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
            Prelude.<$> (x Core..?> "deletes")
            Prelude.<*> (x Core..?> "creates")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateSchedule

instance Prelude.NFData BatchUpdateSchedule

instance Core.ToHeaders BatchUpdateSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchUpdateSchedule where
  toJSON BatchUpdateSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("deletes" Core..=) Prelude.<$> deletes,
            ("creates" Core..=) Prelude.<$> creates
          ]
      )

instance Core.ToPath BatchUpdateSchedule where
  toPath BatchUpdateSchedule' {..} =
    Prelude.mconcat
      ["/prod/channels/", Core.toBS channelId, "/schedule"]

instance Core.ToQuery BatchUpdateSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for BatchUpdateScheduleResponse
--
-- /See:/ 'newBatchUpdateScheduleResponse' smart constructor.
data BatchUpdateScheduleResponse = BatchUpdateScheduleResponse'
  { -- | Schedule actions deleted from the schedule.
    deletes :: Prelude.Maybe BatchScheduleActionDeleteResult,
    -- | Schedule actions created in the schedule.
    creates :: Prelude.Maybe BatchScheduleActionCreateResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchUpdateScheduleResponse
newBatchUpdateScheduleResponse pHttpStatus_ =
  BatchUpdateScheduleResponse'
    { deletes =
        Prelude.Nothing,
      creates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Schedule actions deleted from the schedule.
batchUpdateScheduleResponse_deletes :: Lens.Lens' BatchUpdateScheduleResponse (Prelude.Maybe BatchScheduleActionDeleteResult)
batchUpdateScheduleResponse_deletes = Lens.lens (\BatchUpdateScheduleResponse' {deletes} -> deletes) (\s@BatchUpdateScheduleResponse' {} a -> s {deletes = a} :: BatchUpdateScheduleResponse)

-- | Schedule actions created in the schedule.
batchUpdateScheduleResponse_creates :: Lens.Lens' BatchUpdateScheduleResponse (Prelude.Maybe BatchScheduleActionCreateResult)
batchUpdateScheduleResponse_creates = Lens.lens (\BatchUpdateScheduleResponse' {creates} -> creates) (\s@BatchUpdateScheduleResponse' {} a -> s {creates = a} :: BatchUpdateScheduleResponse)

-- | The response's http status code.
batchUpdateScheduleResponse_httpStatus :: Lens.Lens' BatchUpdateScheduleResponse Prelude.Int
batchUpdateScheduleResponse_httpStatus = Lens.lens (\BatchUpdateScheduleResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateScheduleResponse' {} a -> s {httpStatus = a} :: BatchUpdateScheduleResponse)

instance Prelude.NFData BatchUpdateScheduleResponse
