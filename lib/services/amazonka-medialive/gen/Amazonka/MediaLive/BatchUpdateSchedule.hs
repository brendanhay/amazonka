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
-- Module      : Amazonka.MediaLive.BatchUpdateSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a channel schedule
module Amazonka.MediaLive.BatchUpdateSchedule
  ( -- * Creating a Request
    BatchUpdateSchedule (..),
    newBatchUpdateSchedule,

    -- * Request Lenses
    batchUpdateSchedule_creates,
    batchUpdateSchedule_deletes,
    batchUpdateSchedule_channelId,

    -- * Destructuring the Response
    BatchUpdateScheduleResponse (..),
    newBatchUpdateScheduleResponse,

    -- * Response Lenses
    batchUpdateScheduleResponse_creates,
    batchUpdateScheduleResponse_deletes,
    batchUpdateScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | List of actions to create and list of actions to delete.
--
-- /See:/ 'newBatchUpdateSchedule' smart constructor.
data BatchUpdateSchedule = BatchUpdateSchedule'
  { -- | Schedule actions to create in the schedule.
    creates :: Prelude.Maybe BatchScheduleActionCreateRequest,
    -- | Schedule actions to delete from the schedule.
    deletes :: Prelude.Maybe BatchScheduleActionDeleteRequest,
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
-- 'creates', 'batchUpdateSchedule_creates' - Schedule actions to create in the schedule.
--
-- 'deletes', 'batchUpdateSchedule_deletes' - Schedule actions to delete from the schedule.
--
-- 'channelId', 'batchUpdateSchedule_channelId' - Id of the channel whose schedule is being updated.
newBatchUpdateSchedule ::
  -- | 'channelId'
  Prelude.Text ->
  BatchUpdateSchedule
newBatchUpdateSchedule pChannelId_ =
  BatchUpdateSchedule'
    { creates = Prelude.Nothing,
      deletes = Prelude.Nothing,
      channelId = pChannelId_
    }

-- | Schedule actions to create in the schedule.
batchUpdateSchedule_creates :: Lens.Lens' BatchUpdateSchedule (Prelude.Maybe BatchScheduleActionCreateRequest)
batchUpdateSchedule_creates = Lens.lens (\BatchUpdateSchedule' {creates} -> creates) (\s@BatchUpdateSchedule' {} a -> s {creates = a} :: BatchUpdateSchedule)

-- | Schedule actions to delete from the schedule.
batchUpdateSchedule_deletes :: Lens.Lens' BatchUpdateSchedule (Prelude.Maybe BatchScheduleActionDeleteRequest)
batchUpdateSchedule_deletes = Lens.lens (\BatchUpdateSchedule' {deletes} -> deletes) (\s@BatchUpdateSchedule' {} a -> s {deletes = a} :: BatchUpdateSchedule)

-- | Id of the channel whose schedule is being updated.
batchUpdateSchedule_channelId :: Lens.Lens' BatchUpdateSchedule Prelude.Text
batchUpdateSchedule_channelId = Lens.lens (\BatchUpdateSchedule' {channelId} -> channelId) (\s@BatchUpdateSchedule' {} a -> s {channelId = a} :: BatchUpdateSchedule)

instance Core.AWSRequest BatchUpdateSchedule where
  type
    AWSResponse BatchUpdateSchedule =
      BatchUpdateScheduleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateScheduleResponse'
            Prelude.<$> (x Data..?> "creates")
            Prelude.<*> (x Data..?> "deletes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateSchedule where
  hashWithSalt _salt BatchUpdateSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` creates
      `Prelude.hashWithSalt` deletes
      `Prelude.hashWithSalt` channelId

instance Prelude.NFData BatchUpdateSchedule where
  rnf BatchUpdateSchedule' {..} =
    Prelude.rnf creates `Prelude.seq`
      Prelude.rnf deletes `Prelude.seq`
        Prelude.rnf channelId

instance Data.ToHeaders BatchUpdateSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateSchedule where
  toJSON BatchUpdateSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("creates" Data..=) Prelude.<$> creates,
            ("deletes" Data..=) Prelude.<$> deletes
          ]
      )

instance Data.ToPath BatchUpdateSchedule where
  toPath BatchUpdateSchedule' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId, "/schedule"]

instance Data.ToQuery BatchUpdateSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for BatchUpdateScheduleResponse
--
-- /See:/ 'newBatchUpdateScheduleResponse' smart constructor.
data BatchUpdateScheduleResponse = BatchUpdateScheduleResponse'
  { -- | Schedule actions created in the schedule.
    creates :: Prelude.Maybe BatchScheduleActionCreateResult,
    -- | Schedule actions deleted from the schedule.
    deletes :: Prelude.Maybe BatchScheduleActionDeleteResult,
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
-- 'creates', 'batchUpdateScheduleResponse_creates' - Schedule actions created in the schedule.
--
-- 'deletes', 'batchUpdateScheduleResponse_deletes' - Schedule actions deleted from the schedule.
--
-- 'httpStatus', 'batchUpdateScheduleResponse_httpStatus' - The response's http status code.
newBatchUpdateScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateScheduleResponse
newBatchUpdateScheduleResponse pHttpStatus_ =
  BatchUpdateScheduleResponse'
    { creates =
        Prelude.Nothing,
      deletes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Schedule actions created in the schedule.
batchUpdateScheduleResponse_creates :: Lens.Lens' BatchUpdateScheduleResponse (Prelude.Maybe BatchScheduleActionCreateResult)
batchUpdateScheduleResponse_creates = Lens.lens (\BatchUpdateScheduleResponse' {creates} -> creates) (\s@BatchUpdateScheduleResponse' {} a -> s {creates = a} :: BatchUpdateScheduleResponse)

-- | Schedule actions deleted from the schedule.
batchUpdateScheduleResponse_deletes :: Lens.Lens' BatchUpdateScheduleResponse (Prelude.Maybe BatchScheduleActionDeleteResult)
batchUpdateScheduleResponse_deletes = Lens.lens (\BatchUpdateScheduleResponse' {deletes} -> deletes) (\s@BatchUpdateScheduleResponse' {} a -> s {deletes = a} :: BatchUpdateScheduleResponse)

-- | The response's http status code.
batchUpdateScheduleResponse_httpStatus :: Lens.Lens' BatchUpdateScheduleResponse Prelude.Int
batchUpdateScheduleResponse_httpStatus = Lens.lens (\BatchUpdateScheduleResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateScheduleResponse' {} a -> s {httpStatus = a} :: BatchUpdateScheduleResponse)

instance Prelude.NFData BatchUpdateScheduleResponse where
  rnf BatchUpdateScheduleResponse' {..} =
    Prelude.rnf creates `Prelude.seq`
      Prelude.rnf deletes `Prelude.seq`
        Prelude.rnf httpStatus
