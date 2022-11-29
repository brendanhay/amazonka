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
-- Module      : Amazonka.Glue.UpdateCrawlerSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schedule of a crawler using a @cron@ expression.
module Amazonka.Glue.UpdateCrawlerSchedule
  ( -- * Creating a Request
    UpdateCrawlerSchedule (..),
    newUpdateCrawlerSchedule,

    -- * Request Lenses
    updateCrawlerSchedule_schedule,
    updateCrawlerSchedule_crawlerName,

    -- * Destructuring the Response
    UpdateCrawlerScheduleResponse (..),
    newUpdateCrawlerScheduleResponse,

    -- * Response Lenses
    updateCrawlerScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCrawlerSchedule' smart constructor.
data UpdateCrawlerSchedule = UpdateCrawlerSchedule'
  { -- | The updated @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the crawler whose schedule to update.
    crawlerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCrawlerSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schedule', 'updateCrawlerSchedule_schedule' - The updated @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- 'crawlerName', 'updateCrawlerSchedule_crawlerName' - The name of the crawler whose schedule to update.
newUpdateCrawlerSchedule ::
  -- | 'crawlerName'
  Prelude.Text ->
  UpdateCrawlerSchedule
newUpdateCrawlerSchedule pCrawlerName_ =
  UpdateCrawlerSchedule'
    { schedule = Prelude.Nothing,
      crawlerName = pCrawlerName_
    }

-- | The updated @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
updateCrawlerSchedule_schedule :: Lens.Lens' UpdateCrawlerSchedule (Prelude.Maybe Prelude.Text)
updateCrawlerSchedule_schedule = Lens.lens (\UpdateCrawlerSchedule' {schedule} -> schedule) (\s@UpdateCrawlerSchedule' {} a -> s {schedule = a} :: UpdateCrawlerSchedule)

-- | The name of the crawler whose schedule to update.
updateCrawlerSchedule_crawlerName :: Lens.Lens' UpdateCrawlerSchedule Prelude.Text
updateCrawlerSchedule_crawlerName = Lens.lens (\UpdateCrawlerSchedule' {crawlerName} -> crawlerName) (\s@UpdateCrawlerSchedule' {} a -> s {crawlerName = a} :: UpdateCrawlerSchedule)

instance Core.AWSRequest UpdateCrawlerSchedule where
  type
    AWSResponse UpdateCrawlerSchedule =
      UpdateCrawlerScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCrawlerScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCrawlerSchedule where
  hashWithSalt _salt UpdateCrawlerSchedule' {..} =
    _salt `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` crawlerName

instance Prelude.NFData UpdateCrawlerSchedule where
  rnf UpdateCrawlerSchedule' {..} =
    Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf crawlerName

instance Core.ToHeaders UpdateCrawlerSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.UpdateCrawlerSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCrawlerSchedule where
  toJSON UpdateCrawlerSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Schedule" Core..=) Prelude.<$> schedule,
            Prelude.Just ("CrawlerName" Core..= crawlerName)
          ]
      )

instance Core.ToPath UpdateCrawlerSchedule where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateCrawlerSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCrawlerScheduleResponse' smart constructor.
data UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCrawlerScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCrawlerScheduleResponse_httpStatus' - The response's http status code.
newUpdateCrawlerScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCrawlerScheduleResponse
newUpdateCrawlerScheduleResponse pHttpStatus_ =
  UpdateCrawlerScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCrawlerScheduleResponse_httpStatus :: Lens.Lens' UpdateCrawlerScheduleResponse Prelude.Int
updateCrawlerScheduleResponse_httpStatus = Lens.lens (\UpdateCrawlerScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateCrawlerScheduleResponse' {} a -> s {httpStatus = a} :: UpdateCrawlerScheduleResponse)

instance Prelude.NFData UpdateCrawlerScheduleResponse where
  rnf UpdateCrawlerScheduleResponse' {..} =
    Prelude.rnf httpStatus
