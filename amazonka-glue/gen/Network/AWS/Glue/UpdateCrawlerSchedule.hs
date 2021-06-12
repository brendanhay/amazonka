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
-- Module      : Network.AWS.Glue.UpdateCrawlerSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schedule of a crawler using a @cron@ expression.
module Network.AWS.Glue.UpdateCrawlerSchedule
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCrawlerSchedule' smart constructor.
data UpdateCrawlerSchedule = UpdateCrawlerSchedule'
  { -- | The updated @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    schedule :: Core.Maybe Core.Text,
    -- | The name of the crawler whose schedule to update.
    crawlerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateCrawlerSchedule
newUpdateCrawlerSchedule pCrawlerName_ =
  UpdateCrawlerSchedule'
    { schedule = Core.Nothing,
      crawlerName = pCrawlerName_
    }

-- | The updated @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
updateCrawlerSchedule_schedule :: Lens.Lens' UpdateCrawlerSchedule (Core.Maybe Core.Text)
updateCrawlerSchedule_schedule = Lens.lens (\UpdateCrawlerSchedule' {schedule} -> schedule) (\s@UpdateCrawlerSchedule' {} a -> s {schedule = a} :: UpdateCrawlerSchedule)

-- | The name of the crawler whose schedule to update.
updateCrawlerSchedule_crawlerName :: Lens.Lens' UpdateCrawlerSchedule Core.Text
updateCrawlerSchedule_crawlerName = Lens.lens (\UpdateCrawlerSchedule' {crawlerName} -> crawlerName) (\s@UpdateCrawlerSchedule' {} a -> s {crawlerName = a} :: UpdateCrawlerSchedule)

instance Core.AWSRequest UpdateCrawlerSchedule where
  type
    AWSResponse UpdateCrawlerSchedule =
      UpdateCrawlerScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCrawlerScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCrawlerSchedule

instance Core.NFData UpdateCrawlerSchedule

instance Core.ToHeaders UpdateCrawlerSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateCrawlerSchedule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCrawlerSchedule where
  toJSON UpdateCrawlerSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Schedule" Core..=) Core.<$> schedule,
            Core.Just ("CrawlerName" Core..= crawlerName)
          ]
      )

instance Core.ToPath UpdateCrawlerSchedule where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCrawlerSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCrawlerScheduleResponse' smart constructor.
data UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateCrawlerScheduleResponse
newUpdateCrawlerScheduleResponse pHttpStatus_ =
  UpdateCrawlerScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCrawlerScheduleResponse_httpStatus :: Lens.Lens' UpdateCrawlerScheduleResponse Core.Int
updateCrawlerScheduleResponse_httpStatus = Lens.lens (\UpdateCrawlerScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateCrawlerScheduleResponse' {} a -> s {httpStatus = a} :: UpdateCrawlerScheduleResponse)

instance Core.NFData UpdateCrawlerScheduleResponse
