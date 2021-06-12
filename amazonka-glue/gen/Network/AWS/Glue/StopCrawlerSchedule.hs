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
-- Module      : Network.AWS.Glue.StopCrawlerSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the schedule state of the specified crawler to @NOT_SCHEDULED@, but
-- does not stop the crawler if it is already running.
module Network.AWS.Glue.StopCrawlerSchedule
  ( -- * Creating a Request
    StopCrawlerSchedule (..),
    newStopCrawlerSchedule,

    -- * Request Lenses
    stopCrawlerSchedule_crawlerName,

    -- * Destructuring the Response
    StopCrawlerScheduleResponse (..),
    newStopCrawlerScheduleResponse,

    -- * Response Lenses
    stopCrawlerScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopCrawlerSchedule' smart constructor.
data StopCrawlerSchedule = StopCrawlerSchedule'
  { -- | Name of the crawler whose schedule state to set.
    crawlerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopCrawlerSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerName', 'stopCrawlerSchedule_crawlerName' - Name of the crawler whose schedule state to set.
newStopCrawlerSchedule ::
  -- | 'crawlerName'
  Core.Text ->
  StopCrawlerSchedule
newStopCrawlerSchedule pCrawlerName_ =
  StopCrawlerSchedule' {crawlerName = pCrawlerName_}

-- | Name of the crawler whose schedule state to set.
stopCrawlerSchedule_crawlerName :: Lens.Lens' StopCrawlerSchedule Core.Text
stopCrawlerSchedule_crawlerName = Lens.lens (\StopCrawlerSchedule' {crawlerName} -> crawlerName) (\s@StopCrawlerSchedule' {} a -> s {crawlerName = a} :: StopCrawlerSchedule)

instance Core.AWSRequest StopCrawlerSchedule where
  type
    AWSResponse StopCrawlerSchedule =
      StopCrawlerScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCrawlerScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopCrawlerSchedule

instance Core.NFData StopCrawlerSchedule

instance Core.ToHeaders StopCrawlerSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.StopCrawlerSchedule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopCrawlerSchedule where
  toJSON StopCrawlerSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CrawlerName" Core..= crawlerName)]
      )

instance Core.ToPath StopCrawlerSchedule where
  toPath = Core.const "/"

instance Core.ToQuery StopCrawlerSchedule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopCrawlerScheduleResponse' smart constructor.
data StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopCrawlerScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopCrawlerScheduleResponse_httpStatus' - The response's http status code.
newStopCrawlerScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopCrawlerScheduleResponse
newStopCrawlerScheduleResponse pHttpStatus_ =
  StopCrawlerScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopCrawlerScheduleResponse_httpStatus :: Lens.Lens' StopCrawlerScheduleResponse Core.Int
stopCrawlerScheduleResponse_httpStatus = Lens.lens (\StopCrawlerScheduleResponse' {httpStatus} -> httpStatus) (\s@StopCrawlerScheduleResponse' {} a -> s {httpStatus = a} :: StopCrawlerScheduleResponse)

instance Core.NFData StopCrawlerScheduleResponse
