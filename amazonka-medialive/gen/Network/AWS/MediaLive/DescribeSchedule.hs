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
-- Module      : Network.AWS.MediaLive.DescribeSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a channel schedule
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.DescribeSchedule
  ( -- * Creating a Request
    DescribeSchedule (..),
    newDescribeSchedule,

    -- * Request Lenses
    describeSchedule_nextToken,
    describeSchedule_maxResults,
    describeSchedule_channelId,

    -- * Destructuring the Response
    DescribeScheduleResponse (..),
    newDescribeScheduleResponse,

    -- * Response Lenses
    describeScheduleResponse_nextToken,
    describeScheduleResponse_scheduleActions,
    describeScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'newDescribeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    -- | Id of the channel whose schedule is being updated.
    channelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSchedule_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeSchedule_maxResults' - Undocumented member.
--
-- 'channelId', 'describeSchedule_channelId' - Id of the channel whose schedule is being updated.
newDescribeSchedule ::
  -- | 'channelId'
  Core.Text ->
  DescribeSchedule
newDescribeSchedule pChannelId_ =
  DescribeSchedule'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      channelId = pChannelId_
    }

-- | Undocumented member.
describeSchedule_nextToken :: Lens.Lens' DescribeSchedule (Core.Maybe Core.Text)
describeSchedule_nextToken = Lens.lens (\DescribeSchedule' {nextToken} -> nextToken) (\s@DescribeSchedule' {} a -> s {nextToken = a} :: DescribeSchedule)

-- | Undocumented member.
describeSchedule_maxResults :: Lens.Lens' DescribeSchedule (Core.Maybe Core.Natural)
describeSchedule_maxResults = Lens.lens (\DescribeSchedule' {maxResults} -> maxResults) (\s@DescribeSchedule' {} a -> s {maxResults = a} :: DescribeSchedule)

-- | Id of the channel whose schedule is being updated.
describeSchedule_channelId :: Lens.Lens' DescribeSchedule Core.Text
describeSchedule_channelId = Lens.lens (\DescribeSchedule' {channelId} -> channelId) (\s@DescribeSchedule' {} a -> s {channelId = a} :: DescribeSchedule)

instance Core.AWSPager DescribeSchedule where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduleResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduleResponse_scheduleActions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSchedule_nextToken
          Lens..~ rs
          Lens.^? describeScheduleResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeSchedule where
  type
    AWSResponse DescribeSchedule =
      DescribeScheduleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "scheduleActions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSchedule

instance Core.NFData DescribeSchedule

instance Core.ToHeaders DescribeSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeSchedule where
  toPath DescribeSchedule' {..} =
    Core.mconcat
      ["/prod/channels/", Core.toBS channelId, "/schedule"]

instance Core.ToQuery DescribeSchedule where
  toQuery DescribeSchedule' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Placeholder documentation for DescribeScheduleResponse
--
-- /See:/ 'newDescribeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'
  { -- | The next token; for use in pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of actions in the schedule.
    scheduleActions :: Core.Maybe [ScheduleAction],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeScheduleResponse_nextToken' - The next token; for use in pagination.
--
-- 'scheduleActions', 'describeScheduleResponse_scheduleActions' - The list of actions in the schedule.
--
-- 'httpStatus', 'describeScheduleResponse_httpStatus' - The response's http status code.
newDescribeScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeScheduleResponse
newDescribeScheduleResponse pHttpStatus_ =
  DescribeScheduleResponse'
    { nextToken = Core.Nothing,
      scheduleActions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token; for use in pagination.
describeScheduleResponse_nextToken :: Lens.Lens' DescribeScheduleResponse (Core.Maybe Core.Text)
describeScheduleResponse_nextToken = Lens.lens (\DescribeScheduleResponse' {nextToken} -> nextToken) (\s@DescribeScheduleResponse' {} a -> s {nextToken = a} :: DescribeScheduleResponse)

-- | The list of actions in the schedule.
describeScheduleResponse_scheduleActions :: Lens.Lens' DescribeScheduleResponse (Core.Maybe [ScheduleAction])
describeScheduleResponse_scheduleActions = Lens.lens (\DescribeScheduleResponse' {scheduleActions} -> scheduleActions) (\s@DescribeScheduleResponse' {} a -> s {scheduleActions = a} :: DescribeScheduleResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeScheduleResponse_httpStatus :: Lens.Lens' DescribeScheduleResponse Core.Int
describeScheduleResponse_httpStatus = Lens.lens (\DescribeScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduleResponse' {} a -> s {httpStatus = a} :: DescribeScheduleResponse)

instance Core.NFData DescribeScheduleResponse
