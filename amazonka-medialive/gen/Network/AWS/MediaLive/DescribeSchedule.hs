{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'newDescribeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Id of the channel whose schedule is being updated.
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeSchedule
newDescribeSchedule pChannelId_ =
  DescribeSchedule'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      channelId = pChannelId_
    }

-- | Undocumented member.
describeSchedule_nextToken :: Lens.Lens' DescribeSchedule (Prelude.Maybe Prelude.Text)
describeSchedule_nextToken = Lens.lens (\DescribeSchedule' {nextToken} -> nextToken) (\s@DescribeSchedule' {} a -> s {nextToken = a} :: DescribeSchedule)

-- | Undocumented member.
describeSchedule_maxResults :: Lens.Lens' DescribeSchedule (Prelude.Maybe Prelude.Natural)
describeSchedule_maxResults = Lens.lens (\DescribeSchedule' {maxResults} -> maxResults) (\s@DescribeSchedule' {} a -> s {maxResults = a} :: DescribeSchedule)

-- | Id of the channel whose schedule is being updated.
describeSchedule_channelId :: Lens.Lens' DescribeSchedule Prelude.Text
describeSchedule_channelId = Lens.lens (\DescribeSchedule' {channelId} -> channelId) (\s@DescribeSchedule' {} a -> s {channelId = a} :: DescribeSchedule)

instance Pager.AWSPager DescribeSchedule where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeScheduleResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeScheduleResponse_scheduleActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeSchedule_nextToken
          Lens..~ rs
          Lens.^? describeScheduleResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeSchedule where
  type Rs DescribeSchedule = DescribeScheduleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "scheduleActions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSchedule

instance Prelude.NFData DescribeSchedule

instance Prelude.ToHeaders DescribeSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DescribeSchedule where
  toPath DescribeSchedule' {..} =
    Prelude.mconcat
      [ "/prod/channels/",
        Prelude.toBS channelId,
        "/schedule"
      ]

instance Prelude.ToQuery DescribeSchedule where
  toQuery DescribeSchedule' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | Placeholder documentation for DescribeScheduleResponse
--
-- /See:/ 'newDescribeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'
  { -- | The next token; for use in pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of actions in the schedule.
    scheduleActions :: Prelude.Maybe [ScheduleAction],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeScheduleResponse
newDescribeScheduleResponse pHttpStatus_ =
  DescribeScheduleResponse'
    { nextToken =
        Prelude.Nothing,
      scheduleActions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token; for use in pagination.
describeScheduleResponse_nextToken :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe Prelude.Text)
describeScheduleResponse_nextToken = Lens.lens (\DescribeScheduleResponse' {nextToken} -> nextToken) (\s@DescribeScheduleResponse' {} a -> s {nextToken = a} :: DescribeScheduleResponse)

-- | The list of actions in the schedule.
describeScheduleResponse_scheduleActions :: Lens.Lens' DescribeScheduleResponse (Prelude.Maybe [ScheduleAction])
describeScheduleResponse_scheduleActions = Lens.lens (\DescribeScheduleResponse' {scheduleActions} -> scheduleActions) (\s@DescribeScheduleResponse' {} a -> s {scheduleActions = a} :: DescribeScheduleResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeScheduleResponse_httpStatus :: Lens.Lens' DescribeScheduleResponse Prelude.Int
describeScheduleResponse_httpStatus = Lens.lens (\DescribeScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduleResponse' {} a -> s {httpStatus = a} :: DescribeScheduleResponse)

instance Prelude.NFData DescribeScheduleResponse
