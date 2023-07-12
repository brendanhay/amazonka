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
-- Module      : Amazonka.MediaLive.DescribeSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a channel schedule
--
-- This operation returns paginated results.
module Amazonka.MediaLive.DescribeSchedule
  ( -- * Creating a Request
    DescribeSchedule (..),
    newDescribeSchedule,

    -- * Request Lenses
    describeSchedule_maxResults,
    describeSchedule_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'newDescribeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Id of the channel whose schedule is being updated.
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeSchedule_maxResults' - Undocumented member.
--
-- 'nextToken', 'describeSchedule_nextToken' - Undocumented member.
--
-- 'channelId', 'describeSchedule_channelId' - Id of the channel whose schedule is being updated.
newDescribeSchedule ::
  -- | 'channelId'
  Prelude.Text ->
  DescribeSchedule
newDescribeSchedule pChannelId_ =
  DescribeSchedule'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      channelId = pChannelId_
    }

-- | Undocumented member.
describeSchedule_maxResults :: Lens.Lens' DescribeSchedule (Prelude.Maybe Prelude.Natural)
describeSchedule_maxResults = Lens.lens (\DescribeSchedule' {maxResults} -> maxResults) (\s@DescribeSchedule' {} a -> s {maxResults = a} :: DescribeSchedule)

-- | Undocumented member.
describeSchedule_nextToken :: Lens.Lens' DescribeSchedule (Prelude.Maybe Prelude.Text)
describeSchedule_nextToken = Lens.lens (\DescribeSchedule' {nextToken} -> nextToken) (\s@DescribeSchedule' {} a -> s {nextToken = a} :: DescribeSchedule)

-- | Id of the channel whose schedule is being updated.
describeSchedule_channelId :: Lens.Lens' DescribeSchedule Prelude.Text
describeSchedule_channelId = Lens.lens (\DescribeSchedule' {channelId} -> channelId) (\s@DescribeSchedule' {} a -> s {channelId = a} :: DescribeSchedule)

instance Core.AWSPager DescribeSchedule where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeScheduleResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeScheduleResponse_scheduleActions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSchedule_nextToken
          Lens..~ rs
          Lens.^? describeScheduleResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSchedule where
  type
    AWSResponse DescribeSchedule =
      DescribeScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "scheduleActions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSchedule where
  hashWithSalt _salt DescribeSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` channelId

instance Prelude.NFData DescribeSchedule where
  rnf DescribeSchedule' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelId

instance Data.ToHeaders DescribeSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeSchedule where
  toPath DescribeSchedule' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId, "/schedule"]

instance Data.ToQuery DescribeSchedule where
  toQuery DescribeSchedule' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
describeScheduleResponse_scheduleActions = Lens.lens (\DescribeScheduleResponse' {scheduleActions} -> scheduleActions) (\s@DescribeScheduleResponse' {} a -> s {scheduleActions = a} :: DescribeScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeScheduleResponse_httpStatus :: Lens.Lens' DescribeScheduleResponse Prelude.Int
describeScheduleResponse_httpStatus = Lens.lens (\DescribeScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduleResponse' {} a -> s {httpStatus = a} :: DescribeScheduleResponse)

instance Prelude.NFData DescribeScheduleResponse where
  rnf DescribeScheduleResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scheduleActions
      `Prelude.seq` Prelude.rnf httpStatus
