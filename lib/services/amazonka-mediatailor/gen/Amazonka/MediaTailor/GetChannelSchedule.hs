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
-- Module      : Amazonka.MediaTailor.GetChannelSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about your channel\'s schedule.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.GetChannelSchedule
  ( -- * Creating a Request
    GetChannelSchedule (..),
    newGetChannelSchedule,

    -- * Request Lenses
    getChannelSchedule_durationMinutes,
    getChannelSchedule_maxResults,
    getChannelSchedule_nextToken,
    getChannelSchedule_channelName,

    -- * Destructuring the Response
    GetChannelScheduleResponse (..),
    newGetChannelScheduleResponse,

    -- * Response Lenses
    getChannelScheduleResponse_items,
    getChannelScheduleResponse_nextToken,
    getChannelScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelSchedule' smart constructor.
data GetChannelSchedule = GetChannelSchedule'
  { -- | The duration in minutes of the channel schedule.
    durationMinutes :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of channel schedules that you want MediaTailor to
    -- return in response to the current request. If there are more than
    -- @MaxResults@ channel schedules, use the value of @NextToken@ in the
    -- response to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | (Optional) If the playback configuration has more than @MaxResults@
    -- channel schedules, use @NextToken@ to get the second and subsequent
    -- pages of results.
    --
    -- For the first @GetChannelScheduleRequest@ request, omit this value.
    --
    -- For the second and subsequent requests, get the value of @NextToken@
    -- from the previous response and specify that value for @NextToken@ in the
    -- request.
    --
    -- If the previous response didn\'t include a @NextToken@ element, there
    -- are no more channel schedules to get.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel associated with this Channel Schedule.
    channelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationMinutes', 'getChannelSchedule_durationMinutes' - The duration in minutes of the channel schedule.
--
-- 'maxResults', 'getChannelSchedule_maxResults' - The maximum number of channel schedules that you want MediaTailor to
-- return in response to the current request. If there are more than
-- @MaxResults@ channel schedules, use the value of @NextToken@ in the
-- response to get the next page of results.
--
-- 'nextToken', 'getChannelSchedule_nextToken' - (Optional) If the playback configuration has more than @MaxResults@
-- channel schedules, use @NextToken@ to get the second and subsequent
-- pages of results.
--
-- For the first @GetChannelScheduleRequest@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- If the previous response didn\'t include a @NextToken@ element, there
-- are no more channel schedules to get.
--
-- 'channelName', 'getChannelSchedule_channelName' - The name of the channel associated with this Channel Schedule.
newGetChannelSchedule ::
  -- | 'channelName'
  Prelude.Text ->
  GetChannelSchedule
newGetChannelSchedule pChannelName_ =
  GetChannelSchedule'
    { durationMinutes =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | The duration in minutes of the channel schedule.
getChannelSchedule_durationMinutes :: Lens.Lens' GetChannelSchedule (Prelude.Maybe Prelude.Text)
getChannelSchedule_durationMinutes = Lens.lens (\GetChannelSchedule' {durationMinutes} -> durationMinutes) (\s@GetChannelSchedule' {} a -> s {durationMinutes = a} :: GetChannelSchedule)

-- | The maximum number of channel schedules that you want MediaTailor to
-- return in response to the current request. If there are more than
-- @MaxResults@ channel schedules, use the value of @NextToken@ in the
-- response to get the next page of results.
getChannelSchedule_maxResults :: Lens.Lens' GetChannelSchedule (Prelude.Maybe Prelude.Natural)
getChannelSchedule_maxResults = Lens.lens (\GetChannelSchedule' {maxResults} -> maxResults) (\s@GetChannelSchedule' {} a -> s {maxResults = a} :: GetChannelSchedule)

-- | (Optional) If the playback configuration has more than @MaxResults@
-- channel schedules, use @NextToken@ to get the second and subsequent
-- pages of results.
--
-- For the first @GetChannelScheduleRequest@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- If the previous response didn\'t include a @NextToken@ element, there
-- are no more channel schedules to get.
getChannelSchedule_nextToken :: Lens.Lens' GetChannelSchedule (Prelude.Maybe Prelude.Text)
getChannelSchedule_nextToken = Lens.lens (\GetChannelSchedule' {nextToken} -> nextToken) (\s@GetChannelSchedule' {} a -> s {nextToken = a} :: GetChannelSchedule)

-- | The name of the channel associated with this Channel Schedule.
getChannelSchedule_channelName :: Lens.Lens' GetChannelSchedule Prelude.Text
getChannelSchedule_channelName = Lens.lens (\GetChannelSchedule' {channelName} -> channelName) (\s@GetChannelSchedule' {} a -> s {channelName = a} :: GetChannelSchedule)

instance Core.AWSPager GetChannelSchedule where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getChannelScheduleResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getChannelScheduleResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getChannelSchedule_nextToken
              Lens..~ rs
              Lens.^? getChannelScheduleResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetChannelSchedule where
  type
    AWSResponse GetChannelSchedule =
      GetChannelScheduleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelScheduleResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannelSchedule where
  hashWithSalt _salt GetChannelSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` durationMinutes
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData GetChannelSchedule where
  rnf GetChannelSchedule' {..} =
    Prelude.rnf durationMinutes `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf channelName

instance Data.ToHeaders GetChannelSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetChannelSchedule where
  toPath GetChannelSchedule' {..} =
    Prelude.mconcat
      ["/channel/", Data.toBS channelName, "/schedule"]

instance Data.ToQuery GetChannelSchedule where
  toQuery GetChannelSchedule' {..} =
    Prelude.mconcat
      [ "durationMinutes" Data.=: durationMinutes,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetChannelScheduleResponse' smart constructor.
data GetChannelScheduleResponse = GetChannelScheduleResponse'
  { -- | A list of schedule entries for the channel.
    items :: Prelude.Maybe [ScheduleEntry],
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getChannelScheduleResponse_items' - A list of schedule entries for the channel.
--
-- 'nextToken', 'getChannelScheduleResponse_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'httpStatus', 'getChannelScheduleResponse_httpStatus' - The response's http status code.
newGetChannelScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelScheduleResponse
newGetChannelScheduleResponse pHttpStatus_ =
  GetChannelScheduleResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of schedule entries for the channel.
getChannelScheduleResponse_items :: Lens.Lens' GetChannelScheduleResponse (Prelude.Maybe [ScheduleEntry])
getChannelScheduleResponse_items = Lens.lens (\GetChannelScheduleResponse' {items} -> items) (\s@GetChannelScheduleResponse' {} a -> s {items = a} :: GetChannelScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
getChannelScheduleResponse_nextToken :: Lens.Lens' GetChannelScheduleResponse (Prelude.Maybe Prelude.Text)
getChannelScheduleResponse_nextToken = Lens.lens (\GetChannelScheduleResponse' {nextToken} -> nextToken) (\s@GetChannelScheduleResponse' {} a -> s {nextToken = a} :: GetChannelScheduleResponse)

-- | The response's http status code.
getChannelScheduleResponse_httpStatus :: Lens.Lens' GetChannelScheduleResponse Prelude.Int
getChannelScheduleResponse_httpStatus = Lens.lens (\GetChannelScheduleResponse' {httpStatus} -> httpStatus) (\s@GetChannelScheduleResponse' {} a -> s {httpStatus = a} :: GetChannelScheduleResponse)

instance Prelude.NFData GetChannelScheduleResponse where
  rnf GetChannelScheduleResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
