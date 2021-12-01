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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getChannelSchedule_nextToken,
    getChannelSchedule_durationMinutes,
    getChannelSchedule_maxResults,
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
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelSchedule' smart constructor.
data GetChannelSchedule = GetChannelSchedule'
  { -- | Pagination token from the GET list request. Use the token to fetch the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The schedule duration in minutes. The maximum duration is 4320 minutes
    -- (three days).
    durationMinutes :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return. The maximum number of
    -- results is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the channel you are working on.
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
-- 'nextToken', 'getChannelSchedule_nextToken' - Pagination token from the GET list request. Use the token to fetch the
-- next page of results.
--
-- 'durationMinutes', 'getChannelSchedule_durationMinutes' - The schedule duration in minutes. The maximum duration is 4320 minutes
-- (three days).
--
-- 'maxResults', 'getChannelSchedule_maxResults' - Upper bound on number of records to return. The maximum number of
-- results is 100.
--
-- 'channelName', 'getChannelSchedule_channelName' - The identifier for the channel you are working on.
newGetChannelSchedule ::
  -- | 'channelName'
  Prelude.Text ->
  GetChannelSchedule
newGetChannelSchedule pChannelName_ =
  GetChannelSchedule'
    { nextToken = Prelude.Nothing,
      durationMinutes = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      channelName = pChannelName_
    }

-- | Pagination token from the GET list request. Use the token to fetch the
-- next page of results.
getChannelSchedule_nextToken :: Lens.Lens' GetChannelSchedule (Prelude.Maybe Prelude.Text)
getChannelSchedule_nextToken = Lens.lens (\GetChannelSchedule' {nextToken} -> nextToken) (\s@GetChannelSchedule' {} a -> s {nextToken = a} :: GetChannelSchedule)

-- | The schedule duration in minutes. The maximum duration is 4320 minutes
-- (three days).
getChannelSchedule_durationMinutes :: Lens.Lens' GetChannelSchedule (Prelude.Maybe Prelude.Text)
getChannelSchedule_durationMinutes = Lens.lens (\GetChannelSchedule' {durationMinutes} -> durationMinutes) (\s@GetChannelSchedule' {} a -> s {durationMinutes = a} :: GetChannelSchedule)

-- | Upper bound on number of records to return. The maximum number of
-- results is 100.
getChannelSchedule_maxResults :: Lens.Lens' GetChannelSchedule (Prelude.Maybe Prelude.Natural)
getChannelSchedule_maxResults = Lens.lens (\GetChannelSchedule' {maxResults} -> maxResults) (\s@GetChannelSchedule' {} a -> s {maxResults = a} :: GetChannelSchedule)

-- | The identifier for the channel you are working on.
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelScheduleResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChannelSchedule where
  hashWithSalt salt' GetChannelSchedule' {..} =
    salt' `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` durationMinutes
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetChannelSchedule where
  rnf GetChannelSchedule' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf durationMinutes

instance Core.ToHeaders GetChannelSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetChannelSchedule where
  toPath GetChannelSchedule' {..} =
    Prelude.mconcat
      ["/channel/", Core.toBS channelName, "/schedule"]

instance Core.ToQuery GetChannelSchedule where
  toQuery GetChannelSchedule' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "durationMinutes" Core.=: durationMinutes,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetChannelScheduleResponse' smart constructor.
data GetChannelScheduleResponse = GetChannelScheduleResponse'
  { -- | An array of schedule entries for the channel.
    items :: Prelude.Maybe [ScheduleEntry],
    -- | Pagination token from the GET list request. Use the token to fetch the
    -- next page of results.
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
-- 'items', 'getChannelScheduleResponse_items' - An array of schedule entries for the channel.
--
-- 'nextToken', 'getChannelScheduleResponse_nextToken' - Pagination token from the GET list request. Use the token to fetch the
-- next page of results.
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

-- | An array of schedule entries for the channel.
getChannelScheduleResponse_items :: Lens.Lens' GetChannelScheduleResponse (Prelude.Maybe [ScheduleEntry])
getChannelScheduleResponse_items = Lens.lens (\GetChannelScheduleResponse' {items} -> items) (\s@GetChannelScheduleResponse' {} a -> s {items = a} :: GetChannelScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token from the GET list request. Use the token to fetch the
-- next page of results.
getChannelScheduleResponse_nextToken :: Lens.Lens' GetChannelScheduleResponse (Prelude.Maybe Prelude.Text)
getChannelScheduleResponse_nextToken = Lens.lens (\GetChannelScheduleResponse' {nextToken} -> nextToken) (\s@GetChannelScheduleResponse' {} a -> s {nextToken = a} :: GetChannelScheduleResponse)

-- | The response's http status code.
getChannelScheduleResponse_httpStatus :: Lens.Lens' GetChannelScheduleResponse Prelude.Int
getChannelScheduleResponse_httpStatus = Lens.lens (\GetChannelScheduleResponse' {httpStatus} -> httpStatus) (\s@GetChannelScheduleResponse' {} a -> s {httpStatus = a} :: GetChannelScheduleResponse)

instance Prelude.NFData GetChannelScheduleResponse where
  rnf GetChannelScheduleResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
