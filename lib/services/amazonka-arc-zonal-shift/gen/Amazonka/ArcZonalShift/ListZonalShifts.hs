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
-- Module      : Amazonka.ArcZonalShift.ListZonalShifts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the active zonal shifts in Amazon Route 53 Application
-- Recovery Controller in your AWS account in this AWS Region.
--
-- This operation returns paginated results.
module Amazonka.ArcZonalShift.ListZonalShifts
  ( -- * Creating a Request
    ListZonalShifts (..),
    newListZonalShifts,

    -- * Request Lenses
    listZonalShifts_maxResults,
    listZonalShifts_nextToken,
    listZonalShifts_status,

    -- * Destructuring the Response
    ListZonalShiftsResponse (..),
    newListZonalShiftsResponse,

    -- * Response Lenses
    listZonalShiftsResponse_items,
    listZonalShiftsResponse_nextToken,
    listZonalShiftsResponse_httpStatus,
  )
where

import Amazonka.ArcZonalShift.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListZonalShifts' smart constructor.
data ListZonalShifts = ListZonalShifts'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A status for a zonal shift.
    --
    -- The @Status@ for a zonal shift can have one of the following values:
    --
    -- -   __ACTIVE__: The zonal shift is started and active.
    --
    -- -   __EXPIRED__: The zonal shift has expired (the expiry time was
    --     exceeded).
    --
    -- -   __CANCELED__: The zonal shift was canceled.
    status :: Prelude.Maybe ZonalShiftStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListZonalShifts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listZonalShifts_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listZonalShifts_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'status', 'listZonalShifts_status' - A status for a zonal shift.
--
-- The @Status@ for a zonal shift can have one of the following values:
--
-- -   __ACTIVE__: The zonal shift is started and active.
--
-- -   __EXPIRED__: The zonal shift has expired (the expiry time was
--     exceeded).
--
-- -   __CANCELED__: The zonal shift was canceled.
newListZonalShifts ::
  ListZonalShifts
newListZonalShifts =
  ListZonalShifts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listZonalShifts_maxResults :: Lens.Lens' ListZonalShifts (Prelude.Maybe Prelude.Natural)
listZonalShifts_maxResults = Lens.lens (\ListZonalShifts' {maxResults} -> maxResults) (\s@ListZonalShifts' {} a -> s {maxResults = a} :: ListZonalShifts)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listZonalShifts_nextToken :: Lens.Lens' ListZonalShifts (Prelude.Maybe Prelude.Text)
listZonalShifts_nextToken = Lens.lens (\ListZonalShifts' {nextToken} -> nextToken) (\s@ListZonalShifts' {} a -> s {nextToken = a} :: ListZonalShifts)

-- | A status for a zonal shift.
--
-- The @Status@ for a zonal shift can have one of the following values:
--
-- -   __ACTIVE__: The zonal shift is started and active.
--
-- -   __EXPIRED__: The zonal shift has expired (the expiry time was
--     exceeded).
--
-- -   __CANCELED__: The zonal shift was canceled.
listZonalShifts_status :: Lens.Lens' ListZonalShifts (Prelude.Maybe ZonalShiftStatus)
listZonalShifts_status = Lens.lens (\ListZonalShifts' {status} -> status) (\s@ListZonalShifts' {} a -> s {status = a} :: ListZonalShifts)

instance Core.AWSPager ListZonalShifts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listZonalShiftsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listZonalShiftsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listZonalShifts_nextToken
          Lens..~ rs
          Lens.^? listZonalShiftsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListZonalShifts where
  type
    AWSResponse ListZonalShifts =
      ListZonalShiftsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListZonalShiftsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListZonalShifts where
  hashWithSalt _salt ListZonalShifts' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListZonalShifts where
  rnf ListZonalShifts' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders ListZonalShifts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListZonalShifts where
  toPath = Prelude.const "/zonalshifts"

instance Data.ToQuery ListZonalShifts where
  toQuery ListZonalShifts' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListZonalShiftsResponse' smart constructor.
data ListZonalShiftsResponse = ListZonalShiftsResponse'
  { -- | The items in the response list.
    items :: Prelude.Maybe [ZonalShiftSummary],
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListZonalShiftsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listZonalShiftsResponse_items' - The items in the response list.
--
-- 'nextToken', 'listZonalShiftsResponse_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'httpStatus', 'listZonalShiftsResponse_httpStatus' - The response's http status code.
newListZonalShiftsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListZonalShiftsResponse
newListZonalShiftsResponse pHttpStatus_ =
  ListZonalShiftsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The items in the response list.
listZonalShiftsResponse_items :: Lens.Lens' ListZonalShiftsResponse (Prelude.Maybe [ZonalShiftSummary])
listZonalShiftsResponse_items = Lens.lens (\ListZonalShiftsResponse' {items} -> items) (\s@ListZonalShiftsResponse' {} a -> s {items = a} :: ListZonalShiftsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listZonalShiftsResponse_nextToken :: Lens.Lens' ListZonalShiftsResponse (Prelude.Maybe Prelude.Text)
listZonalShiftsResponse_nextToken = Lens.lens (\ListZonalShiftsResponse' {nextToken} -> nextToken) (\s@ListZonalShiftsResponse' {} a -> s {nextToken = a} :: ListZonalShiftsResponse)

-- | The response's http status code.
listZonalShiftsResponse_httpStatus :: Lens.Lens' ListZonalShiftsResponse Prelude.Int
listZonalShiftsResponse_httpStatus = Lens.lens (\ListZonalShiftsResponse' {httpStatus} -> httpStatus) (\s@ListZonalShiftsResponse' {} a -> s {httpStatus = a} :: ListZonalShiftsResponse)

instance Prelude.NFData ListZonalShiftsResponse where
  rnf ListZonalShiftsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
