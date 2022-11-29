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
-- Module      : Amazonka.SESV2.ListSuppressedDestinations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of email addresses that are on the suppression list for
-- your account.
module Amazonka.SESV2.ListSuppressedDestinations
  ( -- * Creating a Request
    ListSuppressedDestinations (..),
    newListSuppressedDestinations,

    -- * Request Lenses
    listSuppressedDestinations_nextToken,
    listSuppressedDestinations_endDate,
    listSuppressedDestinations_pageSize,
    listSuppressedDestinations_startDate,
    listSuppressedDestinations_reasons,

    -- * Destructuring the Response
    ListSuppressedDestinationsResponse (..),
    newListSuppressedDestinationsResponse,

    -- * Response Lenses
    listSuppressedDestinationsResponse_nextToken,
    listSuppressedDestinationsResponse_suppressedDestinationSummaries,
    listSuppressedDestinationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to obtain a list of email destinations that are on the
-- suppression list for your account.
--
-- /See:/ 'newListSuppressedDestinations' smart constructor.
data ListSuppressedDestinations = ListSuppressedDestinations'
  { -- | A token returned from a previous call to @ListSuppressedDestinations@ to
    -- indicate the position in the list of suppressed email addresses.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Used to filter the list of suppressed email destinations so that it only
    -- includes addresses that were added to the list before a specific date.
    endDate :: Prelude.Maybe Core.POSIX,
    -- | The number of results to show in a single call to
    -- @ListSuppressedDestinations@. If the number of results is larger than
    -- the number you specified in this parameter, then the response includes a
    -- @NextToken@ element, which you can use to obtain additional results.
    pageSize :: Prelude.Maybe Prelude.Int,
    -- | Used to filter the list of suppressed email destinations so that it only
    -- includes addresses that were added to the list after a specific date.
    startDate :: Prelude.Maybe Core.POSIX,
    -- | The factors that caused the email address to be added to .
    reasons :: Prelude.Maybe [SuppressionListReason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSuppressedDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSuppressedDestinations_nextToken' - A token returned from a previous call to @ListSuppressedDestinations@ to
-- indicate the position in the list of suppressed email addresses.
--
-- 'endDate', 'listSuppressedDestinations_endDate' - Used to filter the list of suppressed email destinations so that it only
-- includes addresses that were added to the list before a specific date.
--
-- 'pageSize', 'listSuppressedDestinations_pageSize' - The number of results to show in a single call to
-- @ListSuppressedDestinations@. If the number of results is larger than
-- the number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
--
-- 'startDate', 'listSuppressedDestinations_startDate' - Used to filter the list of suppressed email destinations so that it only
-- includes addresses that were added to the list after a specific date.
--
-- 'reasons', 'listSuppressedDestinations_reasons' - The factors that caused the email address to be added to .
newListSuppressedDestinations ::
  ListSuppressedDestinations
newListSuppressedDestinations =
  ListSuppressedDestinations'
    { nextToken =
        Prelude.Nothing,
      endDate = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      startDate = Prelude.Nothing,
      reasons = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListSuppressedDestinations@ to
-- indicate the position in the list of suppressed email addresses.
listSuppressedDestinations_nextToken :: Lens.Lens' ListSuppressedDestinations (Prelude.Maybe Prelude.Text)
listSuppressedDestinations_nextToken = Lens.lens (\ListSuppressedDestinations' {nextToken} -> nextToken) (\s@ListSuppressedDestinations' {} a -> s {nextToken = a} :: ListSuppressedDestinations)

-- | Used to filter the list of suppressed email destinations so that it only
-- includes addresses that were added to the list before a specific date.
listSuppressedDestinations_endDate :: Lens.Lens' ListSuppressedDestinations (Prelude.Maybe Prelude.UTCTime)
listSuppressedDestinations_endDate = Lens.lens (\ListSuppressedDestinations' {endDate} -> endDate) (\s@ListSuppressedDestinations' {} a -> s {endDate = a} :: ListSuppressedDestinations) Prelude.. Lens.mapping Core._Time

-- | The number of results to show in a single call to
-- @ListSuppressedDestinations@. If the number of results is larger than
-- the number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
listSuppressedDestinations_pageSize :: Lens.Lens' ListSuppressedDestinations (Prelude.Maybe Prelude.Int)
listSuppressedDestinations_pageSize = Lens.lens (\ListSuppressedDestinations' {pageSize} -> pageSize) (\s@ListSuppressedDestinations' {} a -> s {pageSize = a} :: ListSuppressedDestinations)

-- | Used to filter the list of suppressed email destinations so that it only
-- includes addresses that were added to the list after a specific date.
listSuppressedDestinations_startDate :: Lens.Lens' ListSuppressedDestinations (Prelude.Maybe Prelude.UTCTime)
listSuppressedDestinations_startDate = Lens.lens (\ListSuppressedDestinations' {startDate} -> startDate) (\s@ListSuppressedDestinations' {} a -> s {startDate = a} :: ListSuppressedDestinations) Prelude.. Lens.mapping Core._Time

-- | The factors that caused the email address to be added to .
listSuppressedDestinations_reasons :: Lens.Lens' ListSuppressedDestinations (Prelude.Maybe [SuppressionListReason])
listSuppressedDestinations_reasons = Lens.lens (\ListSuppressedDestinations' {reasons} -> reasons) (\s@ListSuppressedDestinations' {} a -> s {reasons = a} :: ListSuppressedDestinations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListSuppressedDestinations where
  type
    AWSResponse ListSuppressedDestinations =
      ListSuppressedDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSuppressedDestinationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "SuppressedDestinationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSuppressedDestinations where
  hashWithSalt _salt ListSuppressedDestinations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` reasons

instance Prelude.NFData ListSuppressedDestinations where
  rnf ListSuppressedDestinations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf reasons

instance Core.ToHeaders ListSuppressedDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListSuppressedDestinations where
  toPath =
    Prelude.const "/v2/email/suppression/addresses"

instance Core.ToQuery ListSuppressedDestinations where
  toQuery ListSuppressedDestinations' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "EndDate" Core.=: endDate,
        "PageSize" Core.=: pageSize,
        "StartDate" Core.=: startDate,
        "Reason"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> reasons)
      ]

-- | A list of suppressed email addresses.
--
-- /See:/ 'newListSuppressedDestinationsResponse' smart constructor.
data ListSuppressedDestinationsResponse = ListSuppressedDestinationsResponse'
  { -- | A token that indicates that there are additional email addresses on the
    -- suppression list for your account. To view additional suppressed
    -- addresses, issue another request to @ListSuppressedDestinations@, and
    -- pass this token in the @NextToken@ parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of summaries, each containing a summary for a suppressed email
    -- destination.
    suppressedDestinationSummaries :: Prelude.Maybe [SuppressedDestinationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSuppressedDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSuppressedDestinationsResponse_nextToken' - A token that indicates that there are additional email addresses on the
-- suppression list for your account. To view additional suppressed
-- addresses, issue another request to @ListSuppressedDestinations@, and
-- pass this token in the @NextToken@ parameter.
--
-- 'suppressedDestinationSummaries', 'listSuppressedDestinationsResponse_suppressedDestinationSummaries' - A list of summaries, each containing a summary for a suppressed email
-- destination.
--
-- 'httpStatus', 'listSuppressedDestinationsResponse_httpStatus' - The response's http status code.
newListSuppressedDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSuppressedDestinationsResponse
newListSuppressedDestinationsResponse pHttpStatus_ =
  ListSuppressedDestinationsResponse'
    { nextToken =
        Prelude.Nothing,
      suppressedDestinationSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates that there are additional email addresses on the
-- suppression list for your account. To view additional suppressed
-- addresses, issue another request to @ListSuppressedDestinations@, and
-- pass this token in the @NextToken@ parameter.
listSuppressedDestinationsResponse_nextToken :: Lens.Lens' ListSuppressedDestinationsResponse (Prelude.Maybe Prelude.Text)
listSuppressedDestinationsResponse_nextToken = Lens.lens (\ListSuppressedDestinationsResponse' {nextToken} -> nextToken) (\s@ListSuppressedDestinationsResponse' {} a -> s {nextToken = a} :: ListSuppressedDestinationsResponse)

-- | A list of summaries, each containing a summary for a suppressed email
-- destination.
listSuppressedDestinationsResponse_suppressedDestinationSummaries :: Lens.Lens' ListSuppressedDestinationsResponse (Prelude.Maybe [SuppressedDestinationSummary])
listSuppressedDestinationsResponse_suppressedDestinationSummaries = Lens.lens (\ListSuppressedDestinationsResponse' {suppressedDestinationSummaries} -> suppressedDestinationSummaries) (\s@ListSuppressedDestinationsResponse' {} a -> s {suppressedDestinationSummaries = a} :: ListSuppressedDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSuppressedDestinationsResponse_httpStatus :: Lens.Lens' ListSuppressedDestinationsResponse Prelude.Int
listSuppressedDestinationsResponse_httpStatus = Lens.lens (\ListSuppressedDestinationsResponse' {httpStatus} -> httpStatus) (\s@ListSuppressedDestinationsResponse' {} a -> s {httpStatus = a} :: ListSuppressedDestinationsResponse)

instance
  Prelude.NFData
    ListSuppressedDestinationsResponse
  where
  rnf ListSuppressedDestinationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf suppressedDestinationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
