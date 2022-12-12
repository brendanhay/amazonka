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
-- Module      : Amazonka.KinesisAnalyticsV2.ListApplications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Kinesis Data Analytics applications in your account.
-- For each application, the response includes the application name, Amazon
-- Resource Name (ARN), and status.
--
-- If you want detailed information about a specific application, use
-- DescribeApplication.
--
-- This operation returns paginated results.
module Amazonka.KinesisAnalyticsV2.ListApplications
  ( -- * Creating a Request
    ListApplications (..),
    newListApplications,

    -- * Request Lenses
    listApplications_limit,
    listApplications_nextToken,

    -- * Destructuring the Response
    ListApplicationsResponse (..),
    newListApplicationsResponse,

    -- * Response Lenses
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applicationSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListApplications' smart constructor.
data ListApplications = ListApplications'
  { -- | The maximum number of applications to list.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If a previous command returned a pagination token, pass it into this
    -- value to retrieve the next set of results. For more information about
    -- pagination, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listApplications_limit' - The maximum number of applications to list.
--
-- 'nextToken', 'listApplications_nextToken' - If a previous command returned a pagination token, pass it into this
-- value to retrieve the next set of results. For more information about
-- pagination, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
newListApplications ::
  ListApplications
newListApplications =
  ListApplications'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of applications to list.
listApplications_limit :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Natural)
listApplications_limit = Lens.lens (\ListApplications' {limit} -> limit) (\s@ListApplications' {} a -> s {limit = a} :: ListApplications)

-- | If a previous command returned a pagination token, pass it into this
-- value to retrieve the next set of results. For more information about
-- pagination, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
listApplications_nextToken :: Lens.Lens' ListApplications (Prelude.Maybe Prelude.Text)
listApplications_nextToken = Lens.lens (\ListApplications' {nextToken} -> nextToken) (\s@ListApplications' {} a -> s {nextToken = a} :: ListApplications)

instance Core.AWSPager ListApplications where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listApplicationsResponse_applicationSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApplications_nextToken
          Lens..~ rs
          Lens.^? listApplicationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListApplications where
  type
    AWSResponse ListApplications =
      ListApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "ApplicationSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListApplications where
  hashWithSalt _salt ListApplications' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApplications where
  rnf ListApplications' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.ListApplications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApplications where
  toJSON ListApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListApplications where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { -- | The pagination token for the next set of results, or @null@ if there are
    -- no additional results. Pass this token into a subsequent command to
    -- retrieve the next set of items For more information about pagination,
    -- see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ApplicationSummary@ objects.
    applicationSummaries :: [ApplicationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationsResponse_nextToken' - The pagination token for the next set of results, or @null@ if there are
-- no additional results. Pass this token into a subsequent command to
-- retrieve the next set of items For more information about pagination,
-- see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
--
-- 'httpStatus', 'listApplicationsResponse_httpStatus' - The response's http status code.
--
-- 'applicationSummaries', 'listApplicationsResponse_applicationSummaries' - A list of @ApplicationSummary@ objects.
newListApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationsResponse
newListApplicationsResponse pHttpStatus_ =
  ListApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      applicationSummaries = Prelude.mempty
    }

-- | The pagination token for the next set of results, or @null@ if there are
-- no additional results. Pass this token into a subsequent command to
-- retrieve the next set of items For more information about pagination,
-- see
-- <https://docs.aws.amazon.com/cli/latest/userguide/pagination.html Using the Amazon Command Line Interface\'s Pagination Options>.
listApplicationsResponse_nextToken :: Lens.Lens' ListApplicationsResponse (Prelude.Maybe Prelude.Text)
listApplicationsResponse_nextToken = Lens.lens (\ListApplicationsResponse' {nextToken} -> nextToken) (\s@ListApplicationsResponse' {} a -> s {nextToken = a} :: ListApplicationsResponse)

-- | The response's http status code.
listApplicationsResponse_httpStatus :: Lens.Lens' ListApplicationsResponse Prelude.Int
listApplicationsResponse_httpStatus = Lens.lens (\ListApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationsResponse' {} a -> s {httpStatus = a} :: ListApplicationsResponse)

-- | A list of @ApplicationSummary@ objects.
listApplicationsResponse_applicationSummaries :: Lens.Lens' ListApplicationsResponse [ApplicationSummary]
listApplicationsResponse_applicationSummaries = Lens.lens (\ListApplicationsResponse' {applicationSummaries} -> applicationSummaries) (\s@ListApplicationsResponse' {} a -> s {applicationSummaries = a} :: ListApplicationsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListApplicationsResponse where
  rnf ListApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationSummaries
