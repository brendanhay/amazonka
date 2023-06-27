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
-- Module      : Amazonka.SecurityLake.ListDataLakeExceptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Security Lake exceptions that you can use to find the
-- source of problems and fix them.
--
-- This operation returns paginated results.
module Amazonka.SecurityLake.ListDataLakeExceptions
  ( -- * Creating a Request
    ListDataLakeExceptions (..),
    newListDataLakeExceptions,

    -- * Request Lenses
    listDataLakeExceptions_maxResults,
    listDataLakeExceptions_nextToken,
    listDataLakeExceptions_regions,

    -- * Destructuring the Response
    ListDataLakeExceptionsResponse (..),
    newListDataLakeExceptionsResponse,

    -- * Response Lenses
    listDataLakeExceptionsResponse_exceptions,
    listDataLakeExceptionsResponse_nextToken,
    listDataLakeExceptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newListDataLakeExceptions' smart constructor.
data ListDataLakeExceptions = ListDataLakeExceptions'
  { -- | List the maximum number of failures in Security Lake.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | List if there are more results available. The value of nextToken is a
    -- unique pagination token for each page. Repeat the call using the
    -- returned token to retrieve the next page. Keep all other arguments
    -- unchanged.
    --
    -- Each pagination token expires after 24 hours. Using an expired
    -- pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List the Amazon Web Services Regions from which exceptions are
    -- retrieved.
    regions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataLakeExceptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataLakeExceptions_maxResults' - List the maximum number of failures in Security Lake.
--
-- 'nextToken', 'listDataLakeExceptions_nextToken' - List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
--
-- 'regions', 'listDataLakeExceptions_regions' - List the Amazon Web Services Regions from which exceptions are
-- retrieved.
newListDataLakeExceptions ::
  ListDataLakeExceptions
newListDataLakeExceptions =
  ListDataLakeExceptions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      regions = Prelude.Nothing
    }

-- | List the maximum number of failures in Security Lake.
listDataLakeExceptions_maxResults :: Lens.Lens' ListDataLakeExceptions (Prelude.Maybe Prelude.Natural)
listDataLakeExceptions_maxResults = Lens.lens (\ListDataLakeExceptions' {maxResults} -> maxResults) (\s@ListDataLakeExceptions' {} a -> s {maxResults = a} :: ListDataLakeExceptions)

-- | List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
listDataLakeExceptions_nextToken :: Lens.Lens' ListDataLakeExceptions (Prelude.Maybe Prelude.Text)
listDataLakeExceptions_nextToken = Lens.lens (\ListDataLakeExceptions' {nextToken} -> nextToken) (\s@ListDataLakeExceptions' {} a -> s {nextToken = a} :: ListDataLakeExceptions)

-- | List the Amazon Web Services Regions from which exceptions are
-- retrieved.
listDataLakeExceptions_regions :: Lens.Lens' ListDataLakeExceptions (Prelude.Maybe [Prelude.Text])
listDataLakeExceptions_regions = Lens.lens (\ListDataLakeExceptions' {regions} -> regions) (\s@ListDataLakeExceptions' {} a -> s {regions = a} :: ListDataLakeExceptions) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListDataLakeExceptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataLakeExceptionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataLakeExceptionsResponse_exceptions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDataLakeExceptions_nextToken
          Lens..~ rs
          Lens.^? listDataLakeExceptionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDataLakeExceptions where
  type
    AWSResponse ListDataLakeExceptions =
      ListDataLakeExceptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataLakeExceptionsResponse'
            Prelude.<$> (x Data..?> "exceptions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataLakeExceptions where
  hashWithSalt _salt ListDataLakeExceptions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regions

instance Prelude.NFData ListDataLakeExceptions where
  rnf ListDataLakeExceptions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regions

instance Data.ToHeaders ListDataLakeExceptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDataLakeExceptions where
  toJSON ListDataLakeExceptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("regions" Data..=) Prelude.<$> regions
          ]
      )

instance Data.ToPath ListDataLakeExceptions where
  toPath = Prelude.const "/v1/datalake/exceptions"

instance Data.ToQuery ListDataLakeExceptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataLakeExceptionsResponse' smart constructor.
data ListDataLakeExceptionsResponse = ListDataLakeExceptionsResponse'
  { -- | Lists the failures that cannot be retried in the current Region.
    exceptions :: Prelude.Maybe [DataLakeException],
    -- | List if there are more results available. The value of nextToken is a
    -- unique pagination token for each page. Repeat the call using the
    -- returned token to retrieve the next page. Keep all other arguments
    -- unchanged.
    --
    -- Each pagination token expires after 24 hours. Using an expired
    -- pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataLakeExceptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptions', 'listDataLakeExceptionsResponse_exceptions' - Lists the failures that cannot be retried in the current Region.
--
-- 'nextToken', 'listDataLakeExceptionsResponse_nextToken' - List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
--
-- 'httpStatus', 'listDataLakeExceptionsResponse_httpStatus' - The response's http status code.
newListDataLakeExceptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataLakeExceptionsResponse
newListDataLakeExceptionsResponse pHttpStatus_ =
  ListDataLakeExceptionsResponse'
    { exceptions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the failures that cannot be retried in the current Region.
listDataLakeExceptionsResponse_exceptions :: Lens.Lens' ListDataLakeExceptionsResponse (Prelude.Maybe [DataLakeException])
listDataLakeExceptionsResponse_exceptions = Lens.lens (\ListDataLakeExceptionsResponse' {exceptions} -> exceptions) (\s@ListDataLakeExceptionsResponse' {} a -> s {exceptions = a} :: ListDataLakeExceptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
listDataLakeExceptionsResponse_nextToken :: Lens.Lens' ListDataLakeExceptionsResponse (Prelude.Maybe Prelude.Text)
listDataLakeExceptionsResponse_nextToken = Lens.lens (\ListDataLakeExceptionsResponse' {nextToken} -> nextToken) (\s@ListDataLakeExceptionsResponse' {} a -> s {nextToken = a} :: ListDataLakeExceptionsResponse)

-- | The response's http status code.
listDataLakeExceptionsResponse_httpStatus :: Lens.Lens' ListDataLakeExceptionsResponse Prelude.Int
listDataLakeExceptionsResponse_httpStatus = Lens.lens (\ListDataLakeExceptionsResponse' {httpStatus} -> httpStatus) (\s@ListDataLakeExceptionsResponse' {} a -> s {httpStatus = a} :: ListDataLakeExceptionsResponse)

instance
  Prelude.NFData
    ListDataLakeExceptionsResponse
  where
  rnf ListDataLakeExceptionsResponse' {..} =
    Prelude.rnf exceptions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
