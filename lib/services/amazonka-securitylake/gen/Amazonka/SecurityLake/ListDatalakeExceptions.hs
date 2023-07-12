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
-- Module      : Amazonka.SecurityLake.ListDatalakeExceptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Security Lake exceptions that you can use to find the
-- source of problems and fix them.
--
-- This operation returns paginated results.
module Amazonka.SecurityLake.ListDatalakeExceptions
  ( -- * Creating a Request
    ListDatalakeExceptions (..),
    newListDatalakeExceptions,

    -- * Request Lenses
    listDatalakeExceptions_maxFailures,
    listDatalakeExceptions_nextToken,
    listDatalakeExceptions_regionSet,

    -- * Destructuring the Response
    ListDatalakeExceptionsResponse (..),
    newListDatalakeExceptionsResponse,

    -- * Response Lenses
    listDatalakeExceptionsResponse_nextToken,
    listDatalakeExceptionsResponse_httpStatus,
    listDatalakeExceptionsResponse_nonRetryableFailures,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newListDatalakeExceptions' smart constructor.
data ListDatalakeExceptions = ListDatalakeExceptions'
  { -- | List the maximum number of failures in Security Lake.
    maxFailures :: Prelude.Maybe Prelude.Int,
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
    regionSet :: Prelude.Maybe [Region]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatalakeExceptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxFailures', 'listDatalakeExceptions_maxFailures' - List the maximum number of failures in Security Lake.
--
-- 'nextToken', 'listDatalakeExceptions_nextToken' - List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
--
-- 'regionSet', 'listDatalakeExceptions_regionSet' - List the Amazon Web Services Regions from which exceptions are
-- retrieved.
newListDatalakeExceptions ::
  ListDatalakeExceptions
newListDatalakeExceptions =
  ListDatalakeExceptions'
    { maxFailures =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      regionSet = Prelude.Nothing
    }

-- | List the maximum number of failures in Security Lake.
listDatalakeExceptions_maxFailures :: Lens.Lens' ListDatalakeExceptions (Prelude.Maybe Prelude.Int)
listDatalakeExceptions_maxFailures = Lens.lens (\ListDatalakeExceptions' {maxFailures} -> maxFailures) (\s@ListDatalakeExceptions' {} a -> s {maxFailures = a} :: ListDatalakeExceptions)

-- | List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
listDatalakeExceptions_nextToken :: Lens.Lens' ListDatalakeExceptions (Prelude.Maybe Prelude.Text)
listDatalakeExceptions_nextToken = Lens.lens (\ListDatalakeExceptions' {nextToken} -> nextToken) (\s@ListDatalakeExceptions' {} a -> s {nextToken = a} :: ListDatalakeExceptions)

-- | List the Amazon Web Services Regions from which exceptions are
-- retrieved.
listDatalakeExceptions_regionSet :: Lens.Lens' ListDatalakeExceptions (Prelude.Maybe [Region])
listDatalakeExceptions_regionSet = Lens.lens (\ListDatalakeExceptions' {regionSet} -> regionSet) (\s@ListDatalakeExceptions' {} a -> s {regionSet = a} :: ListDatalakeExceptions) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListDatalakeExceptions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatalakeExceptionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDatalakeExceptionsResponse_nonRetryableFailures
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDatalakeExceptions_nextToken
          Lens..~ rs
          Lens.^? listDatalakeExceptionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDatalakeExceptions where
  type
    AWSResponse ListDatalakeExceptions =
      ListDatalakeExceptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatalakeExceptionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "nonRetryableFailures"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDatalakeExceptions where
  hashWithSalt _salt ListDatalakeExceptions' {..} =
    _salt
      `Prelude.hashWithSalt` maxFailures
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regionSet

instance Prelude.NFData ListDatalakeExceptions where
  rnf ListDatalakeExceptions' {..} =
    Prelude.rnf maxFailures
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regionSet

instance Data.ToHeaders ListDatalakeExceptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatalakeExceptions where
  toJSON ListDatalakeExceptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxFailures" Data..=) Prelude.<$> maxFailures,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("regionSet" Data..=) Prelude.<$> regionSet
          ]
      )

instance Data.ToPath ListDatalakeExceptions where
  toPath = Prelude.const "/v1/datalake/exceptions"

instance Data.ToQuery ListDatalakeExceptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatalakeExceptionsResponse' smart constructor.
data ListDatalakeExceptionsResponse = ListDatalakeExceptionsResponse'
  { -- | List if there are more results available. The value of nextToken is a
    -- unique pagination token for each page. Repeat the call using the
    -- returned token to retrieve the next page. Keep all other arguments
    -- unchanged.
    --
    -- Each pagination token expires after 24 hours. Using an expired
    -- pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists the failures that cannot be retried in the current Region.
    nonRetryableFailures :: [FailuresResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatalakeExceptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatalakeExceptionsResponse_nextToken' - List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
--
-- 'httpStatus', 'listDatalakeExceptionsResponse_httpStatus' - The response's http status code.
--
-- 'nonRetryableFailures', 'listDatalakeExceptionsResponse_nonRetryableFailures' - Lists the failures that cannot be retried in the current Region.
newListDatalakeExceptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatalakeExceptionsResponse
newListDatalakeExceptionsResponse pHttpStatus_ =
  ListDatalakeExceptionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      nonRetryableFailures = Prelude.mempty
    }

-- | List if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
listDatalakeExceptionsResponse_nextToken :: Lens.Lens' ListDatalakeExceptionsResponse (Prelude.Maybe Prelude.Text)
listDatalakeExceptionsResponse_nextToken = Lens.lens (\ListDatalakeExceptionsResponse' {nextToken} -> nextToken) (\s@ListDatalakeExceptionsResponse' {} a -> s {nextToken = a} :: ListDatalakeExceptionsResponse)

-- | The response's http status code.
listDatalakeExceptionsResponse_httpStatus :: Lens.Lens' ListDatalakeExceptionsResponse Prelude.Int
listDatalakeExceptionsResponse_httpStatus = Lens.lens (\ListDatalakeExceptionsResponse' {httpStatus} -> httpStatus) (\s@ListDatalakeExceptionsResponse' {} a -> s {httpStatus = a} :: ListDatalakeExceptionsResponse)

-- | Lists the failures that cannot be retried in the current Region.
listDatalakeExceptionsResponse_nonRetryableFailures :: Lens.Lens' ListDatalakeExceptionsResponse [FailuresResponse]
listDatalakeExceptionsResponse_nonRetryableFailures = Lens.lens (\ListDatalakeExceptionsResponse' {nonRetryableFailures} -> nonRetryableFailures) (\s@ListDatalakeExceptionsResponse' {} a -> s {nonRetryableFailures = a} :: ListDatalakeExceptionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDatalakeExceptionsResponse
  where
  rnf ListDatalakeExceptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nonRetryableFailures
