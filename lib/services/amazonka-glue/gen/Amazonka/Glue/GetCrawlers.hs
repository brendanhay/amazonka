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
-- Module      : Amazonka.Glue.GetCrawlers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all crawlers defined in the customer account.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetCrawlers
  ( -- * Creating a Request
    GetCrawlers (..),
    newGetCrawlers,

    -- * Request Lenses
    getCrawlers_maxResults,
    getCrawlers_nextToken,

    -- * Destructuring the Response
    GetCrawlersResponse (..),
    newGetCrawlersResponse,

    -- * Response Lenses
    getCrawlersResponse_crawlers,
    getCrawlersResponse_nextToken,
    getCrawlersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCrawlers' smart constructor.
data GetCrawlers = GetCrawlers'
  { -- | The number of crawlers to return on each call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCrawlers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCrawlers_maxResults' - The number of crawlers to return on each call.
--
-- 'nextToken', 'getCrawlers_nextToken' - A continuation token, if this is a continuation request.
newGetCrawlers ::
  GetCrawlers
newGetCrawlers =
  GetCrawlers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of crawlers to return on each call.
getCrawlers_maxResults :: Lens.Lens' GetCrawlers (Prelude.Maybe Prelude.Natural)
getCrawlers_maxResults = Lens.lens (\GetCrawlers' {maxResults} -> maxResults) (\s@GetCrawlers' {} a -> s {maxResults = a} :: GetCrawlers)

-- | A continuation token, if this is a continuation request.
getCrawlers_nextToken :: Lens.Lens' GetCrawlers (Prelude.Maybe Prelude.Text)
getCrawlers_nextToken = Lens.lens (\GetCrawlers' {nextToken} -> nextToken) (\s@GetCrawlers' {} a -> s {nextToken = a} :: GetCrawlers)

instance Core.AWSPager GetCrawlers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCrawlersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCrawlersResponse_crawlers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getCrawlers_nextToken
          Lens..~ rs
          Lens.^? getCrawlersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetCrawlers where
  type AWSResponse GetCrawlers = GetCrawlersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlersResponse'
            Prelude.<$> (x Data..?> "Crawlers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCrawlers where
  hashWithSalt _salt GetCrawlers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetCrawlers where
  rnf GetCrawlers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetCrawlers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetCrawlers" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCrawlers where
  toJSON GetCrawlers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetCrawlers where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCrawlers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCrawlersResponse' smart constructor.
data GetCrawlersResponse = GetCrawlersResponse'
  { -- | A list of crawler metadata.
    crawlers :: Prelude.Maybe [Crawler],
    -- | A continuation token, if the returned list has not reached the end of
    -- those defined in this customer account.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCrawlersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlers', 'getCrawlersResponse_crawlers' - A list of crawler metadata.
--
-- 'nextToken', 'getCrawlersResponse_nextToken' - A continuation token, if the returned list has not reached the end of
-- those defined in this customer account.
--
-- 'httpStatus', 'getCrawlersResponse_httpStatus' - The response's http status code.
newGetCrawlersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCrawlersResponse
newGetCrawlersResponse pHttpStatus_ =
  GetCrawlersResponse'
    { crawlers = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of crawler metadata.
getCrawlersResponse_crawlers :: Lens.Lens' GetCrawlersResponse (Prelude.Maybe [Crawler])
getCrawlersResponse_crawlers = Lens.lens (\GetCrawlersResponse' {crawlers} -> crawlers) (\s@GetCrawlersResponse' {} a -> s {crawlers = a} :: GetCrawlersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the returned list has not reached the end of
-- those defined in this customer account.
getCrawlersResponse_nextToken :: Lens.Lens' GetCrawlersResponse (Prelude.Maybe Prelude.Text)
getCrawlersResponse_nextToken = Lens.lens (\GetCrawlersResponse' {nextToken} -> nextToken) (\s@GetCrawlersResponse' {} a -> s {nextToken = a} :: GetCrawlersResponse)

-- | The response's http status code.
getCrawlersResponse_httpStatus :: Lens.Lens' GetCrawlersResponse Prelude.Int
getCrawlersResponse_httpStatus = Lens.lens (\GetCrawlersResponse' {httpStatus} -> httpStatus) (\s@GetCrawlersResponse' {} a -> s {httpStatus = a} :: GetCrawlersResponse)

instance Prelude.NFData GetCrawlersResponse where
  rnf GetCrawlersResponse' {..} =
    Prelude.rnf crawlers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
