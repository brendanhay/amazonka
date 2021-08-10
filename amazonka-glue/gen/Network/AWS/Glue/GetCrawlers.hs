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
-- Module      : Network.AWS.Glue.GetCrawlers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all crawlers defined in the customer account.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlers
  ( -- * Creating a Request
    GetCrawlers (..),
    newGetCrawlers,

    -- * Request Lenses
    getCrawlers_nextToken,
    getCrawlers_maxResults,

    -- * Destructuring the Response
    GetCrawlersResponse (..),
    newGetCrawlersResponse,

    -- * Response Lenses
    getCrawlersResponse_nextToken,
    getCrawlersResponse_crawlers,
    getCrawlersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCrawlers' smart constructor.
data GetCrawlers = GetCrawlers'
  { -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of crawlers to return on each call.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'getCrawlers_nextToken' - A continuation token, if this is a continuation request.
--
-- 'maxResults', 'getCrawlers_maxResults' - The number of crawlers to return on each call.
newGetCrawlers ::
  GetCrawlers
newGetCrawlers =
  GetCrawlers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation request.
getCrawlers_nextToken :: Lens.Lens' GetCrawlers (Prelude.Maybe Prelude.Text)
getCrawlers_nextToken = Lens.lens (\GetCrawlers' {nextToken} -> nextToken) (\s@GetCrawlers' {} a -> s {nextToken = a} :: GetCrawlers)

-- | The number of crawlers to return on each call.
getCrawlers_maxResults :: Lens.Lens' GetCrawlers (Prelude.Maybe Prelude.Natural)
getCrawlers_maxResults = Lens.lens (\GetCrawlers' {maxResults} -> maxResults) (\s@GetCrawlers' {} a -> s {maxResults = a} :: GetCrawlers)

instance Core.AWSPager GetCrawlers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCrawlersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCrawlersResponse_crawlers Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCrawlers_nextToken
          Lens..~ rs
          Lens.^? getCrawlersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetCrawlers where
  type AWSResponse GetCrawlers = GetCrawlersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Crawlers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCrawlers

instance Prelude.NFData GetCrawlers

instance Core.ToHeaders GetCrawlers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetCrawlers" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCrawlers where
  toJSON GetCrawlers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetCrawlers where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCrawlers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCrawlersResponse' smart constructor.
data GetCrawlersResponse = GetCrawlersResponse'
  { -- | A continuation token, if the returned list has not reached the end of
    -- those defined in this customer account.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of crawler metadata.
    crawlers :: Prelude.Maybe [Crawler],
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
-- 'nextToken', 'getCrawlersResponse_nextToken' - A continuation token, if the returned list has not reached the end of
-- those defined in this customer account.
--
-- 'crawlers', 'getCrawlersResponse_crawlers' - A list of crawler metadata.
--
-- 'httpStatus', 'getCrawlersResponse_httpStatus' - The response's http status code.
newGetCrawlersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCrawlersResponse
newGetCrawlersResponse pHttpStatus_ =
  GetCrawlersResponse'
    { nextToken = Prelude.Nothing,
      crawlers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the returned list has not reached the end of
-- those defined in this customer account.
getCrawlersResponse_nextToken :: Lens.Lens' GetCrawlersResponse (Prelude.Maybe Prelude.Text)
getCrawlersResponse_nextToken = Lens.lens (\GetCrawlersResponse' {nextToken} -> nextToken) (\s@GetCrawlersResponse' {} a -> s {nextToken = a} :: GetCrawlersResponse)

-- | A list of crawler metadata.
getCrawlersResponse_crawlers :: Lens.Lens' GetCrawlersResponse (Prelude.Maybe [Crawler])
getCrawlersResponse_crawlers = Lens.lens (\GetCrawlersResponse' {crawlers} -> crawlers) (\s@GetCrawlersResponse' {} a -> s {crawlers = a} :: GetCrawlersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getCrawlersResponse_httpStatus :: Lens.Lens' GetCrawlersResponse Prelude.Int
getCrawlersResponse_httpStatus = Lens.lens (\GetCrawlersResponse' {httpStatus} -> httpStatus) (\s@GetCrawlersResponse' {} a -> s {httpStatus = a} :: GetCrawlersResponse)

instance Prelude.NFData GetCrawlersResponse
