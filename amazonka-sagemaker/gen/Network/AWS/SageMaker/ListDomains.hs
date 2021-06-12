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
-- Module      : Network.AWS.SageMaker.ListDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the domains.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_nextToken,
    listDomains_maxResults,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list up to a specified limit.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomains_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'maxResults', 'listDomains_maxResults' - Returns a list up to a specified limit.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listDomains_nextToken :: Lens.Lens' ListDomains (Core.Maybe Core.Text)
listDomains_nextToken = Lens.lens (\ListDomains' {nextToken} -> nextToken) (\s@ListDomains' {} a -> s {nextToken = a} :: ListDomains)

-- | Returns a list up to a specified limit.
listDomains_maxResults :: Lens.Lens' ListDomains (Core.Maybe Core.Natural)
listDomains_maxResults = Lens.lens (\ListDomains' {maxResults} -> maxResults) (\s@ListDomains' {} a -> s {maxResults = a} :: ListDomains)

instance Core.AWSPager ListDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_domains Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDomains_nextToken
          Lens..~ rs
          Lens.^? listDomainsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Domains" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDomains

instance Core.NFData ListDomains

instance Core.ToHeaders ListDomains where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListDomains" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListDomains where
  toPath = Core.const "/"

instance Core.ToQuery ListDomains where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of domains.
    domains :: Core.Maybe [DomainDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainsResponse_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'domains', 'listDomainsResponse_domains' - The list of domains.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
newListDomainsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { nextToken = Core.Nothing,
      domains = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listDomainsResponse_nextToken :: Lens.Lens' ListDomainsResponse (Core.Maybe Core.Text)
listDomainsResponse_nextToken = Lens.lens (\ListDomainsResponse' {nextToken} -> nextToken) (\s@ListDomainsResponse' {} a -> s {nextToken = a} :: ListDomainsResponse)

-- | The list of domains.
listDomainsResponse_domains :: Lens.Lens' ListDomainsResponse (Core.Maybe [DomainDetails])
listDomainsResponse_domains = Lens.lens (\ListDomainsResponse' {domains} -> domains) (\s@ListDomainsResponse' {} a -> s {domains = a} :: ListDomainsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Core.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

instance Core.NFData ListDomainsResponse
