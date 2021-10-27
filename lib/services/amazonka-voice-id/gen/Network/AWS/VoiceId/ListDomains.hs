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
-- Module      : Network.AWS.VoiceId.ListDomains
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the domains in the Amazon Web Services account.
module Network.AWS.VoiceId.ListDomains
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
    listDomainsResponse_domainSummaries,
    listDomainsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.VoiceId.Types

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that are returned per call. You can use
    -- @NextToken@ to obtain further pages of results. The default is 100; the
    -- maximum allowed page size is also 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomains_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'maxResults', 'listDomains_maxResults' - The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain further pages of results. The default is 100; the
-- maximum allowed page size is also 100.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listDomains_nextToken :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Text)
listDomains_nextToken = Lens.lens (\ListDomains' {nextToken} -> nextToken) (\s@ListDomains' {} a -> s {nextToken = a} :: ListDomains)

-- | The maximum number of results that are returned per call. You can use
-- @NextToken@ to obtain further pages of results. The default is 100; the
-- maximum allowed page size is also 100.
listDomains_maxResults :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Natural)
listDomains_maxResults = Lens.lens (\ListDomains' {maxResults} -> maxResults) (\s@ListDomains' {} a -> s {maxResults = a} :: ListDomains)

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DomainSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomains

instance Prelude.NFData ListDomains

instance Core.ToHeaders ListDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("VoiceID.ListDomains" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDomains where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDomains where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | If @NextToken@ is returned, there are more results available. The value
    -- of @NextToken@ is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list containing details about each domain in the Amazon Web Services
    -- account.
    domainSummaries :: Prelude.Maybe [DomainSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDomainsResponse_nextToken' - If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
--
-- 'domainSummaries', 'listDomainsResponse_domainSummaries' - A list containing details about each domain in the Amazon Web Services
-- account.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
newListDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { nextToken = Prelude.Nothing,
      domainSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @NextToken@ is returned, there are more results available. The value
-- of @NextToken@ is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours.
listDomainsResponse_nextToken :: Lens.Lens' ListDomainsResponse (Prelude.Maybe Prelude.Text)
listDomainsResponse_nextToken = Lens.lens (\ListDomainsResponse' {nextToken} -> nextToken) (\s@ListDomainsResponse' {} a -> s {nextToken = a} :: ListDomainsResponse)

-- | A list containing details about each domain in the Amazon Web Services
-- account.
listDomainsResponse_domainSummaries :: Lens.Lens' ListDomainsResponse (Prelude.Maybe [DomainSummary])
listDomainsResponse_domainSummaries = Lens.lens (\ListDomainsResponse' {domainSummaries} -> domainSummaries) (\s@ListDomainsResponse' {} a -> s {domainSummaries = a} :: ListDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Prelude.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

instance Prelude.NFData ListDomainsResponse
