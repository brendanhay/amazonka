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
-- Module      : Amazonka.Route53Resolver.ListFirewallDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the domains that you have defined for the specified firewall
-- domain list.
--
-- A single call might return only a partial list of the domains. For
-- information, see @MaxResults@.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListFirewallDomains
  ( -- * Creating a Request
    ListFirewallDomains (..),
    newListFirewallDomains,

    -- * Request Lenses
    listFirewallDomains_maxResults,
    listFirewallDomains_nextToken,
    listFirewallDomains_firewallDomainListId,

    -- * Destructuring the Response
    ListFirewallDomainsResponse (..),
    newListFirewallDomainsResponse,

    -- * Response Lenses
    listFirewallDomainsResponse_domains,
    listFirewallDomainsResponse_nextToken,
    listFirewallDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListFirewallDomains' smart constructor.
data ListFirewallDomains = ListFirewallDomains'
  { -- | The maximum number of objects that you want Resolver to return for this
    -- request. If more objects are available, in the response, Resolver
    -- provides a @NextToken@ value that you can use in a subsequent call to
    -- get the next batch of objects.
    --
    -- If you don\'t specify a value for @MaxResults@, Resolver returns up to
    -- 100 objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first call to this list request, omit this value.
    --
    -- When you request a list of objects, Resolver returns at most the number
    -- of objects specified in @MaxResults@. If more objects are available for
    -- retrieval, Resolver returns a @NextToken@ value in the response. To
    -- retrieve the next batch of objects, use the token that was returned for
    -- the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the domain list whose domains you want to retrieve.
    firewallDomainListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFirewallDomains_maxResults' - The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
--
-- 'nextToken', 'listFirewallDomains_nextToken' - For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
--
-- 'firewallDomainListId', 'listFirewallDomains_firewallDomainListId' - The ID of the domain list whose domains you want to retrieve.
newListFirewallDomains ::
  -- | 'firewallDomainListId'
  Prelude.Text ->
  ListFirewallDomains
newListFirewallDomains pFirewallDomainListId_ =
  ListFirewallDomains'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      firewallDomainListId = pFirewallDomainListId_
    }

-- | The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
listFirewallDomains_maxResults :: Lens.Lens' ListFirewallDomains (Prelude.Maybe Prelude.Natural)
listFirewallDomains_maxResults = Lens.lens (\ListFirewallDomains' {maxResults} -> maxResults) (\s@ListFirewallDomains' {} a -> s {maxResults = a} :: ListFirewallDomains)

-- | For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
listFirewallDomains_nextToken :: Lens.Lens' ListFirewallDomains (Prelude.Maybe Prelude.Text)
listFirewallDomains_nextToken = Lens.lens (\ListFirewallDomains' {nextToken} -> nextToken) (\s@ListFirewallDomains' {} a -> s {nextToken = a} :: ListFirewallDomains)

-- | The ID of the domain list whose domains you want to retrieve.
listFirewallDomains_firewallDomainListId :: Lens.Lens' ListFirewallDomains Prelude.Text
listFirewallDomains_firewallDomainListId = Lens.lens (\ListFirewallDomains' {firewallDomainListId} -> firewallDomainListId) (\s@ListFirewallDomains' {} a -> s {firewallDomainListId = a} :: ListFirewallDomains)

instance Core.AWSPager ListFirewallDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallDomainsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallDomainsResponse_domains
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFirewallDomains_nextToken
          Lens..~ rs
          Lens.^? listFirewallDomainsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFirewallDomains where
  type
    AWSResponse ListFirewallDomains =
      ListFirewallDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallDomainsResponse'
            Prelude.<$> (x Data..?> "Domains" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewallDomains where
  hashWithSalt _salt ListFirewallDomains' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` firewallDomainListId

instance Prelude.NFData ListFirewallDomains where
  rnf ListFirewallDomains' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf firewallDomainListId

instance Data.ToHeaders ListFirewallDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListFirewallDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFirewallDomains where
  toJSON ListFirewallDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "FirewallDomainListId"
                  Data..= firewallDomainListId
              )
          ]
      )

instance Data.ToPath ListFirewallDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFirewallDomains where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallDomainsResponse' smart constructor.
data ListFirewallDomainsResponse = ListFirewallDomainsResponse'
  { -- | A list of the domains in the firewall domain list.
    --
    -- This might be a partial list of the domains that you\'ve defined in the
    -- domain list. For information, see @MaxResults@.
    domains :: Prelude.Maybe [Prelude.Text],
    -- | If objects are still available for retrieval, Resolver returns this
    -- token in the response. To retrieve the next batch of objects, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domains', 'listFirewallDomainsResponse_domains' - A list of the domains in the firewall domain list.
--
-- This might be a partial list of the domains that you\'ve defined in the
-- domain list. For information, see @MaxResults@.
--
-- 'nextToken', 'listFirewallDomainsResponse_nextToken' - If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
--
-- 'httpStatus', 'listFirewallDomainsResponse_httpStatus' - The response's http status code.
newListFirewallDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallDomainsResponse
newListFirewallDomainsResponse pHttpStatus_ =
  ListFirewallDomainsResponse'
    { domains =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the domains in the firewall domain list.
--
-- This might be a partial list of the domains that you\'ve defined in the
-- domain list. For information, see @MaxResults@.
listFirewallDomainsResponse_domains :: Lens.Lens' ListFirewallDomainsResponse (Prelude.Maybe [Prelude.Text])
listFirewallDomainsResponse_domains = Lens.lens (\ListFirewallDomainsResponse' {domains} -> domains) (\s@ListFirewallDomainsResponse' {} a -> s {domains = a} :: ListFirewallDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
listFirewallDomainsResponse_nextToken :: Lens.Lens' ListFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
listFirewallDomainsResponse_nextToken = Lens.lens (\ListFirewallDomainsResponse' {nextToken} -> nextToken) (\s@ListFirewallDomainsResponse' {} a -> s {nextToken = a} :: ListFirewallDomainsResponse)

-- | The response's http status code.
listFirewallDomainsResponse_httpStatus :: Lens.Lens' ListFirewallDomainsResponse Prelude.Int
listFirewallDomainsResponse_httpStatus = Lens.lens (\ListFirewallDomainsResponse' {httpStatus} -> httpStatus) (\s@ListFirewallDomainsResponse' {} a -> s {httpStatus = a} :: ListFirewallDomainsResponse)

instance Prelude.NFData ListFirewallDomainsResponse where
  rnf ListFirewallDomainsResponse' {..} =
    Prelude.rnf domains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
