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
-- Module      : Amazonka.Route53Resolver.ListFirewallDomainLists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the firewall domain lists that you have defined. For each
-- firewall domain list, you can retrieve the domains that are defined for
-- a list by calling ListFirewallDomains.
--
-- A single call to this list operation might return only a partial list of
-- the domain lists. For information, see @MaxResults@.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListFirewallDomainLists
  ( -- * Creating a Request
    ListFirewallDomainLists (..),
    newListFirewallDomainLists,

    -- * Request Lenses
    listFirewallDomainLists_maxResults,
    listFirewallDomainLists_nextToken,

    -- * Destructuring the Response
    ListFirewallDomainListsResponse (..),
    newListFirewallDomainListsResponse,

    -- * Response Lenses
    listFirewallDomainListsResponse_firewallDomainLists,
    listFirewallDomainListsResponse_nextToken,
    listFirewallDomainListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListFirewallDomainLists' smart constructor.
data ListFirewallDomainLists = ListFirewallDomainLists'
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
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallDomainLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFirewallDomainLists_maxResults' - The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
--
-- 'nextToken', 'listFirewallDomainLists_nextToken' - For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
newListFirewallDomainLists ::
  ListFirewallDomainLists
newListFirewallDomainLists =
  ListFirewallDomainLists'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
listFirewallDomainLists_maxResults :: Lens.Lens' ListFirewallDomainLists (Prelude.Maybe Prelude.Natural)
listFirewallDomainLists_maxResults = Lens.lens (\ListFirewallDomainLists' {maxResults} -> maxResults) (\s@ListFirewallDomainLists' {} a -> s {maxResults = a} :: ListFirewallDomainLists)

-- | For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
listFirewallDomainLists_nextToken :: Lens.Lens' ListFirewallDomainLists (Prelude.Maybe Prelude.Text)
listFirewallDomainLists_nextToken = Lens.lens (\ListFirewallDomainLists' {nextToken} -> nextToken) (\s@ListFirewallDomainLists' {} a -> s {nextToken = a} :: ListFirewallDomainLists)

instance Core.AWSPager ListFirewallDomainLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallDomainListsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallDomainListsResponse_firewallDomainLists
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFirewallDomainLists_nextToken
          Lens..~ rs
          Lens.^? listFirewallDomainListsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFirewallDomainLists where
  type
    AWSResponse ListFirewallDomainLists =
      ListFirewallDomainListsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallDomainListsResponse'
            Prelude.<$> ( x
                            Data..?> "FirewallDomainLists"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewallDomainLists where
  hashWithSalt _salt ListFirewallDomainLists' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFirewallDomainLists where
  rnf ListFirewallDomainLists' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListFirewallDomainLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListFirewallDomainLists" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFirewallDomainLists where
  toJSON ListFirewallDomainLists' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListFirewallDomainLists where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFirewallDomainLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallDomainListsResponse' smart constructor.
data ListFirewallDomainListsResponse = ListFirewallDomainListsResponse'
  { -- | A list of the domain lists that you have defined.
    --
    -- This might be a partial list of the domain lists that you\'ve defined.
    -- For information, see @MaxResults@.
    firewallDomainLists :: Prelude.Maybe [FirewallDomainListMetadata],
    -- | If objects are still available for retrieval, Resolver returns this
    -- token in the response. To retrieve the next batch of objects, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallDomainListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainLists', 'listFirewallDomainListsResponse_firewallDomainLists' - A list of the domain lists that you have defined.
--
-- This might be a partial list of the domain lists that you\'ve defined.
-- For information, see @MaxResults@.
--
-- 'nextToken', 'listFirewallDomainListsResponse_nextToken' - If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
--
-- 'httpStatus', 'listFirewallDomainListsResponse_httpStatus' - The response's http status code.
newListFirewallDomainListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallDomainListsResponse
newListFirewallDomainListsResponse pHttpStatus_ =
  ListFirewallDomainListsResponse'
    { firewallDomainLists =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the domain lists that you have defined.
--
-- This might be a partial list of the domain lists that you\'ve defined.
-- For information, see @MaxResults@.
listFirewallDomainListsResponse_firewallDomainLists :: Lens.Lens' ListFirewallDomainListsResponse (Prelude.Maybe [FirewallDomainListMetadata])
listFirewallDomainListsResponse_firewallDomainLists = Lens.lens (\ListFirewallDomainListsResponse' {firewallDomainLists} -> firewallDomainLists) (\s@ListFirewallDomainListsResponse' {} a -> s {firewallDomainLists = a} :: ListFirewallDomainListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
listFirewallDomainListsResponse_nextToken :: Lens.Lens' ListFirewallDomainListsResponse (Prelude.Maybe Prelude.Text)
listFirewallDomainListsResponse_nextToken = Lens.lens (\ListFirewallDomainListsResponse' {nextToken} -> nextToken) (\s@ListFirewallDomainListsResponse' {} a -> s {nextToken = a} :: ListFirewallDomainListsResponse)

-- | The response's http status code.
listFirewallDomainListsResponse_httpStatus :: Lens.Lens' ListFirewallDomainListsResponse Prelude.Int
listFirewallDomainListsResponse_httpStatus = Lens.lens (\ListFirewallDomainListsResponse' {httpStatus} -> httpStatus) (\s@ListFirewallDomainListsResponse' {} a -> s {httpStatus = a} :: ListFirewallDomainListsResponse)

instance
  Prelude.NFData
    ListFirewallDomainListsResponse
  where
  rnf ListFirewallDomainListsResponse' {..} =
    Prelude.rnf firewallDomainLists
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
