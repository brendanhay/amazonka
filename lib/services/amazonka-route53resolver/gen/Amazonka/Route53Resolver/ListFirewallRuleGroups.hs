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
-- Module      : Amazonka.Route53Resolver.ListFirewallRuleGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the minimal high-level information for the rule groups that
-- you have defined.
--
-- A single call might return only a partial list of the rule groups. For
-- information, see @MaxResults@.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListFirewallRuleGroups
  ( -- * Creating a Request
    ListFirewallRuleGroups (..),
    newListFirewallRuleGroups,

    -- * Request Lenses
    listFirewallRuleGroups_maxResults,
    listFirewallRuleGroups_nextToken,

    -- * Destructuring the Response
    ListFirewallRuleGroupsResponse (..),
    newListFirewallRuleGroupsResponse,

    -- * Response Lenses
    listFirewallRuleGroupsResponse_firewallRuleGroups,
    listFirewallRuleGroupsResponse_nextToken,
    listFirewallRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListFirewallRuleGroups' smart constructor.
data ListFirewallRuleGroups = ListFirewallRuleGroups'
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
-- Create a value of 'ListFirewallRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFirewallRuleGroups_maxResults' - The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
--
-- 'nextToken', 'listFirewallRuleGroups_nextToken' - For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
newListFirewallRuleGroups ::
  ListFirewallRuleGroups
newListFirewallRuleGroups =
  ListFirewallRuleGroups'
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
listFirewallRuleGroups_maxResults :: Lens.Lens' ListFirewallRuleGroups (Prelude.Maybe Prelude.Natural)
listFirewallRuleGroups_maxResults = Lens.lens (\ListFirewallRuleGroups' {maxResults} -> maxResults) (\s@ListFirewallRuleGroups' {} a -> s {maxResults = a} :: ListFirewallRuleGroups)

-- | For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
listFirewallRuleGroups_nextToken :: Lens.Lens' ListFirewallRuleGroups (Prelude.Maybe Prelude.Text)
listFirewallRuleGroups_nextToken = Lens.lens (\ListFirewallRuleGroups' {nextToken} -> nextToken) (\s@ListFirewallRuleGroups' {} a -> s {nextToken = a} :: ListFirewallRuleGroups)

instance Core.AWSPager ListFirewallRuleGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallRuleGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallRuleGroupsResponse_firewallRuleGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listFirewallRuleGroups_nextToken
              Lens..~ rs
              Lens.^? listFirewallRuleGroupsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListFirewallRuleGroups where
  type
    AWSResponse ListFirewallRuleGroups =
      ListFirewallRuleGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallRuleGroupsResponse'
            Prelude.<$> ( x
                            Data..?> "FirewallRuleGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewallRuleGroups where
  hashWithSalt _salt ListFirewallRuleGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFirewallRuleGroups where
  rnf ListFirewallRuleGroups' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListFirewallRuleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListFirewallRuleGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFirewallRuleGroups where
  toJSON ListFirewallRuleGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListFirewallRuleGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFirewallRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallRuleGroupsResponse' smart constructor.
data ListFirewallRuleGroupsResponse = ListFirewallRuleGroupsResponse'
  { -- | A list of your firewall rule groups.
    --
    -- This might be a partial list of the rule groups that you have defined.
    -- For information, see @MaxResults@.
    firewallRuleGroups :: Prelude.Maybe [FirewallRuleGroupMetadata],
    -- | If objects are still available for retrieval, Resolver returns this
    -- token in the response. To retrieve the next batch of objects, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallRuleGroups', 'listFirewallRuleGroupsResponse_firewallRuleGroups' - A list of your firewall rule groups.
--
-- This might be a partial list of the rule groups that you have defined.
-- For information, see @MaxResults@.
--
-- 'nextToken', 'listFirewallRuleGroupsResponse_nextToken' - If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
--
-- 'httpStatus', 'listFirewallRuleGroupsResponse_httpStatus' - The response's http status code.
newListFirewallRuleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallRuleGroupsResponse
newListFirewallRuleGroupsResponse pHttpStatus_ =
  ListFirewallRuleGroupsResponse'
    { firewallRuleGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of your firewall rule groups.
--
-- This might be a partial list of the rule groups that you have defined.
-- For information, see @MaxResults@.
listFirewallRuleGroupsResponse_firewallRuleGroups :: Lens.Lens' ListFirewallRuleGroupsResponse (Prelude.Maybe [FirewallRuleGroupMetadata])
listFirewallRuleGroupsResponse_firewallRuleGroups = Lens.lens (\ListFirewallRuleGroupsResponse' {firewallRuleGroups} -> firewallRuleGroups) (\s@ListFirewallRuleGroupsResponse' {} a -> s {firewallRuleGroups = a} :: ListFirewallRuleGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
listFirewallRuleGroupsResponse_nextToken :: Lens.Lens' ListFirewallRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listFirewallRuleGroupsResponse_nextToken = Lens.lens (\ListFirewallRuleGroupsResponse' {nextToken} -> nextToken) (\s@ListFirewallRuleGroupsResponse' {} a -> s {nextToken = a} :: ListFirewallRuleGroupsResponse)

-- | The response's http status code.
listFirewallRuleGroupsResponse_httpStatus :: Lens.Lens' ListFirewallRuleGroupsResponse Prelude.Int
listFirewallRuleGroupsResponse_httpStatus = Lens.lens (\ListFirewallRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListFirewallRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListFirewallRuleGroupsResponse)

instance
  Prelude.NFData
    ListFirewallRuleGroupsResponse
  where
  rnf ListFirewallRuleGroupsResponse' {..} =
    Prelude.rnf firewallRuleGroups `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
