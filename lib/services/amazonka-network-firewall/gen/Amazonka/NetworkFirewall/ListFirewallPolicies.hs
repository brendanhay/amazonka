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
-- Module      : Amazonka.NetworkFirewall.ListFirewallPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for the firewall policies that you have defined.
-- Depending on your setting for max results and the number of firewall
-- policies, a single call might not return the full list.
--
-- This operation returns paginated results.
module Amazonka.NetworkFirewall.ListFirewallPolicies
  ( -- * Creating a Request
    ListFirewallPolicies (..),
    newListFirewallPolicies,

    -- * Request Lenses
    listFirewallPolicies_nextToken,
    listFirewallPolicies_maxResults,

    -- * Destructuring the Response
    ListFirewallPoliciesResponse (..),
    newListFirewallPoliciesResponse,

    -- * Response Lenses
    listFirewallPoliciesResponse_nextToken,
    listFirewallPoliciesResponse_firewallPolicies,
    listFirewallPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFirewallPolicies' smart constructor.
data ListFirewallPolicies = ListFirewallPolicies'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Network Firewall to return
    -- for this request. If more objects are available, in the response,
    -- Network Firewall provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFirewallPolicies_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'maxResults', 'listFirewallPolicies_maxResults' - The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
newListFirewallPolicies ::
  ListFirewallPolicies
newListFirewallPolicies =
  ListFirewallPolicies'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listFirewallPolicies_nextToken :: Lens.Lens' ListFirewallPolicies (Prelude.Maybe Prelude.Text)
listFirewallPolicies_nextToken = Lens.lens (\ListFirewallPolicies' {nextToken} -> nextToken) (\s@ListFirewallPolicies' {} a -> s {nextToken = a} :: ListFirewallPolicies)

-- | The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listFirewallPolicies_maxResults :: Lens.Lens' ListFirewallPolicies (Prelude.Maybe Prelude.Natural)
listFirewallPolicies_maxResults = Lens.lens (\ListFirewallPolicies' {maxResults} -> maxResults) (\s@ListFirewallPolicies' {} a -> s {maxResults = a} :: ListFirewallPolicies)

instance Core.AWSPager ListFirewallPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallPoliciesResponse_firewallPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFirewallPolicies_nextToken
          Lens..~ rs
          Lens.^? listFirewallPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFirewallPolicies where
  type
    AWSResponse ListFirewallPolicies =
      ListFirewallPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallPoliciesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "FirewallPolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewallPolicies where
  hashWithSalt _salt ListFirewallPolicies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFirewallPolicies where
  rnf ListFirewallPolicies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListFirewallPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.ListFirewallPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFirewallPolicies where
  toJSON ListFirewallPolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListFirewallPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFirewallPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallPoliciesResponse' smart constructor.
data ListFirewallPoliciesResponse = ListFirewallPoliciesResponse'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The metadata for the firewall policies. Depending on your setting for
    -- max results and the number of firewall policies that you have, this
    -- might not be the full list.
    firewallPolicies :: Prelude.Maybe [FirewallPolicyMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFirewallPoliciesResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'firewallPolicies', 'listFirewallPoliciesResponse_firewallPolicies' - The metadata for the firewall policies. Depending on your setting for
-- max results and the number of firewall policies that you have, this
-- might not be the full list.
--
-- 'httpStatus', 'listFirewallPoliciesResponse_httpStatus' - The response's http status code.
newListFirewallPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallPoliciesResponse
newListFirewallPoliciesResponse pHttpStatus_ =
  ListFirewallPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      firewallPolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listFirewallPoliciesResponse_nextToken :: Lens.Lens' ListFirewallPoliciesResponse (Prelude.Maybe Prelude.Text)
listFirewallPoliciesResponse_nextToken = Lens.lens (\ListFirewallPoliciesResponse' {nextToken} -> nextToken) (\s@ListFirewallPoliciesResponse' {} a -> s {nextToken = a} :: ListFirewallPoliciesResponse)

-- | The metadata for the firewall policies. Depending on your setting for
-- max results and the number of firewall policies that you have, this
-- might not be the full list.
listFirewallPoliciesResponse_firewallPolicies :: Lens.Lens' ListFirewallPoliciesResponse (Prelude.Maybe [FirewallPolicyMetadata])
listFirewallPoliciesResponse_firewallPolicies = Lens.lens (\ListFirewallPoliciesResponse' {firewallPolicies} -> firewallPolicies) (\s@ListFirewallPoliciesResponse' {} a -> s {firewallPolicies = a} :: ListFirewallPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFirewallPoliciesResponse_httpStatus :: Lens.Lens' ListFirewallPoliciesResponse Prelude.Int
listFirewallPoliciesResponse_httpStatus = Lens.lens (\ListFirewallPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListFirewallPoliciesResponse' {} a -> s {httpStatus = a} :: ListFirewallPoliciesResponse)

instance Prelude.NFData ListFirewallPoliciesResponse where
  rnf ListFirewallPoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf firewallPolicies
      `Prelude.seq` Prelude.rnf httpStatus
