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
-- Module      : Amazonka.NetworkFirewall.ListFirewalls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for the firewalls that you have defined. If you
-- provide VPC identifiers in your request, this returns only the firewalls
-- for those VPCs.
--
-- Depending on your setting for max results and the number of firewalls, a
-- single call might not return the full list.
--
-- This operation returns paginated results.
module Amazonka.NetworkFirewall.ListFirewalls
  ( -- * Creating a Request
    ListFirewalls (..),
    newListFirewalls,

    -- * Request Lenses
    listFirewalls_maxResults,
    listFirewalls_nextToken,
    listFirewalls_vpcIds,

    -- * Destructuring the Response
    ListFirewallsResponse (..),
    newListFirewallsResponse,

    -- * Response Lenses
    listFirewallsResponse_firewalls,
    listFirewallsResponse_nextToken,
    listFirewallsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFirewalls' smart constructor.
data ListFirewalls = ListFirewalls'
  { -- | The maximum number of objects that you want Network Firewall to return
    -- for this request. If more objects are available, in the response,
    -- Network Firewall provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifiers of the VPCs that you want Network Firewall to
    -- retrieve the firewalls for. Leave this blank to retrieve all firewalls
    -- that you have defined.
    vpcIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewalls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFirewalls_maxResults' - The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- 'nextToken', 'listFirewalls_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'vpcIds', 'listFirewalls_vpcIds' - The unique identifiers of the VPCs that you want Network Firewall to
-- retrieve the firewalls for. Leave this blank to retrieve all firewalls
-- that you have defined.
newListFirewalls ::
  ListFirewalls
newListFirewalls =
  ListFirewalls'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcIds = Prelude.Nothing
    }

-- | The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listFirewalls_maxResults :: Lens.Lens' ListFirewalls (Prelude.Maybe Prelude.Natural)
listFirewalls_maxResults = Lens.lens (\ListFirewalls' {maxResults} -> maxResults) (\s@ListFirewalls' {} a -> s {maxResults = a} :: ListFirewalls)

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listFirewalls_nextToken :: Lens.Lens' ListFirewalls (Prelude.Maybe Prelude.Text)
listFirewalls_nextToken = Lens.lens (\ListFirewalls' {nextToken} -> nextToken) (\s@ListFirewalls' {} a -> s {nextToken = a} :: ListFirewalls)

-- | The unique identifiers of the VPCs that you want Network Firewall to
-- retrieve the firewalls for. Leave this blank to retrieve all firewalls
-- that you have defined.
listFirewalls_vpcIds :: Lens.Lens' ListFirewalls (Prelude.Maybe [Prelude.Text])
listFirewalls_vpcIds = Lens.lens (\ListFirewalls' {vpcIds} -> vpcIds) (\s@ListFirewalls' {} a -> s {vpcIds = a} :: ListFirewalls) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager ListFirewalls where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallsResponse_firewalls
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFirewalls_nextToken
          Lens..~ rs
          Lens.^? listFirewallsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFirewalls where
  type
    AWSResponse ListFirewalls =
      ListFirewallsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallsResponse'
            Prelude.<$> (x Data..?> "Firewalls" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewalls where
  hashWithSalt _salt ListFirewalls' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcIds

instance Prelude.NFData ListFirewalls where
  rnf ListFirewalls' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcIds

instance Data.ToHeaders ListFirewalls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.ListFirewalls" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFirewalls where
  toJSON ListFirewalls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("VpcIds" Data..=) Prelude.<$> vpcIds
          ]
      )

instance Data.ToPath ListFirewalls where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFirewalls where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallsResponse' smart constructor.
data ListFirewallsResponse = ListFirewallsResponse'
  { -- | The firewall metadata objects for the VPCs that you specified. Depending
    -- on your setting for max results and the number of firewalls you have, a
    -- single call might not be the full list.
    firewalls :: Prelude.Maybe [FirewallMetadata],
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewalls', 'listFirewallsResponse_firewalls' - The firewall metadata objects for the VPCs that you specified. Depending
-- on your setting for max results and the number of firewalls you have, a
-- single call might not be the full list.
--
-- 'nextToken', 'listFirewallsResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'httpStatus', 'listFirewallsResponse_httpStatus' - The response's http status code.
newListFirewallsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallsResponse
newListFirewallsResponse pHttpStatus_ =
  ListFirewallsResponse'
    { firewalls = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The firewall metadata objects for the VPCs that you specified. Depending
-- on your setting for max results and the number of firewalls you have, a
-- single call might not be the full list.
listFirewallsResponse_firewalls :: Lens.Lens' ListFirewallsResponse (Prelude.Maybe [FirewallMetadata])
listFirewallsResponse_firewalls = Lens.lens (\ListFirewallsResponse' {firewalls} -> firewalls) (\s@ListFirewallsResponse' {} a -> s {firewalls = a} :: ListFirewallsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listFirewallsResponse_nextToken :: Lens.Lens' ListFirewallsResponse (Prelude.Maybe Prelude.Text)
listFirewallsResponse_nextToken = Lens.lens (\ListFirewallsResponse' {nextToken} -> nextToken) (\s@ListFirewallsResponse' {} a -> s {nextToken = a} :: ListFirewallsResponse)

-- | The response's http status code.
listFirewallsResponse_httpStatus :: Lens.Lens' ListFirewallsResponse Prelude.Int
listFirewallsResponse_httpStatus = Lens.lens (\ListFirewallsResponse' {httpStatus} -> httpStatus) (\s@ListFirewallsResponse' {} a -> s {httpStatus = a} :: ListFirewallsResponse)

instance Prelude.NFData ListFirewallsResponse where
  rnf ListFirewallsResponse' {..} =
    Prelude.rnf firewalls
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
