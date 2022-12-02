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
-- Module      : Amazonka.Route53Resolver.ListFirewallConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the firewall configurations that you have defined. DNS
-- Firewall uses the configurations to manage firewall behavior for your
-- VPCs.
--
-- A single call might return only a partial list of the configurations.
-- For information, see @MaxResults@.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListFirewallConfigs
  ( -- * Creating a Request
    ListFirewallConfigs (..),
    newListFirewallConfigs,

    -- * Request Lenses
    listFirewallConfigs_nextToken,
    listFirewallConfigs_maxResults,

    -- * Destructuring the Response
    ListFirewallConfigsResponse (..),
    newListFirewallConfigsResponse,

    -- * Response Lenses
    listFirewallConfigsResponse_nextToken,
    listFirewallConfigsResponse_firewallConfigs,
    listFirewallConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListFirewallConfigs' smart constructor.
data ListFirewallConfigs = ListFirewallConfigs'
  { -- | For the first call to this list request, omit this value.
    --
    -- When you request a list of objects, Resolver returns at most the number
    -- of objects specified in @MaxResults@. If more objects are available for
    -- retrieval, Resolver returns a @NextToken@ value in the response. To
    -- retrieve the next batch of objects, use the token that was returned for
    -- the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects that you want Resolver to return for this
    -- request. If more objects are available, in the response, Resolver
    -- provides a @NextToken@ value that you can use in a subsequent call to
    -- get the next batch of objects.
    --
    -- If you don\'t specify a value for @MaxResults@, Resolver returns up to
    -- 100 objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFirewallConfigs_nextToken' - For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
--
-- 'maxResults', 'listFirewallConfigs_maxResults' - The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
newListFirewallConfigs ::
  ListFirewallConfigs
newListFirewallConfigs =
  ListFirewallConfigs'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | For the first call to this list request, omit this value.
--
-- When you request a list of objects, Resolver returns at most the number
-- of objects specified in @MaxResults@. If more objects are available for
-- retrieval, Resolver returns a @NextToken@ value in the response. To
-- retrieve the next batch of objects, use the token that was returned for
-- the prior request in your next request.
listFirewallConfigs_nextToken :: Lens.Lens' ListFirewallConfigs (Prelude.Maybe Prelude.Text)
listFirewallConfigs_nextToken = Lens.lens (\ListFirewallConfigs' {nextToken} -> nextToken) (\s@ListFirewallConfigs' {} a -> s {nextToken = a} :: ListFirewallConfigs)

-- | The maximum number of objects that you want Resolver to return for this
-- request. If more objects are available, in the response, Resolver
-- provides a @NextToken@ value that you can use in a subsequent call to
-- get the next batch of objects.
--
-- If you don\'t specify a value for @MaxResults@, Resolver returns up to
-- 100 objects.
listFirewallConfigs_maxResults :: Lens.Lens' ListFirewallConfigs (Prelude.Maybe Prelude.Natural)
listFirewallConfigs_maxResults = Lens.lens (\ListFirewallConfigs' {maxResults} -> maxResults) (\s@ListFirewallConfigs' {} a -> s {maxResults = a} :: ListFirewallConfigs)

instance Core.AWSPager ListFirewallConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFirewallConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFirewallConfigsResponse_firewallConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFirewallConfigs_nextToken
          Lens..~ rs
          Lens.^? listFirewallConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFirewallConfigs where
  type
    AWSResponse ListFirewallConfigs =
      ListFirewallConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFirewallConfigsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "FirewallConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFirewallConfigs where
  hashWithSalt _salt ListFirewallConfigs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListFirewallConfigs where
  rnf ListFirewallConfigs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListFirewallConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListFirewallConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFirewallConfigs where
  toJSON ListFirewallConfigs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListFirewallConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFirewallConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFirewallConfigsResponse' smart constructor.
data ListFirewallConfigsResponse = ListFirewallConfigsResponse'
  { -- | If objects are still available for retrieval, Resolver returns this
    -- token in the response. To retrieve the next batch of objects, provide
    -- this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The configurations for the firewall behavior provided by DNS Firewall
    -- for VPCs from Amazon Virtual Private Cloud (Amazon VPC).
    firewallConfigs :: Prelude.Maybe [FirewallConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFirewallConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFirewallConfigsResponse_nextToken' - If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
--
-- 'firewallConfigs', 'listFirewallConfigsResponse_firewallConfigs' - The configurations for the firewall behavior provided by DNS Firewall
-- for VPCs from Amazon Virtual Private Cloud (Amazon VPC).
--
-- 'httpStatus', 'listFirewallConfigsResponse_httpStatus' - The response's http status code.
newListFirewallConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFirewallConfigsResponse
newListFirewallConfigsResponse pHttpStatus_ =
  ListFirewallConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      firewallConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If objects are still available for retrieval, Resolver returns this
-- token in the response. To retrieve the next batch of objects, provide
-- this token in your next request.
listFirewallConfigsResponse_nextToken :: Lens.Lens' ListFirewallConfigsResponse (Prelude.Maybe Prelude.Text)
listFirewallConfigsResponse_nextToken = Lens.lens (\ListFirewallConfigsResponse' {nextToken} -> nextToken) (\s@ListFirewallConfigsResponse' {} a -> s {nextToken = a} :: ListFirewallConfigsResponse)

-- | The configurations for the firewall behavior provided by DNS Firewall
-- for VPCs from Amazon Virtual Private Cloud (Amazon VPC).
listFirewallConfigsResponse_firewallConfigs :: Lens.Lens' ListFirewallConfigsResponse (Prelude.Maybe [FirewallConfig])
listFirewallConfigsResponse_firewallConfigs = Lens.lens (\ListFirewallConfigsResponse' {firewallConfigs} -> firewallConfigs) (\s@ListFirewallConfigsResponse' {} a -> s {firewallConfigs = a} :: ListFirewallConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFirewallConfigsResponse_httpStatus :: Lens.Lens' ListFirewallConfigsResponse Prelude.Int
listFirewallConfigsResponse_httpStatus = Lens.lens (\ListFirewallConfigsResponse' {httpStatus} -> httpStatus) (\s@ListFirewallConfigsResponse' {} a -> s {httpStatus = a} :: ListFirewallConfigsResponse)

instance Prelude.NFData ListFirewallConfigsResponse where
  rnf ListFirewallConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf firewallConfigs
      `Prelude.seq` Prelude.rnf httpStatus
