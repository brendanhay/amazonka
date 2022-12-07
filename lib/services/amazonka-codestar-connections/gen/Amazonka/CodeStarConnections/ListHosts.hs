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
-- Module      : Amazonka.CodeStarConnections.ListHosts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosts associated with your account.
module Amazonka.CodeStarConnections.ListHosts
  ( -- * Creating a Request
    ListHosts (..),
    newListHosts,

    -- * Request Lenses
    listHosts_nextToken,
    listHosts_maxResults,

    -- * Destructuring the Response
    ListHostsResponse (..),
    newListHostsResponse,

    -- * Response Lenses
    listHostsResponse_nextToken,
    listHostsResponse_hosts,
    listHostsResponse_httpStatus,
  )
where

import Amazonka.CodeStarConnections.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHosts' smart constructor.
data ListHosts = ListHosts'
  { -- | The token that was returned from the previous @ListHosts@ call, which
    -- can be used to return the next set of hosts in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHosts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHosts_nextToken' - The token that was returned from the previous @ListHosts@ call, which
-- can be used to return the next set of hosts in the list.
--
-- 'maxResults', 'listHosts_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newListHosts ::
  ListHosts
newListHosts =
  ListHosts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that was returned from the previous @ListHosts@ call, which
-- can be used to return the next set of hosts in the list.
listHosts_nextToken :: Lens.Lens' ListHosts (Prelude.Maybe Prelude.Text)
listHosts_nextToken = Lens.lens (\ListHosts' {nextToken} -> nextToken) (\s@ListHosts' {} a -> s {nextToken = a} :: ListHosts)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listHosts_maxResults :: Lens.Lens' ListHosts (Prelude.Maybe Prelude.Natural)
listHosts_maxResults = Lens.lens (\ListHosts' {maxResults} -> maxResults) (\s@ListHosts' {} a -> s {maxResults = a} :: ListHosts)

instance Core.AWSRequest ListHosts where
  type AWSResponse ListHosts = ListHostsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHostsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Hosts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHosts where
  hashWithSalt _salt ListHosts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListHosts where
  rnf ListHosts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListHosts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.ListHosts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHosts where
  toJSON ListHosts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListHosts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHosts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHostsResponse' smart constructor.
data ListHostsResponse = ListHostsResponse'
  { -- | A token that can be used in the next @ListHosts@ call. To view all items
    -- in the list, continue to call this operation with each subsequent token
    -- until no more @nextToken@ values are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of hosts and the details for each host, such as status, endpoint,
    -- and provider type.
    hosts :: Prelude.Maybe [Host],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHostsResponse_nextToken' - A token that can be used in the next @ListHosts@ call. To view all items
-- in the list, continue to call this operation with each subsequent token
-- until no more @nextToken@ values are returned.
--
-- 'hosts', 'listHostsResponse_hosts' - A list of hosts and the details for each host, such as status, endpoint,
-- and provider type.
--
-- 'httpStatus', 'listHostsResponse_httpStatus' - The response's http status code.
newListHostsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHostsResponse
newListHostsResponse pHttpStatus_ =
  ListHostsResponse'
    { nextToken = Prelude.Nothing,
      hosts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used in the next @ListHosts@ call. To view all items
-- in the list, continue to call this operation with each subsequent token
-- until no more @nextToken@ values are returned.
listHostsResponse_nextToken :: Lens.Lens' ListHostsResponse (Prelude.Maybe Prelude.Text)
listHostsResponse_nextToken = Lens.lens (\ListHostsResponse' {nextToken} -> nextToken) (\s@ListHostsResponse' {} a -> s {nextToken = a} :: ListHostsResponse)

-- | A list of hosts and the details for each host, such as status, endpoint,
-- and provider type.
listHostsResponse_hosts :: Lens.Lens' ListHostsResponse (Prelude.Maybe [Host])
listHostsResponse_hosts = Lens.lens (\ListHostsResponse' {hosts} -> hosts) (\s@ListHostsResponse' {} a -> s {hosts = a} :: ListHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listHostsResponse_httpStatus :: Lens.Lens' ListHostsResponse Prelude.Int
listHostsResponse_httpStatus = Lens.lens (\ListHostsResponse' {httpStatus} -> httpStatus) (\s@ListHostsResponse' {} a -> s {httpStatus = a} :: ListHostsResponse)

instance Prelude.NFData ListHostsResponse where
  rnf ListHostsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf hosts
      `Prelude.seq` Prelude.rnf httpStatus
