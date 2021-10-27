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
-- Module      : Network.AWS.CodeStarConnections.ListHosts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosts associated with your account.
module Network.AWS.CodeStarConnections.ListHosts
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
    listHostsResponse_hosts,
    listHostsResponse_nextToken,
    listHostsResponse_httpStatus,
  )
where

import Network.AWS.CodeStarConnections.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHostsResponse'
            Prelude.<$> (x Core..?> "Hosts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHosts

instance Prelude.NFData ListHosts

instance Core.ToHeaders ListHosts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.ListHosts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListHosts where
  toJSON ListHosts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListHosts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListHosts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHostsResponse' smart constructor.
data ListHostsResponse = ListHostsResponse'
  { -- | A list of hosts and the details for each host, such as status, endpoint,
    -- and provider type.
    hosts :: Prelude.Maybe [Host],
    -- | A token that can be used in the next @ListHosts@ call. To view all items
    -- in the list, continue to call this operation with each subsequent token
    -- until no more @nextToken@ values are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'hosts', 'listHostsResponse_hosts' - A list of hosts and the details for each host, such as status, endpoint,
-- and provider type.
--
-- 'nextToken', 'listHostsResponse_nextToken' - A token that can be used in the next @ListHosts@ call. To view all items
-- in the list, continue to call this operation with each subsequent token
-- until no more @nextToken@ values are returned.
--
-- 'httpStatus', 'listHostsResponse_httpStatus' - The response's http status code.
newListHostsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHostsResponse
newListHostsResponse pHttpStatus_ =
  ListHostsResponse'
    { hosts = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of hosts and the details for each host, such as status, endpoint,
-- and provider type.
listHostsResponse_hosts :: Lens.Lens' ListHostsResponse (Prelude.Maybe [Host])
listHostsResponse_hosts = Lens.lens (\ListHostsResponse' {hosts} -> hosts) (\s@ListHostsResponse' {} a -> s {hosts = a} :: ListHostsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used in the next @ListHosts@ call. To view all items
-- in the list, continue to call this operation with each subsequent token
-- until no more @nextToken@ values are returned.
listHostsResponse_nextToken :: Lens.Lens' ListHostsResponse (Prelude.Maybe Prelude.Text)
listHostsResponse_nextToken = Lens.lens (\ListHostsResponse' {nextToken} -> nextToken) (\s@ListHostsResponse' {} a -> s {nextToken = a} :: ListHostsResponse)

-- | The response's http status code.
listHostsResponse_httpStatus :: Lens.Lens' ListHostsResponse Prelude.Int
listHostsResponse_httpStatus = Lens.lens (\ListHostsResponse' {httpStatus} -> httpStatus) (\s@ListHostsResponse' {} a -> s {httpStatus = a} :: ListHostsResponse)

instance Prelude.NFData ListHostsResponse
