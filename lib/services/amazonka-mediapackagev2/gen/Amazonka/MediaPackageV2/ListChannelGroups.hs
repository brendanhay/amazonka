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
-- Module      : Amazonka.MediaPackageV2.ListChannelGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all channel groups that are configured in AWS Elemental
-- MediaPackage, including the channels and origin endpoints that are
-- associated with it.
--
-- This operation returns paginated results.
module Amazonka.MediaPackageV2.ListChannelGroups
  ( -- * Creating a Request
    ListChannelGroups (..),
    newListChannelGroups,

    -- * Request Lenses
    listChannelGroups_maxResults,
    listChannelGroups_nextToken,

    -- * Destructuring the Response
    ListChannelGroupsResponse (..),
    newListChannelGroupsResponse,

    -- * Response Lenses
    listChannelGroupsResponse_items,
    listChannelGroupsResponse_nextToken,
    listChannelGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannelGroups' smart constructor.
data ListChannelGroups = ListChannelGroups'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the GET list request. Use the token to fetch
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listChannelGroups_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listChannelGroups_nextToken' - The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
newListChannelGroups ::
  ListChannelGroups
newListChannelGroups =
  ListChannelGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in the response.
listChannelGroups_maxResults :: Lens.Lens' ListChannelGroups (Prelude.Maybe Prelude.Natural)
listChannelGroups_maxResults = Lens.lens (\ListChannelGroups' {maxResults} -> maxResults) (\s@ListChannelGroups' {} a -> s {maxResults = a} :: ListChannelGroups)

-- | The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
listChannelGroups_nextToken :: Lens.Lens' ListChannelGroups (Prelude.Maybe Prelude.Text)
listChannelGroups_nextToken = Lens.lens (\ListChannelGroups' {nextToken} -> nextToken) (\s@ListChannelGroups' {} a -> s {nextToken = a} :: ListChannelGroups)

instance Core.AWSPager ListChannelGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChannelGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listChannelGroupsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listChannelGroups_nextToken
          Lens..~ rs
          Lens.^? listChannelGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListChannelGroups where
  type
    AWSResponse ListChannelGroups =
      ListChannelGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelGroupsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannelGroups where
  hashWithSalt _salt ListChannelGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListChannelGroups where
  rnf ListChannelGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListChannelGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListChannelGroups where
  toPath = Prelude.const "/channelGroup"

instance Data.ToQuery ListChannelGroups where
  toQuery ListChannelGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListChannelGroupsResponse' smart constructor.
data ListChannelGroupsResponse = ListChannelGroupsResponse'
  { -- | The objects being returned.
    items :: Prelude.Maybe [ChannelGroupListConfiguration],
    -- | The pagination token from the GET list request. Use the token to fetch
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listChannelGroupsResponse_items' - The objects being returned.
--
-- 'nextToken', 'listChannelGroupsResponse_nextToken' - The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
--
-- 'httpStatus', 'listChannelGroupsResponse_httpStatus' - The response's http status code.
newListChannelGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelGroupsResponse
newListChannelGroupsResponse pHttpStatus_ =
  ListChannelGroupsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The objects being returned.
listChannelGroupsResponse_items :: Lens.Lens' ListChannelGroupsResponse (Prelude.Maybe [ChannelGroupListConfiguration])
listChannelGroupsResponse_items = Lens.lens (\ListChannelGroupsResponse' {items} -> items) (\s@ListChannelGroupsResponse' {} a -> s {items = a} :: ListChannelGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the GET list request. Use the token to fetch
-- the next page of results.
listChannelGroupsResponse_nextToken :: Lens.Lens' ListChannelGroupsResponse (Prelude.Maybe Prelude.Text)
listChannelGroupsResponse_nextToken = Lens.lens (\ListChannelGroupsResponse' {nextToken} -> nextToken) (\s@ListChannelGroupsResponse' {} a -> s {nextToken = a} :: ListChannelGroupsResponse)

-- | The response's http status code.
listChannelGroupsResponse_httpStatus :: Lens.Lens' ListChannelGroupsResponse Prelude.Int
listChannelGroupsResponse_httpStatus = Lens.lens (\ListChannelGroupsResponse' {httpStatus} -> httpStatus) (\s@ListChannelGroupsResponse' {} a -> s {httpStatus = a} :: ListChannelGroupsResponse)

instance Prelude.NFData ListChannelGroupsResponse where
  rnf ListChannelGroupsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
