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
-- Module      : Amazonka.MigrationHubStrategy.ListServers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the servers.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubStrategy.ListServers
  ( -- * Creating a Request
    ListServers (..),
    newListServers,

    -- * Request Lenses
    listServers_filterValue,
    listServers_groupIdFilter,
    listServers_maxResults,
    listServers_nextToken,
    listServers_serverCriteria,
    listServers_sort,

    -- * Destructuring the Response
    ListServersResponse (..),
    newListServersResponse,

    -- * Response Lenses
    listServersResponse_nextToken,
    listServersResponse_serverInfos,
    listServersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServers' smart constructor.
data ListServers = ListServers'
  { -- | Specifies the filter value, which is based on the type of server
    -- criteria. For example, if @serverCriteria@ is @OS_NAME@, and the
    -- @filterValue@ is equal to @WindowsServer@, then @ListServers@ returns
    -- all of the servers matching the OS name @WindowsServer@.
    filterValue :: Prelude.Maybe Prelude.Text,
    -- | Specifies the group ID to filter on.
    groupIdFilter :: Prelude.Maybe [Group],
    -- | The maximum number of items to include in the response. The maximum
    -- value is 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token from a previous call that you use to retrieve the next set of
    -- results. For example, if a previous call to this action returned 100
    -- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
    -- results along with a token. You then use the returned token to retrieve
    -- the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Criteria for filtering servers.
    serverCriteria :: Prelude.Maybe ServerCriteria,
    -- | Specifies whether to sort by ascending (@ASC@) or descending (@DESC@)
    -- order.
    sort :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterValue', 'listServers_filterValue' - Specifies the filter value, which is based on the type of server
-- criteria. For example, if @serverCriteria@ is @OS_NAME@, and the
-- @filterValue@ is equal to @WindowsServer@, then @ListServers@ returns
-- all of the servers matching the OS name @WindowsServer@.
--
-- 'groupIdFilter', 'listServers_groupIdFilter' - Specifies the group ID to filter on.
--
-- 'maxResults', 'listServers_maxResults' - The maximum number of items to include in the response. The maximum
-- value is 100.
--
-- 'nextToken', 'listServers_nextToken' - The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
--
-- 'serverCriteria', 'listServers_serverCriteria' - Criteria for filtering servers.
--
-- 'sort', 'listServers_sort' - Specifies whether to sort by ascending (@ASC@) or descending (@DESC@)
-- order.
newListServers ::
  ListServers
newListServers =
  ListServers'
    { filterValue = Prelude.Nothing,
      groupIdFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serverCriteria = Prelude.Nothing,
      sort = Prelude.Nothing
    }

-- | Specifies the filter value, which is based on the type of server
-- criteria. For example, if @serverCriteria@ is @OS_NAME@, and the
-- @filterValue@ is equal to @WindowsServer@, then @ListServers@ returns
-- all of the servers matching the OS name @WindowsServer@.
listServers_filterValue :: Lens.Lens' ListServers (Prelude.Maybe Prelude.Text)
listServers_filterValue = Lens.lens (\ListServers' {filterValue} -> filterValue) (\s@ListServers' {} a -> s {filterValue = a} :: ListServers)

-- | Specifies the group ID to filter on.
listServers_groupIdFilter :: Lens.Lens' ListServers (Prelude.Maybe [Group])
listServers_groupIdFilter = Lens.lens (\ListServers' {groupIdFilter} -> groupIdFilter) (\s@ListServers' {} a -> s {groupIdFilter = a} :: ListServers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to include in the response. The maximum
-- value is 100.
listServers_maxResults :: Lens.Lens' ListServers (Prelude.Maybe Prelude.Int)
listServers_maxResults = Lens.lens (\ListServers' {maxResults} -> maxResults) (\s@ListServers' {} a -> s {maxResults = a} :: ListServers)

-- | The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
listServers_nextToken :: Lens.Lens' ListServers (Prelude.Maybe Prelude.Text)
listServers_nextToken = Lens.lens (\ListServers' {nextToken} -> nextToken) (\s@ListServers' {} a -> s {nextToken = a} :: ListServers)

-- | Criteria for filtering servers.
listServers_serverCriteria :: Lens.Lens' ListServers (Prelude.Maybe ServerCriteria)
listServers_serverCriteria = Lens.lens (\ListServers' {serverCriteria} -> serverCriteria) (\s@ListServers' {} a -> s {serverCriteria = a} :: ListServers)

-- | Specifies whether to sort by ascending (@ASC@) or descending (@DESC@)
-- order.
listServers_sort :: Lens.Lens' ListServers (Prelude.Maybe SortOrder)
listServers_sort = Lens.lens (\ListServers' {sort} -> sort) (\s@ListServers' {} a -> s {sort = a} :: ListServers)

instance Core.AWSPager ListServers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServersResponse_serverInfos Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServers_nextToken
          Lens..~ rs
          Lens.^? listServersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListServers where
  type AWSResponse ListServers = ListServersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "serverInfos" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServers where
  hashWithSalt _salt ListServers' {..} =
    _salt `Prelude.hashWithSalt` filterValue
      `Prelude.hashWithSalt` groupIdFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serverCriteria
      `Prelude.hashWithSalt` sort

instance Prelude.NFData ListServers where
  rnf ListServers' {..} =
    Prelude.rnf filterValue
      `Prelude.seq` Prelude.rnf groupIdFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serverCriteria
      `Prelude.seq` Prelude.rnf sort

instance Data.ToHeaders ListServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServers where
  toJSON ListServers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterValue" Data..=) Prelude.<$> filterValue,
            ("groupIdFilter" Data..=) Prelude.<$> groupIdFilter,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("serverCriteria" Data..=)
              Prelude.<$> serverCriteria,
            ("sort" Data..=) Prelude.<$> sort
          ]
      )

instance Data.ToPath ListServers where
  toPath = Prelude.const "/list-servers"

instance Data.ToQuery ListServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServersResponse' smart constructor.
data ListServersResponse = ListServersResponse'
  { -- | The token you use to retrieve the next set of results, or null if there
    -- are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of servers with detailed information about each server.
    serverInfos :: Prelude.Maybe [ServerDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServersResponse_nextToken' - The token you use to retrieve the next set of results, or null if there
-- are no more results.
--
-- 'serverInfos', 'listServersResponse_serverInfos' - The list of servers with detailed information about each server.
--
-- 'httpStatus', 'listServersResponse_httpStatus' - The response's http status code.
newListServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServersResponse
newListServersResponse pHttpStatus_ =
  ListServersResponse'
    { nextToken = Prelude.Nothing,
      serverInfos = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token you use to retrieve the next set of results, or null if there
-- are no more results.
listServersResponse_nextToken :: Lens.Lens' ListServersResponse (Prelude.Maybe Prelude.Text)
listServersResponse_nextToken = Lens.lens (\ListServersResponse' {nextToken} -> nextToken) (\s@ListServersResponse' {} a -> s {nextToken = a} :: ListServersResponse)

-- | The list of servers with detailed information about each server.
listServersResponse_serverInfos :: Lens.Lens' ListServersResponse (Prelude.Maybe [ServerDetail])
listServersResponse_serverInfos = Lens.lens (\ListServersResponse' {serverInfos} -> serverInfos) (\s@ListServersResponse' {} a -> s {serverInfos = a} :: ListServersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listServersResponse_httpStatus :: Lens.Lens' ListServersResponse Prelude.Int
listServersResponse_httpStatus = Lens.lens (\ListServersResponse' {httpStatus} -> httpStatus) (\s@ListServersResponse' {} a -> s {httpStatus = a} :: ListServersResponse)

instance Prelude.NFData ListServersResponse where
  rnf ListServersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serverInfos
      `Prelude.seq` Prelude.rnf httpStatus
