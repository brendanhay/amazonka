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
-- Module      : Amazonka.GroundStation.ListDataflowEndpointGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DataflowEndpoint@ groups.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListDataflowEndpointGroups
  ( -- * Creating a Request
    ListDataflowEndpointGroups (..),
    newListDataflowEndpointGroups,

    -- * Request Lenses
    listDataflowEndpointGroups_maxResults,
    listDataflowEndpointGroups_nextToken,

    -- * Destructuring the Response
    ListDataflowEndpointGroupsResponse (..),
    newListDataflowEndpointGroupsResponse,

    -- * Response Lenses
    listDataflowEndpointGroupsResponse_dataflowEndpointGroupList,
    listDataflowEndpointGroupsResponse_nextToken,
    listDataflowEndpointGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListDataflowEndpointGroups' smart constructor.
data ListDataflowEndpointGroups = ListDataflowEndpointGroups'
  { -- | Maximum number of dataflow endpoint groups returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token returned in the request of a previous
    -- @ListDataflowEndpointGroups@ call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataflowEndpointGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataflowEndpointGroups_maxResults' - Maximum number of dataflow endpoint groups returned.
--
-- 'nextToken', 'listDataflowEndpointGroups_nextToken' - Next token returned in the request of a previous
-- @ListDataflowEndpointGroups@ call. Used to get the next page of results.
newListDataflowEndpointGroups ::
  ListDataflowEndpointGroups
newListDataflowEndpointGroups =
  ListDataflowEndpointGroups'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of dataflow endpoint groups returned.
listDataflowEndpointGroups_maxResults :: Lens.Lens' ListDataflowEndpointGroups (Prelude.Maybe Prelude.Natural)
listDataflowEndpointGroups_maxResults = Lens.lens (\ListDataflowEndpointGroups' {maxResults} -> maxResults) (\s@ListDataflowEndpointGroups' {} a -> s {maxResults = a} :: ListDataflowEndpointGroups)

-- | Next token returned in the request of a previous
-- @ListDataflowEndpointGroups@ call. Used to get the next page of results.
listDataflowEndpointGroups_nextToken :: Lens.Lens' ListDataflowEndpointGroups (Prelude.Maybe Prelude.Text)
listDataflowEndpointGroups_nextToken = Lens.lens (\ListDataflowEndpointGroups' {nextToken} -> nextToken) (\s@ListDataflowEndpointGroups' {} a -> s {nextToken = a} :: ListDataflowEndpointGroups)

instance Core.AWSPager ListDataflowEndpointGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataflowEndpointGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataflowEndpointGroupsResponse_dataflowEndpointGroupList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDataflowEndpointGroups_nextToken
          Lens..~ rs
          Lens.^? listDataflowEndpointGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDataflowEndpointGroups where
  type
    AWSResponse ListDataflowEndpointGroups =
      ListDataflowEndpointGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataflowEndpointGroupsResponse'
            Prelude.<$> ( x
                            Data..?> "dataflowEndpointGroupList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataflowEndpointGroups where
  hashWithSalt _salt ListDataflowEndpointGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDataflowEndpointGroups where
  rnf ListDataflowEndpointGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDataflowEndpointGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataflowEndpointGroups where
  toPath = Prelude.const "/dataflowEndpointGroup"

instance Data.ToQuery ListDataflowEndpointGroups where
  toQuery ListDataflowEndpointGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- |
--
-- /See:/ 'newListDataflowEndpointGroupsResponse' smart constructor.
data ListDataflowEndpointGroupsResponse = ListDataflowEndpointGroupsResponse'
  { -- | A list of dataflow endpoint groups.
    dataflowEndpointGroupList :: Prelude.Maybe [DataflowEndpointListItem],
    -- | Next token returned in the response of a previous
    -- @ListDataflowEndpointGroups@ call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataflowEndpointGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataflowEndpointGroupList', 'listDataflowEndpointGroupsResponse_dataflowEndpointGroupList' - A list of dataflow endpoint groups.
--
-- 'nextToken', 'listDataflowEndpointGroupsResponse_nextToken' - Next token returned in the response of a previous
-- @ListDataflowEndpointGroups@ call. Used to get the next page of results.
--
-- 'httpStatus', 'listDataflowEndpointGroupsResponse_httpStatus' - The response's http status code.
newListDataflowEndpointGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataflowEndpointGroupsResponse
newListDataflowEndpointGroupsResponse pHttpStatus_ =
  ListDataflowEndpointGroupsResponse'
    { dataflowEndpointGroupList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of dataflow endpoint groups.
listDataflowEndpointGroupsResponse_dataflowEndpointGroupList :: Lens.Lens' ListDataflowEndpointGroupsResponse (Prelude.Maybe [DataflowEndpointListItem])
listDataflowEndpointGroupsResponse_dataflowEndpointGroupList = Lens.lens (\ListDataflowEndpointGroupsResponse' {dataflowEndpointGroupList} -> dataflowEndpointGroupList) (\s@ListDataflowEndpointGroupsResponse' {} a -> s {dataflowEndpointGroupList = a} :: ListDataflowEndpointGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next token returned in the response of a previous
-- @ListDataflowEndpointGroups@ call. Used to get the next page of results.
listDataflowEndpointGroupsResponse_nextToken :: Lens.Lens' ListDataflowEndpointGroupsResponse (Prelude.Maybe Prelude.Text)
listDataflowEndpointGroupsResponse_nextToken = Lens.lens (\ListDataflowEndpointGroupsResponse' {nextToken} -> nextToken) (\s@ListDataflowEndpointGroupsResponse' {} a -> s {nextToken = a} :: ListDataflowEndpointGroupsResponse)

-- | The response's http status code.
listDataflowEndpointGroupsResponse_httpStatus :: Lens.Lens' ListDataflowEndpointGroupsResponse Prelude.Int
listDataflowEndpointGroupsResponse_httpStatus = Lens.lens (\ListDataflowEndpointGroupsResponse' {httpStatus} -> httpStatus) (\s@ListDataflowEndpointGroupsResponse' {} a -> s {httpStatus = a} :: ListDataflowEndpointGroupsResponse)

instance
  Prelude.NFData
    ListDataflowEndpointGroupsResponse
  where
  rnf ListDataflowEndpointGroupsResponse' {..} =
    Prelude.rnf dataflowEndpointGroupList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
