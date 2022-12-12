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
-- Module      : Amazonka.ArcZonalShift.ListManagedResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the resources in your AWS account in this AWS Region that are
-- managed for zonal shifts in Amazon Route 53 Application Recovery
-- Controller, and information about them. The information includes their
-- Amazon Resource Names (ARNs), the Availability Zones the resources are
-- deployed in, and the resource name.
--
-- This operation returns paginated results.
module Amazonka.ArcZonalShift.ListManagedResources
  ( -- * Creating a Request
    ListManagedResources (..),
    newListManagedResources,

    -- * Request Lenses
    listManagedResources_maxResults,
    listManagedResources_nextToken,

    -- * Destructuring the Response
    ListManagedResourcesResponse (..),
    newListManagedResourcesResponse,

    -- * Response Lenses
    listManagedResourcesResponse_nextToken,
    listManagedResourcesResponse_httpStatus,
    listManagedResourcesResponse_items,
  )
where

import Amazonka.ArcZonalShift.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedResources' smart constructor.
data ListManagedResources = ListManagedResources'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listManagedResources_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listManagedResources_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
newListManagedResources ::
  ListManagedResources
newListManagedResources =
  ListManagedResources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listManagedResources_maxResults :: Lens.Lens' ListManagedResources (Prelude.Maybe Prelude.Natural)
listManagedResources_maxResults = Lens.lens (\ListManagedResources' {maxResults} -> maxResults) (\s@ListManagedResources' {} a -> s {maxResults = a} :: ListManagedResources)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listManagedResources_nextToken :: Lens.Lens' ListManagedResources (Prelude.Maybe Prelude.Text)
listManagedResources_nextToken = Lens.lens (\ListManagedResources' {nextToken} -> nextToken) (\s@ListManagedResources' {} a -> s {nextToken = a} :: ListManagedResources)

instance Core.AWSPager ListManagedResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listManagedResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listManagedResourcesResponse_items) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listManagedResources_nextToken
          Lens..~ rs
          Lens.^? listManagedResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListManagedResources where
  type
    AWSResponse ListManagedResources =
      ListManagedResourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListManagedResources where
  hashWithSalt _salt ListManagedResources' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListManagedResources where
  rnf ListManagedResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListManagedResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListManagedResources where
  toPath = Prelude.const "/managedresources"

instance Data.ToQuery ListManagedResources where
  toQuery ListManagedResources' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListManagedResourcesResponse' smart constructor.
data ListManagedResourcesResponse = ListManagedResourcesResponse'
  { -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The items in the response list.
    items :: [ManagedResourceSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedResourcesResponse_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'httpStatus', 'listManagedResourcesResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listManagedResourcesResponse_items' - The items in the response list.
newListManagedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedResourcesResponse
newListManagedResourcesResponse pHttpStatus_ =
  ListManagedResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listManagedResourcesResponse_nextToken :: Lens.Lens' ListManagedResourcesResponse (Prelude.Maybe Prelude.Text)
listManagedResourcesResponse_nextToken = Lens.lens (\ListManagedResourcesResponse' {nextToken} -> nextToken) (\s@ListManagedResourcesResponse' {} a -> s {nextToken = a} :: ListManagedResourcesResponse)

-- | The response's http status code.
listManagedResourcesResponse_httpStatus :: Lens.Lens' ListManagedResourcesResponse Prelude.Int
listManagedResourcesResponse_httpStatus = Lens.lens (\ListManagedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListManagedResourcesResponse' {} a -> s {httpStatus = a} :: ListManagedResourcesResponse)

-- | The items in the response list.
listManagedResourcesResponse_items :: Lens.Lens' ListManagedResourcesResponse [ManagedResourceSummary]
listManagedResourcesResponse_items = Lens.lens (\ListManagedResourcesResponse' {items} -> items) (\s@ListManagedResourcesResponse' {} a -> s {items = a} :: ListManagedResourcesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListManagedResourcesResponse where
  rnf ListManagedResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
