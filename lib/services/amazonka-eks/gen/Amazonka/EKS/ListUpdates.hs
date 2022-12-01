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
-- Module      : Amazonka.EKS.ListUpdates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the updates associated with an Amazon EKS cluster or managed node
-- group in your Amazon Web Services account, in the specified Region.
--
-- This operation returns paginated results.
module Amazonka.EKS.ListUpdates
  ( -- * Creating a Request
    ListUpdates (..),
    newListUpdates,

    -- * Request Lenses
    listUpdates_nextToken,
    listUpdates_nodegroupName,
    listUpdates_addonName,
    listUpdates_maxResults,
    listUpdates_name,

    -- * Destructuring the Response
    ListUpdatesResponse (..),
    newListUpdatesResponse,

    -- * Response Lenses
    listUpdatesResponse_nextToken,
    listUpdatesResponse_updateIds,
    listUpdatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUpdates' smart constructor.
data ListUpdates = ListUpdates'
  { -- | The @nextToken@ value returned from a previous paginated @ListUpdates@
    -- request where @maxResults@ was used and the results exceeded the value
    -- of that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon EKS managed node group to list updates for.
    nodegroupName :: Prelude.Maybe Prelude.Text,
    -- | The names of the installed add-ons that have available updates.
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of update results returned by @ListUpdates@ in
    -- paginated output. When you use this parameter, @ListUpdates@ returns
    -- only @maxResults@ results in a single page along with a @nextToken@
    -- response element. You can see the remaining results of the initial
    -- request by sending another @ListUpdates@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If you don\'t
    -- use this parameter, @ListUpdates@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Amazon EKS cluster to list updates for.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUpdates_nextToken' - The @nextToken@ value returned from a previous paginated @ListUpdates@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
--
-- 'nodegroupName', 'listUpdates_nodegroupName' - The name of the Amazon EKS managed node group to list updates for.
--
-- 'addonName', 'listUpdates_addonName' - The names of the installed add-ons that have available updates.
--
-- 'maxResults', 'listUpdates_maxResults' - The maximum number of update results returned by @ListUpdates@ in
-- paginated output. When you use this parameter, @ListUpdates@ returns
-- only @maxResults@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListUpdates@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If you don\'t
-- use this parameter, @ListUpdates@ returns up to 100 results and a
-- @nextToken@ value if applicable.
--
-- 'name', 'listUpdates_name' - The name of the Amazon EKS cluster to list updates for.
newListUpdates ::
  -- | 'name'
  Prelude.Text ->
  ListUpdates
newListUpdates pName_ =
  ListUpdates'
    { nextToken = Prelude.Nothing,
      nodegroupName = Prelude.Nothing,
      addonName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_
    }

-- | The @nextToken@ value returned from a previous paginated @ListUpdates@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
listUpdates_nextToken :: Lens.Lens' ListUpdates (Prelude.Maybe Prelude.Text)
listUpdates_nextToken = Lens.lens (\ListUpdates' {nextToken} -> nextToken) (\s@ListUpdates' {} a -> s {nextToken = a} :: ListUpdates)

-- | The name of the Amazon EKS managed node group to list updates for.
listUpdates_nodegroupName :: Lens.Lens' ListUpdates (Prelude.Maybe Prelude.Text)
listUpdates_nodegroupName = Lens.lens (\ListUpdates' {nodegroupName} -> nodegroupName) (\s@ListUpdates' {} a -> s {nodegroupName = a} :: ListUpdates)

-- | The names of the installed add-ons that have available updates.
listUpdates_addonName :: Lens.Lens' ListUpdates (Prelude.Maybe Prelude.Text)
listUpdates_addonName = Lens.lens (\ListUpdates' {addonName} -> addonName) (\s@ListUpdates' {} a -> s {addonName = a} :: ListUpdates)

-- | The maximum number of update results returned by @ListUpdates@ in
-- paginated output. When you use this parameter, @ListUpdates@ returns
-- only @maxResults@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListUpdates@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If you don\'t
-- use this parameter, @ListUpdates@ returns up to 100 results and a
-- @nextToken@ value if applicable.
listUpdates_maxResults :: Lens.Lens' ListUpdates (Prelude.Maybe Prelude.Natural)
listUpdates_maxResults = Lens.lens (\ListUpdates' {maxResults} -> maxResults) (\s@ListUpdates' {} a -> s {maxResults = a} :: ListUpdates)

-- | The name of the Amazon EKS cluster to list updates for.
listUpdates_name :: Lens.Lens' ListUpdates Prelude.Text
listUpdates_name = Lens.lens (\ListUpdates' {name} -> name) (\s@ListUpdates' {} a -> s {name = a} :: ListUpdates)

instance Core.AWSPager ListUpdates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUpdatesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUpdatesResponse_updateIds Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUpdates_nextToken
          Lens..~ rs
          Lens.^? listUpdatesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListUpdates where
  type AWSResponse ListUpdates = ListUpdatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUpdatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "updateIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUpdates where
  hashWithSalt _salt ListUpdates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nodegroupName
      `Prelude.hashWithSalt` addonName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListUpdates where
  rnf ListUpdates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodegroupName
      `Prelude.seq` Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders ListUpdates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListUpdates where
  toPath ListUpdates' {..} =
    Prelude.mconcat
      ["/clusters/", Core.toBS name, "/updates"]

instance Core.ToQuery ListUpdates where
  toQuery ListUpdates' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "nodegroupName" Core.=: nodegroupName,
        "addonName" Core.=: addonName,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListUpdatesResponse' smart constructor.
data ListUpdatesResponse = ListUpdatesResponse'
  { -- | The @nextToken@ value to include in a future @ListUpdates@ request. When
    -- the results of a @ListUpdates@ request exceed @maxResults@, you can use
    -- this value to retrieve the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all the updates for the specified cluster and Region.
    updateIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUpdatesResponse_nextToken' - The @nextToken@ value to include in a future @ListUpdates@ request. When
-- the results of a @ListUpdates@ request exceed @maxResults@, you can use
-- this value to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'updateIds', 'listUpdatesResponse_updateIds' - A list of all the updates for the specified cluster and Region.
--
-- 'httpStatus', 'listUpdatesResponse_httpStatus' - The response's http status code.
newListUpdatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUpdatesResponse
newListUpdatesResponse pHttpStatus_ =
  ListUpdatesResponse'
    { nextToken = Prelude.Nothing,
      updateIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListUpdates@ request. When
-- the results of a @ListUpdates@ request exceed @maxResults@, you can use
-- this value to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
listUpdatesResponse_nextToken :: Lens.Lens' ListUpdatesResponse (Prelude.Maybe Prelude.Text)
listUpdatesResponse_nextToken = Lens.lens (\ListUpdatesResponse' {nextToken} -> nextToken) (\s@ListUpdatesResponse' {} a -> s {nextToken = a} :: ListUpdatesResponse)

-- | A list of all the updates for the specified cluster and Region.
listUpdatesResponse_updateIds :: Lens.Lens' ListUpdatesResponse (Prelude.Maybe [Prelude.Text])
listUpdatesResponse_updateIds = Lens.lens (\ListUpdatesResponse' {updateIds} -> updateIds) (\s@ListUpdatesResponse' {} a -> s {updateIds = a} :: ListUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUpdatesResponse_httpStatus :: Lens.Lens' ListUpdatesResponse Prelude.Int
listUpdatesResponse_httpStatus = Lens.lens (\ListUpdatesResponse' {httpStatus} -> httpStatus) (\s@ListUpdatesResponse' {} a -> s {httpStatus = a} :: ListUpdatesResponse)

instance Prelude.NFData ListUpdatesResponse where
  rnf ListUpdatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf updateIds
      `Prelude.seq` Prelude.rnf httpStatus
