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
-- Module      : Network.AWS.EKS.ListAddons
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available add-ons.
--
-- This operation returns paginated results.
module Network.AWS.EKS.ListAddons
  ( -- * Creating a Request
    ListAddons (..),
    newListAddons,

    -- * Request Lenses
    listAddons_nextToken,
    listAddons_maxResults,
    listAddons_clusterName,

    -- * Destructuring the Response
    ListAddonsResponse (..),
    newListAddonsResponse,

    -- * Response Lenses
    listAddonsResponse_nextToken,
    listAddonsResponse_addons,
    listAddonsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAddons' smart constructor.
data ListAddons = ListAddons'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @ListAddonsRequest@ where @maxResults@ was used and the results exceeded
    -- the value of that parameter. Pagination continues from the end of the
    -- previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of add-on results returned by @ListAddonsRequest@ in
    -- paginated output. When you use this parameter, @ListAddonsRequest@
    -- returns only @maxResults@ results in a single page along with a
    -- @nextToken@ response element. You can see the remaining results of the
    -- initial request by sending another @ListAddonsRequest@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 100. If you
    -- don\'t use this parameter, @ListAddonsRequest@ returns up to 100 results
    -- and a @nextToken@ value, if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the cluster.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAddons' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAddons_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListAddonsRequest@ where @maxResults@ was used and the results exceeded
-- the value of that parameter. Pagination continues from the end of the
-- previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listAddons_maxResults' - The maximum number of add-on results returned by @ListAddonsRequest@ in
-- paginated output. When you use this parameter, @ListAddonsRequest@
-- returns only @maxResults@ results in a single page along with a
-- @nextToken@ response element. You can see the remaining results of the
-- initial request by sending another @ListAddonsRequest@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListAddonsRequest@ returns up to 100 results
-- and a @nextToken@ value, if applicable.
--
-- 'clusterName', 'listAddons_clusterName' - The name of the cluster.
newListAddons ::
  -- | 'clusterName'
  Prelude.Text ->
  ListAddons
newListAddons pClusterName_ =
  ListAddons'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      clusterName = pClusterName_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @ListAddonsRequest@ where @maxResults@ was used and the results exceeded
-- the value of that parameter. Pagination continues from the end of the
-- previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listAddons_nextToken :: Lens.Lens' ListAddons (Prelude.Maybe Prelude.Text)
listAddons_nextToken = Lens.lens (\ListAddons' {nextToken} -> nextToken) (\s@ListAddons' {} a -> s {nextToken = a} :: ListAddons)

-- | The maximum number of add-on results returned by @ListAddonsRequest@ in
-- paginated output. When you use this parameter, @ListAddonsRequest@
-- returns only @maxResults@ results in a single page along with a
-- @nextToken@ response element. You can see the remaining results of the
-- initial request by sending another @ListAddonsRequest@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListAddonsRequest@ returns up to 100 results
-- and a @nextToken@ value, if applicable.
listAddons_maxResults :: Lens.Lens' ListAddons (Prelude.Maybe Prelude.Natural)
listAddons_maxResults = Lens.lens (\ListAddons' {maxResults} -> maxResults) (\s@ListAddons' {} a -> s {maxResults = a} :: ListAddons)

-- | The name of the cluster.
listAddons_clusterName :: Lens.Lens' ListAddons Prelude.Text
listAddons_clusterName = Lens.lens (\ListAddons' {clusterName} -> clusterName) (\s@ListAddons' {} a -> s {clusterName = a} :: ListAddons)

instance Core.AWSPager ListAddons where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAddonsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAddonsResponse_addons Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAddons_nextToken
          Lens..~ rs
          Lens.^? listAddonsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListAddons where
  type AWSResponse ListAddons = ListAddonsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAddonsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "addons" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAddons

instance Prelude.NFData ListAddons

instance Core.ToHeaders ListAddons where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListAddons where
  toPath ListAddons' {..} =
    Prelude.mconcat
      ["/clusters/", Core.toBS clusterName, "/addons"]

instance Core.ToQuery ListAddons where
  toQuery ListAddons' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAddonsResponse' smart constructor.
data ListAddonsResponse = ListAddonsResponse'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @ListAddonsResponse@ where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of available add-ons.
    addons :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAddonsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAddonsResponse_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListAddonsResponse@ where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'addons', 'listAddonsResponse_addons' - A list of available add-ons.
--
-- 'httpStatus', 'listAddonsResponse_httpStatus' - The response's http status code.
newListAddonsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAddonsResponse
newListAddonsResponse pHttpStatus_ =
  ListAddonsResponse'
    { nextToken = Prelude.Nothing,
      addons = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @ListAddonsResponse@ where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listAddonsResponse_nextToken :: Lens.Lens' ListAddonsResponse (Prelude.Maybe Prelude.Text)
listAddonsResponse_nextToken = Lens.lens (\ListAddonsResponse' {nextToken} -> nextToken) (\s@ListAddonsResponse' {} a -> s {nextToken = a} :: ListAddonsResponse)

-- | A list of available add-ons.
listAddonsResponse_addons :: Lens.Lens' ListAddonsResponse (Prelude.Maybe [Prelude.Text])
listAddonsResponse_addons = Lens.lens (\ListAddonsResponse' {addons} -> addons) (\s@ListAddonsResponse' {} a -> s {addons = a} :: ListAddonsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAddonsResponse_httpStatus :: Lens.Lens' ListAddonsResponse Prelude.Int
listAddonsResponse_httpStatus = Lens.lens (\ListAddonsResponse' {httpStatus} -> httpStatus) (\s@ListAddonsResponse' {} a -> s {httpStatus = a} :: ListAddonsResponse)

instance Prelude.NFData ListAddonsResponse
