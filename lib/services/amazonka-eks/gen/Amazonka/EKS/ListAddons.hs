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
-- Module      : Amazonka.EKS.ListAddons
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available add-ons.
--
-- This operation returns paginated results.
module Amazonka.EKS.ListAddons
  ( -- * Creating a Request
    ListAddons (..),
    newListAddons,

    -- * Request Lenses
    listAddons_maxResults,
    listAddons_nextToken,
    listAddons_clusterName,

    -- * Destructuring the Response
    ListAddonsResponse (..),
    newListAddonsResponse,

    -- * Response Lenses
    listAddonsResponse_addons,
    listAddonsResponse_nextToken,
    listAddonsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAddons' smart constructor.
data ListAddons = ListAddons'
  { -- | The maximum number of add-on results returned by @ListAddonsRequest@ in
    -- paginated output. When you use this parameter, @ListAddonsRequest@
    -- returns only @maxResults@ results in a single page along with a
    -- @nextToken@ response element. You can see the remaining results of the
    -- initial request by sending another @ListAddonsRequest@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 100. If you
    -- don\'t use this parameter, @ListAddonsRequest@ returns up to 100 results
    -- and a @nextToken@ value, if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListAddonsRequest@ where @maxResults@ was used and the results exceeded
    -- the value of that parameter. Pagination continues from the end of the
    -- previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listAddons_maxResults' - The maximum number of add-on results returned by @ListAddonsRequest@ in
-- paginated output. When you use this parameter, @ListAddonsRequest@
-- returns only @maxResults@ results in a single page along with a
-- @nextToken@ response element. You can see the remaining results of the
-- initial request by sending another @ListAddonsRequest@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListAddonsRequest@ returns up to 100 results
-- and a @nextToken@ value, if applicable.
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
-- 'clusterName', 'listAddons_clusterName' - The name of the cluster.
newListAddons ::
  -- | 'clusterName'
  Prelude.Text ->
  ListAddons
newListAddons pClusterName_ =
  ListAddons'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterName = pClusterName_
    }

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

-- | The name of the cluster.
listAddons_clusterName :: Lens.Lens' ListAddons Prelude.Text
listAddons_clusterName = Lens.lens (\ListAddons' {clusterName} -> clusterName) (\s@ListAddons' {} a -> s {clusterName = a} :: ListAddons)

instance Core.AWSPager ListAddons where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAddonsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAddonsResponse_addons
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAddons_nextToken
          Lens..~ rs
          Lens.^? listAddonsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAddons where
  type AWSResponse ListAddons = ListAddonsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAddonsResponse'
            Prelude.<$> (x Data..?> "addons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAddons where
  hashWithSalt _salt ListAddons' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData ListAddons where
  rnf ListAddons' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders ListAddons where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAddons where
  toPath ListAddons' {..} =
    Prelude.mconcat
      ["/clusters/", Data.toBS clusterName, "/addons"]

instance Data.ToQuery ListAddons where
  toQuery ListAddons' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAddonsResponse' smart constructor.
data ListAddonsResponse = ListAddonsResponse'
  { -- | A list of available add-ons.
    addons :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListAddonsResponse@ where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'addons', 'listAddonsResponse_addons' - A list of available add-ons.
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
-- 'httpStatus', 'listAddonsResponse_httpStatus' - The response's http status code.
newListAddonsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAddonsResponse
newListAddonsResponse pHttpStatus_ =
  ListAddonsResponse'
    { addons = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of available add-ons.
listAddonsResponse_addons :: Lens.Lens' ListAddonsResponse (Prelude.Maybe [Prelude.Text])
listAddonsResponse_addons = Lens.lens (\ListAddonsResponse' {addons} -> addons) (\s@ListAddonsResponse' {} a -> s {addons = a} :: ListAddonsResponse) Prelude.. Lens.mapping Lens.coerced

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

-- | The response's http status code.
listAddonsResponse_httpStatus :: Lens.Lens' ListAddonsResponse Prelude.Int
listAddonsResponse_httpStatus = Lens.lens (\ListAddonsResponse' {httpStatus} -> httpStatus) (\s@ListAddonsResponse' {} a -> s {httpStatus = a} :: ListAddonsResponse)

instance Prelude.NFData ListAddonsResponse where
  rnf ListAddonsResponse' {..} =
    Prelude.rnf addons
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
