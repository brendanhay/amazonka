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
-- Module      : Amazonka.DataBrew.ListRecipeVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a particular DataBrew recipe, except for
-- @LATEST_WORKING@.
--
-- This operation returns paginated results.
module Amazonka.DataBrew.ListRecipeVersions
  ( -- * Creating a Request
    ListRecipeVersions (..),
    newListRecipeVersions,

    -- * Request Lenses
    listRecipeVersions_maxResults,
    listRecipeVersions_nextToken,
    listRecipeVersions_name,

    -- * Destructuring the Response
    ListRecipeVersionsResponse (..),
    newListRecipeVersionsResponse,

    -- * Response Lenses
    listRecipeVersionsResponse_nextToken,
    listRecipeVersionsResponse_httpStatus,
    listRecipeVersionsResponse_recipes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecipeVersions' smart constructor.
data ListRecipeVersions = ListRecipeVersions'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the recipe for which to return version information.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecipeVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecipeVersions_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listRecipeVersions_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'name', 'listRecipeVersions_name' - The name of the recipe for which to return version information.
newListRecipeVersions ::
  -- | 'name'
  Prelude.Text ->
  ListRecipeVersions
newListRecipeVersions pName_ =
  ListRecipeVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of results to return in this request.
listRecipeVersions_maxResults :: Lens.Lens' ListRecipeVersions (Prelude.Maybe Prelude.Natural)
listRecipeVersions_maxResults = Lens.lens (\ListRecipeVersions' {maxResults} -> maxResults) (\s@ListRecipeVersions' {} a -> s {maxResults = a} :: ListRecipeVersions)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listRecipeVersions_nextToken :: Lens.Lens' ListRecipeVersions (Prelude.Maybe Prelude.Text)
listRecipeVersions_nextToken = Lens.lens (\ListRecipeVersions' {nextToken} -> nextToken) (\s@ListRecipeVersions' {} a -> s {nextToken = a} :: ListRecipeVersions)

-- | The name of the recipe for which to return version information.
listRecipeVersions_name :: Lens.Lens' ListRecipeVersions Prelude.Text
listRecipeVersions_name = Lens.lens (\ListRecipeVersions' {name} -> name) (\s@ListRecipeVersions' {} a -> s {name = a} :: ListRecipeVersions)

instance Core.AWSPager ListRecipeVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecipeVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listRecipeVersionsResponse_recipes) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRecipeVersions_nextToken
          Lens..~ rs
          Lens.^? listRecipeVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRecipeVersions where
  type
    AWSResponse ListRecipeVersions =
      ListRecipeVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecipeVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Recipes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRecipeVersions where
  hashWithSalt _salt ListRecipeVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListRecipeVersions where
  rnf ListRecipeVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ListRecipeVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecipeVersions where
  toPath = Prelude.const "/recipeVersions"

instance Data.ToQuery ListRecipeVersions where
  toQuery ListRecipeVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "name" Data.=: name
      ]

-- | /See:/ 'newListRecipeVersionsResponse' smart constructor.
data ListRecipeVersionsResponse = ListRecipeVersionsResponse'
  { -- | A token that you can use in a subsequent call to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of versions for the specified recipe.
    recipes :: [Recipe]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecipeVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecipeVersionsResponse_nextToken' - A token that you can use in a subsequent call to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listRecipeVersionsResponse_httpStatus' - The response's http status code.
--
-- 'recipes', 'listRecipeVersionsResponse_recipes' - A list of versions for the specified recipe.
newListRecipeVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecipeVersionsResponse
newListRecipeVersionsResponse pHttpStatus_ =
  ListRecipeVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      recipes = Prelude.mempty
    }

-- | A token that you can use in a subsequent call to retrieve the next set
-- of results.
listRecipeVersionsResponse_nextToken :: Lens.Lens' ListRecipeVersionsResponse (Prelude.Maybe Prelude.Text)
listRecipeVersionsResponse_nextToken = Lens.lens (\ListRecipeVersionsResponse' {nextToken} -> nextToken) (\s@ListRecipeVersionsResponse' {} a -> s {nextToken = a} :: ListRecipeVersionsResponse)

-- | The response's http status code.
listRecipeVersionsResponse_httpStatus :: Lens.Lens' ListRecipeVersionsResponse Prelude.Int
listRecipeVersionsResponse_httpStatus = Lens.lens (\ListRecipeVersionsResponse' {httpStatus} -> httpStatus) (\s@ListRecipeVersionsResponse' {} a -> s {httpStatus = a} :: ListRecipeVersionsResponse)

-- | A list of versions for the specified recipe.
listRecipeVersionsResponse_recipes :: Lens.Lens' ListRecipeVersionsResponse [Recipe]
listRecipeVersionsResponse_recipes = Lens.lens (\ListRecipeVersionsResponse' {recipes} -> recipes) (\s@ListRecipeVersionsResponse' {} a -> s {recipes = a} :: ListRecipeVersionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRecipeVersionsResponse where
  rnf ListRecipeVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recipes
