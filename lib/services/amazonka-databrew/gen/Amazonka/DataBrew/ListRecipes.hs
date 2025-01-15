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
-- Module      : Amazonka.DataBrew.ListRecipes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the DataBrew recipes that are defined.
--
-- This operation returns paginated results.
module Amazonka.DataBrew.ListRecipes
  ( -- * Creating a Request
    ListRecipes (..),
    newListRecipes,

    -- * Request Lenses
    listRecipes_maxResults,
    listRecipes_nextToken,
    listRecipes_recipeVersion,

    -- * Destructuring the Response
    ListRecipesResponse (..),
    newListRecipesResponse,

    -- * Response Lenses
    listRecipesResponse_nextToken,
    listRecipesResponse_httpStatus,
    listRecipesResponse_recipes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecipes' smart constructor.
data ListRecipes = ListRecipes'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Return only those recipes with a version identifier of @LATEST_WORKING@
    -- or @LATEST_PUBLISHED@. If @RecipeVersion@ is omitted, @ListRecipes@
    -- returns all of the @LATEST_PUBLISHED@ recipe versions.
    --
    -- Valid values: @LATEST_WORKING@ | @LATEST_PUBLISHED@
    recipeVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecipes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecipes_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listRecipes_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'recipeVersion', 'listRecipes_recipeVersion' - Return only those recipes with a version identifier of @LATEST_WORKING@
-- or @LATEST_PUBLISHED@. If @RecipeVersion@ is omitted, @ListRecipes@
-- returns all of the @LATEST_PUBLISHED@ recipe versions.
--
-- Valid values: @LATEST_WORKING@ | @LATEST_PUBLISHED@
newListRecipes ::
  ListRecipes
newListRecipes =
  ListRecipes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recipeVersion = Prelude.Nothing
    }

-- | The maximum number of results to return in this request.
listRecipes_maxResults :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Natural)
listRecipes_maxResults = Lens.lens (\ListRecipes' {maxResults} -> maxResults) (\s@ListRecipes' {} a -> s {maxResults = a} :: ListRecipes)

-- | The token returned by a previous call to retrieve the next set of
-- results.
listRecipes_nextToken :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Text)
listRecipes_nextToken = Lens.lens (\ListRecipes' {nextToken} -> nextToken) (\s@ListRecipes' {} a -> s {nextToken = a} :: ListRecipes)

-- | Return only those recipes with a version identifier of @LATEST_WORKING@
-- or @LATEST_PUBLISHED@. If @RecipeVersion@ is omitted, @ListRecipes@
-- returns all of the @LATEST_PUBLISHED@ recipe versions.
--
-- Valid values: @LATEST_WORKING@ | @LATEST_PUBLISHED@
listRecipes_recipeVersion :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Text)
listRecipes_recipeVersion = Lens.lens (\ListRecipes' {recipeVersion} -> recipeVersion) (\s@ListRecipes' {} a -> s {recipeVersion = a} :: ListRecipes)

instance Core.AWSPager ListRecipes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecipesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listRecipesResponse_recipes) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listRecipes_nextToken
              Lens..~ rs
              Lens.^? listRecipesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListRecipes where
  type AWSResponse ListRecipes = ListRecipesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecipesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Recipes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRecipes where
  hashWithSalt _salt ListRecipes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` recipeVersion

instance Prelude.NFData ListRecipes where
  rnf ListRecipes' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf recipeVersion

instance Data.ToHeaders ListRecipes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecipes where
  toPath = Prelude.const "/recipes"

instance Data.ToQuery ListRecipes where
  toQuery ListRecipes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "recipeVersion" Data.=: recipeVersion
      ]

-- | /See:/ 'newListRecipesResponse' smart constructor.
data ListRecipesResponse = ListRecipesResponse'
  { -- | A token that you can use in a subsequent call to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of recipes that are defined.
    recipes :: [Recipe]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecipesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecipesResponse_nextToken' - A token that you can use in a subsequent call to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listRecipesResponse_httpStatus' - The response's http status code.
--
-- 'recipes', 'listRecipesResponse_recipes' - A list of recipes that are defined.
newListRecipesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecipesResponse
newListRecipesResponse pHttpStatus_ =
  ListRecipesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      recipes = Prelude.mempty
    }

-- | A token that you can use in a subsequent call to retrieve the next set
-- of results.
listRecipesResponse_nextToken :: Lens.Lens' ListRecipesResponse (Prelude.Maybe Prelude.Text)
listRecipesResponse_nextToken = Lens.lens (\ListRecipesResponse' {nextToken} -> nextToken) (\s@ListRecipesResponse' {} a -> s {nextToken = a} :: ListRecipesResponse)

-- | The response's http status code.
listRecipesResponse_httpStatus :: Lens.Lens' ListRecipesResponse Prelude.Int
listRecipesResponse_httpStatus = Lens.lens (\ListRecipesResponse' {httpStatus} -> httpStatus) (\s@ListRecipesResponse' {} a -> s {httpStatus = a} :: ListRecipesResponse)

-- | A list of recipes that are defined.
listRecipesResponse_recipes :: Lens.Lens' ListRecipesResponse [Recipe]
listRecipesResponse_recipes = Lens.lens (\ListRecipesResponse' {recipes} -> recipes) (\s@ListRecipesResponse' {} a -> s {recipes = a} :: ListRecipesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRecipesResponse where
  rnf ListRecipesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf recipes
