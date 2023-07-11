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
-- Module      : Amazonka.Personalize.ListRecipes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available recipes. The response provides the
-- properties for each recipe, including the recipe\'s Amazon Resource Name
-- (ARN).
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListRecipes
  ( -- * Creating a Request
    ListRecipes (..),
    newListRecipes,

    -- * Request Lenses
    listRecipes_domain,
    listRecipes_maxResults,
    listRecipes_nextToken,
    listRecipes_recipeProvider,

    -- * Destructuring the Response
    ListRecipesResponse (..),
    newListRecipesResponse,

    -- * Response Lenses
    listRecipesResponse_nextToken,
    listRecipesResponse_recipes,
    listRecipesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRecipes' smart constructor.
data ListRecipes = ListRecipes'
  { -- | Filters returned recipes by domain for a Domain dataset group. Only
    -- recipes (Domain dataset group use cases) for this domain are included in
    -- the response. If you don\'t specify a domain, all recipes are returned.
    domain :: Prelude.Maybe Domain,
    -- | The maximum number of recipes to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token returned from the previous call to @ListRecipes@ for getting the
    -- next set of recipes (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The default is @SERVICE@.
    recipeProvider :: Prelude.Maybe RecipeProvider
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
-- 'domain', 'listRecipes_domain' - Filters returned recipes by domain for a Domain dataset group. Only
-- recipes (Domain dataset group use cases) for this domain are included in
-- the response. If you don\'t specify a domain, all recipes are returned.
--
-- 'maxResults', 'listRecipes_maxResults' - The maximum number of recipes to return.
--
-- 'nextToken', 'listRecipes_nextToken' - A token returned from the previous call to @ListRecipes@ for getting the
-- next set of recipes (if they exist).
--
-- 'recipeProvider', 'listRecipes_recipeProvider' - The default is @SERVICE@.
newListRecipes ::
  ListRecipes
newListRecipes =
  ListRecipes'
    { domain = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      recipeProvider = Prelude.Nothing
    }

-- | Filters returned recipes by domain for a Domain dataset group. Only
-- recipes (Domain dataset group use cases) for this domain are included in
-- the response. If you don\'t specify a domain, all recipes are returned.
listRecipes_domain :: Lens.Lens' ListRecipes (Prelude.Maybe Domain)
listRecipes_domain = Lens.lens (\ListRecipes' {domain} -> domain) (\s@ListRecipes' {} a -> s {domain = a} :: ListRecipes)

-- | The maximum number of recipes to return.
listRecipes_maxResults :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Natural)
listRecipes_maxResults = Lens.lens (\ListRecipes' {maxResults} -> maxResults) (\s@ListRecipes' {} a -> s {maxResults = a} :: ListRecipes)

-- | A token returned from the previous call to @ListRecipes@ for getting the
-- next set of recipes (if they exist).
listRecipes_nextToken :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Text)
listRecipes_nextToken = Lens.lens (\ListRecipes' {nextToken} -> nextToken) (\s@ListRecipes' {} a -> s {nextToken = a} :: ListRecipes)

-- | The default is @SERVICE@.
listRecipes_recipeProvider :: Lens.Lens' ListRecipes (Prelude.Maybe RecipeProvider)
listRecipes_recipeProvider = Lens.lens (\ListRecipes' {recipeProvider} -> recipeProvider) (\s@ListRecipes' {} a -> s {recipeProvider = a} :: ListRecipes)

instance Core.AWSPager ListRecipes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecipesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecipesResponse_recipes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRecipes_nextToken
          Lens..~ rs
          Lens.^? listRecipesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRecipes where
  type AWSResponse ListRecipes = ListRecipesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecipesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "recipes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecipes where
  hashWithSalt _salt ListRecipes' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` recipeProvider

instance Prelude.NFData ListRecipes where
  rnf ListRecipes' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recipeProvider

instance Data.ToHeaders ListRecipes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListRecipes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRecipes where
  toJSON ListRecipes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domain" Data..=) Prelude.<$> domain,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("recipeProvider" Data..=)
              Prelude.<$> recipeProvider
          ]
      )

instance Data.ToPath ListRecipes where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRecipes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRecipesResponse' smart constructor.
data ListRecipesResponse = ListRecipesResponse'
  { -- | A token for getting the next set of recipes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of available recipes.
    recipes :: Prelude.Maybe [RecipeSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listRecipesResponse_nextToken' - A token for getting the next set of recipes.
--
-- 'recipes', 'listRecipesResponse_recipes' - The list of available recipes.
--
-- 'httpStatus', 'listRecipesResponse_httpStatus' - The response's http status code.
newListRecipesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecipesResponse
newListRecipesResponse pHttpStatus_ =
  ListRecipesResponse'
    { nextToken = Prelude.Nothing,
      recipes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of recipes.
listRecipesResponse_nextToken :: Lens.Lens' ListRecipesResponse (Prelude.Maybe Prelude.Text)
listRecipesResponse_nextToken = Lens.lens (\ListRecipesResponse' {nextToken} -> nextToken) (\s@ListRecipesResponse' {} a -> s {nextToken = a} :: ListRecipesResponse)

-- | The list of available recipes.
listRecipesResponse_recipes :: Lens.Lens' ListRecipesResponse (Prelude.Maybe [RecipeSummary])
listRecipesResponse_recipes = Lens.lens (\ListRecipesResponse' {recipes} -> recipes) (\s@ListRecipesResponse' {} a -> s {recipes = a} :: ListRecipesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecipesResponse_httpStatus :: Lens.Lens' ListRecipesResponse Prelude.Int
listRecipesResponse_httpStatus = Lens.lens (\ListRecipesResponse' {httpStatus} -> httpStatus) (\s@ListRecipesResponse' {} a -> s {httpStatus = a} :: ListRecipesResponse)

instance Prelude.NFData ListRecipesResponse where
  rnf ListRecipesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recipes
      `Prelude.seq` Prelude.rnf httpStatus
