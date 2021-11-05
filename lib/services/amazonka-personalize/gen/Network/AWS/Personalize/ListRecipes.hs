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
-- Module      : Network.AWS.Personalize.ListRecipes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available recipes. The response provides the
-- properties for each recipe, including the recipe\'s Amazon Resource Name
-- (ARN).
--
-- This operation returns paginated results.
module Network.AWS.Personalize.ListRecipes
  ( -- * Creating a Request
    ListRecipes (..),
    newListRecipes,

    -- * Request Lenses
    listRecipes_nextToken,
    listRecipes_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRecipes' smart constructor.
data ListRecipes = ListRecipes'
  { -- | A token returned from the previous call to @ListRecipes@ for getting the
    -- next set of recipes (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of recipes to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listRecipes_nextToken' - A token returned from the previous call to @ListRecipes@ for getting the
-- next set of recipes (if they exist).
--
-- 'maxResults', 'listRecipes_maxResults' - The maximum number of recipes to return.
--
-- 'recipeProvider', 'listRecipes_recipeProvider' - The default is @SERVICE@.
newListRecipes ::
  ListRecipes
newListRecipes =
  ListRecipes'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      recipeProvider = Prelude.Nothing
    }

-- | A token returned from the previous call to @ListRecipes@ for getting the
-- next set of recipes (if they exist).
listRecipes_nextToken :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Text)
listRecipes_nextToken = Lens.lens (\ListRecipes' {nextToken} -> nextToken) (\s@ListRecipes' {} a -> s {nextToken = a} :: ListRecipes)

-- | The maximum number of recipes to return.
listRecipes_maxResults :: Lens.Lens' ListRecipes (Prelude.Maybe Prelude.Natural)
listRecipes_maxResults = Lens.lens (\ListRecipes' {maxResults} -> maxResults) (\s@ListRecipes' {} a -> s {maxResults = a} :: ListRecipes)

-- | The default is @SERVICE@.
listRecipes_recipeProvider :: Lens.Lens' ListRecipes (Prelude.Maybe RecipeProvider)
listRecipes_recipeProvider = Lens.lens (\ListRecipes' {recipeProvider} -> recipeProvider) (\s@ListRecipes' {} a -> s {recipeProvider = a} :: ListRecipes)

instance Core.AWSPager ListRecipes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecipesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecipesResponse_recipes Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecipes_nextToken
          Lens..~ rs
          Lens.^? listRecipesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRecipes where
  type AWSResponse ListRecipes = ListRecipesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecipesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "recipes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecipes

instance Prelude.NFData ListRecipes

instance Core.ToHeaders ListRecipes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.ListRecipes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRecipes where
  toJSON ListRecipes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("recipeProvider" Core..=)
              Prelude.<$> recipeProvider
          ]
      )

instance Core.ToPath ListRecipes where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRecipes where
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

instance Prelude.NFData ListRecipesResponse
