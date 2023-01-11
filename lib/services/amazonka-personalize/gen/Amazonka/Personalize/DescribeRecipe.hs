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
-- Module      : Amazonka.Personalize.DescribeRecipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a recipe.
--
-- A recipe contains three items:
--
-- -   An algorithm that trains a model.
--
-- -   Hyperparameters that govern the training.
--
-- -   Feature transformation information for modifying the input data
--     before training.
--
-- Amazon Personalize provides a set of predefined recipes. You specify a
-- recipe when you create a solution with the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolution.html CreateSolution>
-- API. @CreateSolution@ trains a model by using the algorithm in the
-- specified recipe and a training dataset. The solution, when deployed as
-- a campaign, can provide recommendations using the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations>
-- API.
module Amazonka.Personalize.DescribeRecipe
  ( -- * Creating a Request
    DescribeRecipe (..),
    newDescribeRecipe,

    -- * Request Lenses
    describeRecipe_recipeArn,

    -- * Destructuring the Response
    DescribeRecipeResponse (..),
    newDescribeRecipeResponse,

    -- * Response Lenses
    describeRecipeResponse_recipe,
    describeRecipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecipe' smart constructor.
data DescribeRecipe = DescribeRecipe'
  { -- | The Amazon Resource Name (ARN) of the recipe to describe.
    recipeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recipeArn', 'describeRecipe_recipeArn' - The Amazon Resource Name (ARN) of the recipe to describe.
newDescribeRecipe ::
  -- | 'recipeArn'
  Prelude.Text ->
  DescribeRecipe
newDescribeRecipe pRecipeArn_ =
  DescribeRecipe' {recipeArn = pRecipeArn_}

-- | The Amazon Resource Name (ARN) of the recipe to describe.
describeRecipe_recipeArn :: Lens.Lens' DescribeRecipe Prelude.Text
describeRecipe_recipeArn = Lens.lens (\DescribeRecipe' {recipeArn} -> recipeArn) (\s@DescribeRecipe' {} a -> s {recipeArn = a} :: DescribeRecipe)

instance Core.AWSRequest DescribeRecipe where
  type
    AWSResponse DescribeRecipe =
      DescribeRecipeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecipeResponse'
            Prelude.<$> (x Data..?> "recipe")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecipe where
  hashWithSalt _salt DescribeRecipe' {..} =
    _salt `Prelude.hashWithSalt` recipeArn

instance Prelude.NFData DescribeRecipe where
  rnf DescribeRecipe' {..} = Prelude.rnf recipeArn

instance Data.ToHeaders DescribeRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeRecipe" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecipe where
  toJSON DescribeRecipe' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("recipeArn" Data..= recipeArn)]
      )

instance Data.ToPath DescribeRecipe where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRecipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecipeResponse' smart constructor.
data DescribeRecipeResponse = DescribeRecipeResponse'
  { -- | An object that describes the recipe.
    recipe :: Prelude.Maybe Recipe,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recipe', 'describeRecipeResponse_recipe' - An object that describes the recipe.
--
-- 'httpStatus', 'describeRecipeResponse_httpStatus' - The response's http status code.
newDescribeRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecipeResponse
newDescribeRecipeResponse pHttpStatus_ =
  DescribeRecipeResponse'
    { recipe = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the recipe.
describeRecipeResponse_recipe :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Recipe)
describeRecipeResponse_recipe = Lens.lens (\DescribeRecipeResponse' {recipe} -> recipe) (\s@DescribeRecipeResponse' {} a -> s {recipe = a} :: DescribeRecipeResponse)

-- | The response's http status code.
describeRecipeResponse_httpStatus :: Lens.Lens' DescribeRecipeResponse Prelude.Int
describeRecipeResponse_httpStatus = Lens.lens (\DescribeRecipeResponse' {httpStatus} -> httpStatus) (\s@DescribeRecipeResponse' {} a -> s {httpStatus = a} :: DescribeRecipeResponse)

instance Prelude.NFData DescribeRecipeResponse where
  rnf DescribeRecipeResponse' {..} =
    Prelude.rnf recipe
      `Prelude.seq` Prelude.rnf httpStatus
