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
-- Module      : Amazonka.DataBrew.UpdateRecipe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the definition of the @LATEST_WORKING@ version of a DataBrew
-- recipe.
module Amazonka.DataBrew.UpdateRecipe
  ( -- * Creating a Request
    UpdateRecipe (..),
    newUpdateRecipe,

    -- * Request Lenses
    updateRecipe_steps,
    updateRecipe_description,
    updateRecipe_name,

    -- * Destructuring the Response
    UpdateRecipeResponse (..),
    newUpdateRecipeResponse,

    -- * Response Lenses
    updateRecipeResponse_httpStatus,
    updateRecipeResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRecipe' smart constructor.
data UpdateRecipe = UpdateRecipe'
  { -- | One or more steps to be performed by the recipe. Each step consists of
    -- an action, and the conditions under which the action should succeed.
    steps :: Prelude.Maybe [RecipeStep],
    -- | A description of the recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the recipe to be updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'steps', 'updateRecipe_steps' - One or more steps to be performed by the recipe. Each step consists of
-- an action, and the conditions under which the action should succeed.
--
-- 'description', 'updateRecipe_description' - A description of the recipe.
--
-- 'name', 'updateRecipe_name' - The name of the recipe to be updated.
newUpdateRecipe ::
  -- | 'name'
  Prelude.Text ->
  UpdateRecipe
newUpdateRecipe pName_ =
  UpdateRecipe'
    { steps = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | One or more steps to be performed by the recipe. Each step consists of
-- an action, and the conditions under which the action should succeed.
updateRecipe_steps :: Lens.Lens' UpdateRecipe (Prelude.Maybe [RecipeStep])
updateRecipe_steps = Lens.lens (\UpdateRecipe' {steps} -> steps) (\s@UpdateRecipe' {} a -> s {steps = a} :: UpdateRecipe) Prelude.. Lens.mapping Lens.coerced

-- | A description of the recipe.
updateRecipe_description :: Lens.Lens' UpdateRecipe (Prelude.Maybe Prelude.Text)
updateRecipe_description = Lens.lens (\UpdateRecipe' {description} -> description) (\s@UpdateRecipe' {} a -> s {description = a} :: UpdateRecipe)

-- | The name of the recipe to be updated.
updateRecipe_name :: Lens.Lens' UpdateRecipe Prelude.Text
updateRecipe_name = Lens.lens (\UpdateRecipe' {name} -> name) (\s@UpdateRecipe' {} a -> s {name = a} :: UpdateRecipe)

instance Core.AWSRequest UpdateRecipe where
  type AWSResponse UpdateRecipe = UpdateRecipeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRecipeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Name")
      )

instance Prelude.Hashable UpdateRecipe where
  hashWithSalt _salt UpdateRecipe' {..} =
    _salt `Prelude.hashWithSalt` steps
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateRecipe where
  rnf UpdateRecipe' {..} =
    Prelude.rnf steps
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRecipe where
  toJSON UpdateRecipe' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Steps" Core..=) Prelude.<$> steps,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateRecipe where
  toPath UpdateRecipe' {..} =
    Prelude.mconcat ["/recipes/", Core.toBS name]

instance Core.ToQuery UpdateRecipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRecipeResponse' smart constructor.
data UpdateRecipeResponse = UpdateRecipeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the recipe that was updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRecipeResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateRecipeResponse_name' - The name of the recipe that was updated.
newUpdateRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateRecipeResponse
newUpdateRecipeResponse pHttpStatus_ pName_ =
  UpdateRecipeResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateRecipeResponse_httpStatus :: Lens.Lens' UpdateRecipeResponse Prelude.Int
updateRecipeResponse_httpStatus = Lens.lens (\UpdateRecipeResponse' {httpStatus} -> httpStatus) (\s@UpdateRecipeResponse' {} a -> s {httpStatus = a} :: UpdateRecipeResponse)

-- | The name of the recipe that was updated.
updateRecipeResponse_name :: Lens.Lens' UpdateRecipeResponse Prelude.Text
updateRecipeResponse_name = Lens.lens (\UpdateRecipeResponse' {name} -> name) (\s@UpdateRecipeResponse' {} a -> s {name = a} :: UpdateRecipeResponse)

instance Prelude.NFData UpdateRecipeResponse where
  rnf UpdateRecipeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
