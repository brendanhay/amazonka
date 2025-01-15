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
-- Module      : Amazonka.DataBrew.DeleteRecipeVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a single version of a DataBrew recipe.
module Amazonka.DataBrew.DeleteRecipeVersion
  ( -- * Creating a Request
    DeleteRecipeVersion (..),
    newDeleteRecipeVersion,

    -- * Request Lenses
    deleteRecipeVersion_name,
    deleteRecipeVersion_recipeVersion,

    -- * Destructuring the Response
    DeleteRecipeVersionResponse (..),
    newDeleteRecipeVersionResponse,

    -- * Response Lenses
    deleteRecipeVersionResponse_httpStatus,
    deleteRecipeVersionResponse_name,
    deleteRecipeVersionResponse_recipeVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRecipeVersion' smart constructor.
data DeleteRecipeVersion = DeleteRecipeVersion'
  { -- | The name of the recipe.
    name :: Prelude.Text,
    -- | The version of the recipe to be deleted. You can specify a numeric
    -- versions (@X.Y@) or @LATEST_WORKING@. @LATEST_PUBLISHED@ is not
    -- supported.
    recipeVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecipeVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteRecipeVersion_name' - The name of the recipe.
--
-- 'recipeVersion', 'deleteRecipeVersion_recipeVersion' - The version of the recipe to be deleted. You can specify a numeric
-- versions (@X.Y@) or @LATEST_WORKING@. @LATEST_PUBLISHED@ is not
-- supported.
newDeleteRecipeVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'recipeVersion'
  Prelude.Text ->
  DeleteRecipeVersion
newDeleteRecipeVersion pName_ pRecipeVersion_ =
  DeleteRecipeVersion'
    { name = pName_,
      recipeVersion = pRecipeVersion_
    }

-- | The name of the recipe.
deleteRecipeVersion_name :: Lens.Lens' DeleteRecipeVersion Prelude.Text
deleteRecipeVersion_name = Lens.lens (\DeleteRecipeVersion' {name} -> name) (\s@DeleteRecipeVersion' {} a -> s {name = a} :: DeleteRecipeVersion)

-- | The version of the recipe to be deleted. You can specify a numeric
-- versions (@X.Y@) or @LATEST_WORKING@. @LATEST_PUBLISHED@ is not
-- supported.
deleteRecipeVersion_recipeVersion :: Lens.Lens' DeleteRecipeVersion Prelude.Text
deleteRecipeVersion_recipeVersion = Lens.lens (\DeleteRecipeVersion' {recipeVersion} -> recipeVersion) (\s@DeleteRecipeVersion' {} a -> s {recipeVersion = a} :: DeleteRecipeVersion)

instance Core.AWSRequest DeleteRecipeVersion where
  type
    AWSResponse DeleteRecipeVersion =
      DeleteRecipeVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRecipeVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> (x Data..:> "RecipeVersion")
      )

instance Prelude.Hashable DeleteRecipeVersion where
  hashWithSalt _salt DeleteRecipeVersion' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recipeVersion

instance Prelude.NFData DeleteRecipeVersion where
  rnf DeleteRecipeVersion' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf recipeVersion

instance Data.ToHeaders DeleteRecipeVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRecipeVersion where
  toPath DeleteRecipeVersion' {..} =
    Prelude.mconcat
      [ "/recipes/",
        Data.toBS name,
        "/recipeVersion/",
        Data.toBS recipeVersion
      ]

instance Data.ToQuery DeleteRecipeVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRecipeVersionResponse' smart constructor.
data DeleteRecipeVersionResponse = DeleteRecipeVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the recipe that was deleted.
    name :: Prelude.Text,
    -- | The version of the recipe that was deleted.
    recipeVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRecipeVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRecipeVersionResponse_httpStatus' - The response's http status code.
--
-- 'name', 'deleteRecipeVersionResponse_name' - The name of the recipe that was deleted.
--
-- 'recipeVersion', 'deleteRecipeVersionResponse_recipeVersion' - The version of the recipe that was deleted.
newDeleteRecipeVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'recipeVersion'
  Prelude.Text ->
  DeleteRecipeVersionResponse
newDeleteRecipeVersionResponse
  pHttpStatus_
  pName_
  pRecipeVersion_ =
    DeleteRecipeVersionResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        recipeVersion = pRecipeVersion_
      }

-- | The response's http status code.
deleteRecipeVersionResponse_httpStatus :: Lens.Lens' DeleteRecipeVersionResponse Prelude.Int
deleteRecipeVersionResponse_httpStatus = Lens.lens (\DeleteRecipeVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteRecipeVersionResponse' {} a -> s {httpStatus = a} :: DeleteRecipeVersionResponse)

-- | The name of the recipe that was deleted.
deleteRecipeVersionResponse_name :: Lens.Lens' DeleteRecipeVersionResponse Prelude.Text
deleteRecipeVersionResponse_name = Lens.lens (\DeleteRecipeVersionResponse' {name} -> name) (\s@DeleteRecipeVersionResponse' {} a -> s {name = a} :: DeleteRecipeVersionResponse)

-- | The version of the recipe that was deleted.
deleteRecipeVersionResponse_recipeVersion :: Lens.Lens' DeleteRecipeVersionResponse Prelude.Text
deleteRecipeVersionResponse_recipeVersion = Lens.lens (\DeleteRecipeVersionResponse' {recipeVersion} -> recipeVersion) (\s@DeleteRecipeVersionResponse' {} a -> s {recipeVersion = a} :: DeleteRecipeVersionResponse)

instance Prelude.NFData DeleteRecipeVersionResponse where
  rnf DeleteRecipeVersionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf recipeVersion
