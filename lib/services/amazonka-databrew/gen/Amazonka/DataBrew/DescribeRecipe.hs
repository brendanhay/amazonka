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
-- Module      : Amazonka.DataBrew.DescribeRecipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the definition of a specific DataBrew recipe corresponding to a
-- particular version.
module Amazonka.DataBrew.DescribeRecipe
  ( -- * Creating a Request
    DescribeRecipe (..),
    newDescribeRecipe,

    -- * Request Lenses
    describeRecipe_recipeVersion,
    describeRecipe_name,

    -- * Destructuring the Response
    DescribeRecipeResponse (..),
    newDescribeRecipeResponse,

    -- * Response Lenses
    describeRecipeResponse_createDate,
    describeRecipeResponse_createdBy,
    describeRecipeResponse_description,
    describeRecipeResponse_lastModifiedBy,
    describeRecipeResponse_lastModifiedDate,
    describeRecipeResponse_projectName,
    describeRecipeResponse_publishedBy,
    describeRecipeResponse_publishedDate,
    describeRecipeResponse_recipeVersion,
    describeRecipeResponse_resourceArn,
    describeRecipeResponse_steps,
    describeRecipeResponse_tags,
    describeRecipeResponse_httpStatus,
    describeRecipeResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecipe' smart constructor.
data DescribeRecipe = DescribeRecipe'
  { -- | The recipe version identifier. If this parameter isn\'t specified, then
    -- the latest published version is returned.
    recipeVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the recipe to be described.
    name :: Prelude.Text
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
-- 'recipeVersion', 'describeRecipe_recipeVersion' - The recipe version identifier. If this parameter isn\'t specified, then
-- the latest published version is returned.
--
-- 'name', 'describeRecipe_name' - The name of the recipe to be described.
newDescribeRecipe ::
  -- | 'name'
  Prelude.Text ->
  DescribeRecipe
newDescribeRecipe pName_ =
  DescribeRecipe'
    { recipeVersion = Prelude.Nothing,
      name = pName_
    }

-- | The recipe version identifier. If this parameter isn\'t specified, then
-- the latest published version is returned.
describeRecipe_recipeVersion :: Lens.Lens' DescribeRecipe (Prelude.Maybe Prelude.Text)
describeRecipe_recipeVersion = Lens.lens (\DescribeRecipe' {recipeVersion} -> recipeVersion) (\s@DescribeRecipe' {} a -> s {recipeVersion = a} :: DescribeRecipe)

-- | The name of the recipe to be described.
describeRecipe_name :: Lens.Lens' DescribeRecipe Prelude.Text
describeRecipe_name = Lens.lens (\DescribeRecipe' {name} -> name) (\s@DescribeRecipe' {} a -> s {name = a} :: DescribeRecipe)

instance Core.AWSRequest DescribeRecipe where
  type
    AWSResponse DescribeRecipe =
      DescribeRecipeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecipeResponse'
            Prelude.<$> (x Data..?> "CreateDate")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedDate")
            Prelude.<*> (x Data..?> "ProjectName")
            Prelude.<*> (x Data..?> "PublishedBy")
            Prelude.<*> (x Data..?> "PublishedDate")
            Prelude.<*> (x Data..?> "RecipeVersion")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "Steps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable DescribeRecipe where
  hashWithSalt _salt DescribeRecipe' {..} =
    _salt
      `Prelude.hashWithSalt` recipeVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeRecipe where
  rnf DescribeRecipe' {..} =
    Prelude.rnf recipeVersion
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DescribeRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRecipe where
  toPath DescribeRecipe' {..} =
    Prelude.mconcat ["/recipes/", Data.toBS name]

instance Data.ToQuery DescribeRecipe where
  toQuery DescribeRecipe' {..} =
    Prelude.mconcat
      ["recipeVersion" Data.=: recipeVersion]

-- | /See:/ 'newDescribeRecipeResponse' smart constructor.
data DescribeRecipeResponse = DescribeRecipeResponse'
  { -- | The date and time that the recipe was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier (user name) of the user who created the recipe.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The description of the recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier (user name) of the user who last modified the recipe.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the recipe was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the project associated with this recipe.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The identifier (user name) of the user who last published the recipe.
    publishedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the recipe was last published.
    publishedDate :: Prelude.Maybe Data.POSIX,
    -- | The recipe version identifier.
    recipeVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the recipe.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | One or more steps to be performed by the recipe. Each step consists of
    -- an action, and the conditions under which the action should succeed.
    steps :: Prelude.Maybe [RecipeStep],
    -- | Metadata tags associated with this project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the recipe.
    name :: Prelude.Text
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
-- 'createDate', 'describeRecipeResponse_createDate' - The date and time that the recipe was created.
--
-- 'createdBy', 'describeRecipeResponse_createdBy' - The identifier (user name) of the user who created the recipe.
--
-- 'description', 'describeRecipeResponse_description' - The description of the recipe.
--
-- 'lastModifiedBy', 'describeRecipeResponse_lastModifiedBy' - The identifier (user name) of the user who last modified the recipe.
--
-- 'lastModifiedDate', 'describeRecipeResponse_lastModifiedDate' - The date and time that the recipe was last modified.
--
-- 'projectName', 'describeRecipeResponse_projectName' - The name of the project associated with this recipe.
--
-- 'publishedBy', 'describeRecipeResponse_publishedBy' - The identifier (user name) of the user who last published the recipe.
--
-- 'publishedDate', 'describeRecipeResponse_publishedDate' - The date and time when the recipe was last published.
--
-- 'recipeVersion', 'describeRecipeResponse_recipeVersion' - The recipe version identifier.
--
-- 'resourceArn', 'describeRecipeResponse_resourceArn' - The ARN of the recipe.
--
-- 'steps', 'describeRecipeResponse_steps' - One or more steps to be performed by the recipe. Each step consists of
-- an action, and the conditions under which the action should succeed.
--
-- 'tags', 'describeRecipeResponse_tags' - Metadata tags associated with this project.
--
-- 'httpStatus', 'describeRecipeResponse_httpStatus' - The response's http status code.
--
-- 'name', 'describeRecipeResponse_name' - The name of the recipe.
newDescribeRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DescribeRecipeResponse
newDescribeRecipeResponse pHttpStatus_ pName_ =
  DescribeRecipeResponse'
    { createDate =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      projectName = Prelude.Nothing,
      publishedBy = Prelude.Nothing,
      publishedDate = Prelude.Nothing,
      recipeVersion = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      steps = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The date and time that the recipe was created.
describeRecipeResponse_createDate :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.UTCTime)
describeRecipeResponse_createDate = Lens.lens (\DescribeRecipeResponse' {createDate} -> createDate) (\s@DescribeRecipeResponse' {} a -> s {createDate = a} :: DescribeRecipeResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier (user name) of the user who created the recipe.
describeRecipeResponse_createdBy :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_createdBy = Lens.lens (\DescribeRecipeResponse' {createdBy} -> createdBy) (\s@DescribeRecipeResponse' {} a -> s {createdBy = a} :: DescribeRecipeResponse)

-- | The description of the recipe.
describeRecipeResponse_description :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_description = Lens.lens (\DescribeRecipeResponse' {description} -> description) (\s@DescribeRecipeResponse' {} a -> s {description = a} :: DescribeRecipeResponse)

-- | The identifier (user name) of the user who last modified the recipe.
describeRecipeResponse_lastModifiedBy :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_lastModifiedBy = Lens.lens (\DescribeRecipeResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeRecipeResponse' {} a -> s {lastModifiedBy = a} :: DescribeRecipeResponse)

-- | The date and time that the recipe was last modified.
describeRecipeResponse_lastModifiedDate :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.UTCTime)
describeRecipeResponse_lastModifiedDate = Lens.lens (\DescribeRecipeResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeRecipeResponse' {} a -> s {lastModifiedDate = a} :: DescribeRecipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the project associated with this recipe.
describeRecipeResponse_projectName :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_projectName = Lens.lens (\DescribeRecipeResponse' {projectName} -> projectName) (\s@DescribeRecipeResponse' {} a -> s {projectName = a} :: DescribeRecipeResponse)

-- | The identifier (user name) of the user who last published the recipe.
describeRecipeResponse_publishedBy :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_publishedBy = Lens.lens (\DescribeRecipeResponse' {publishedBy} -> publishedBy) (\s@DescribeRecipeResponse' {} a -> s {publishedBy = a} :: DescribeRecipeResponse)

-- | The date and time when the recipe was last published.
describeRecipeResponse_publishedDate :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.UTCTime)
describeRecipeResponse_publishedDate = Lens.lens (\DescribeRecipeResponse' {publishedDate} -> publishedDate) (\s@DescribeRecipeResponse' {} a -> s {publishedDate = a} :: DescribeRecipeResponse) Prelude.. Lens.mapping Data._Time

-- | The recipe version identifier.
describeRecipeResponse_recipeVersion :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_recipeVersion = Lens.lens (\DescribeRecipeResponse' {recipeVersion} -> recipeVersion) (\s@DescribeRecipeResponse' {} a -> s {recipeVersion = a} :: DescribeRecipeResponse)

-- | The ARN of the recipe.
describeRecipeResponse_resourceArn :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe Prelude.Text)
describeRecipeResponse_resourceArn = Lens.lens (\DescribeRecipeResponse' {resourceArn} -> resourceArn) (\s@DescribeRecipeResponse' {} a -> s {resourceArn = a} :: DescribeRecipeResponse)

-- | One or more steps to be performed by the recipe. Each step consists of
-- an action, and the conditions under which the action should succeed.
describeRecipeResponse_steps :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe [RecipeStep])
describeRecipeResponse_steps = Lens.lens (\DescribeRecipeResponse' {steps} -> steps) (\s@DescribeRecipeResponse' {} a -> s {steps = a} :: DescribeRecipeResponse) Prelude.. Lens.mapping Lens.coerced

-- | Metadata tags associated with this project.
describeRecipeResponse_tags :: Lens.Lens' DescribeRecipeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRecipeResponse_tags = Lens.lens (\DescribeRecipeResponse' {tags} -> tags) (\s@DescribeRecipeResponse' {} a -> s {tags = a} :: DescribeRecipeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRecipeResponse_httpStatus :: Lens.Lens' DescribeRecipeResponse Prelude.Int
describeRecipeResponse_httpStatus = Lens.lens (\DescribeRecipeResponse' {httpStatus} -> httpStatus) (\s@DescribeRecipeResponse' {} a -> s {httpStatus = a} :: DescribeRecipeResponse)

-- | The name of the recipe.
describeRecipeResponse_name :: Lens.Lens' DescribeRecipeResponse Prelude.Text
describeRecipeResponse_name = Lens.lens (\DescribeRecipeResponse' {name} -> name) (\s@DescribeRecipeResponse' {} a -> s {name = a} :: DescribeRecipeResponse)

instance Prelude.NFData DescribeRecipeResponse where
  rnf DescribeRecipeResponse' {..} =
    Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf publishedBy
      `Prelude.seq` Prelude.rnf publishedDate
      `Prelude.seq` Prelude.rnf recipeVersion
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf steps
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
