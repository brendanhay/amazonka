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
-- Module      : Amazonka.GreengrassV2.GetComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the recipe for a version of a component.
module Amazonka.GreengrassV2.GetComponent
  ( -- * Creating a Request
    GetComponent (..),
    newGetComponent,

    -- * Request Lenses
    getComponent_recipeOutputFormat,
    getComponent_arn,

    -- * Destructuring the Response
    GetComponentResponse (..),
    newGetComponentResponse,

    -- * Response Lenses
    getComponentResponse_tags,
    getComponentResponse_httpStatus,
    getComponentResponse_recipeOutputFormat,
    getComponentResponse_recipe,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComponent' smart constructor.
data GetComponent = GetComponent'
  { -- | The format of the recipe.
    recipeOutputFormat :: Prelude.Maybe RecipeOutputFormat,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recipeOutputFormat', 'getComponent_recipeOutputFormat' - The format of the recipe.
--
-- 'arn', 'getComponent_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
newGetComponent ::
  -- | 'arn'
  Prelude.Text ->
  GetComponent
newGetComponent pArn_ =
  GetComponent'
    { recipeOutputFormat = Prelude.Nothing,
      arn = pArn_
    }

-- | The format of the recipe.
getComponent_recipeOutputFormat :: Lens.Lens' GetComponent (Prelude.Maybe RecipeOutputFormat)
getComponent_recipeOutputFormat = Lens.lens (\GetComponent' {recipeOutputFormat} -> recipeOutputFormat) (\s@GetComponent' {} a -> s {recipeOutputFormat = a} :: GetComponent)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
getComponent_arn :: Lens.Lens' GetComponent Prelude.Text
getComponent_arn = Lens.lens (\GetComponent' {arn} -> arn) (\s@GetComponent' {} a -> s {arn = a} :: GetComponent)

instance Core.AWSRequest GetComponent where
  type AWSResponse GetComponent = GetComponentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "recipeOutputFormat")
            Prelude.<*> (x Data..:> "recipe")
      )

instance Prelude.Hashable GetComponent where
  hashWithSalt _salt GetComponent' {..} =
    _salt `Prelude.hashWithSalt` recipeOutputFormat
      `Prelude.hashWithSalt` arn

instance Prelude.NFData GetComponent where
  rnf GetComponent' {..} =
    Prelude.rnf recipeOutputFormat
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders GetComponent where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetComponent where
  toPath GetComponent' {..} =
    Prelude.mconcat
      ["/greengrass/v2/components/", Data.toBS arn]

instance Data.ToQuery GetComponent where
  toQuery GetComponent' {..} =
    Prelude.mconcat
      ["recipeOutputFormat" Data.=: recipeOutputFormat]

-- | /See:/ 'newGetComponentResponse' smart constructor.
data GetComponentResponse = GetComponentResponse'
  { -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The format of the recipe.
    recipeOutputFormat :: RecipeOutputFormat,
    -- | The recipe of the component version.
    recipe :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getComponentResponse_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'httpStatus', 'getComponentResponse_httpStatus' - The response's http status code.
--
-- 'recipeOutputFormat', 'getComponentResponse_recipeOutputFormat' - The format of the recipe.
--
-- 'recipe', 'getComponentResponse_recipe' - The recipe of the component version.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newGetComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recipeOutputFormat'
  RecipeOutputFormat ->
  -- | 'recipe'
  Prelude.ByteString ->
  GetComponentResponse
newGetComponentResponse
  pHttpStatus_
  pRecipeOutputFormat_
  pRecipe_ =
    GetComponentResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        recipeOutputFormat = pRecipeOutputFormat_,
        recipe = Data._Base64 Lens.# pRecipe_
      }

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
getComponentResponse_tags :: Lens.Lens' GetComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getComponentResponse_tags = Lens.lens (\GetComponentResponse' {tags} -> tags) (\s@GetComponentResponse' {} a -> s {tags = a} :: GetComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getComponentResponse_httpStatus :: Lens.Lens' GetComponentResponse Prelude.Int
getComponentResponse_httpStatus = Lens.lens (\GetComponentResponse' {httpStatus} -> httpStatus) (\s@GetComponentResponse' {} a -> s {httpStatus = a} :: GetComponentResponse)

-- | The format of the recipe.
getComponentResponse_recipeOutputFormat :: Lens.Lens' GetComponentResponse RecipeOutputFormat
getComponentResponse_recipeOutputFormat = Lens.lens (\GetComponentResponse' {recipeOutputFormat} -> recipeOutputFormat) (\s@GetComponentResponse' {} a -> s {recipeOutputFormat = a} :: GetComponentResponse)

-- | The recipe of the component version.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getComponentResponse_recipe :: Lens.Lens' GetComponentResponse Prelude.ByteString
getComponentResponse_recipe = Lens.lens (\GetComponentResponse' {recipe} -> recipe) (\s@GetComponentResponse' {} a -> s {recipe = a} :: GetComponentResponse) Prelude.. Data._Base64

instance Prelude.NFData GetComponentResponse where
  rnf GetComponentResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recipeOutputFormat
      `Prelude.seq` Prelude.rnf recipe
