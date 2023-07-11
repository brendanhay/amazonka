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
-- Module      : Amazonka.ImageBuilder.CreateImageRecipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new image recipe. Image recipes define how images are
-- configured, tested, and assessed.
module Amazonka.ImageBuilder.CreateImageRecipe
  ( -- * Creating a Request
    CreateImageRecipe (..),
    newCreateImageRecipe,

    -- * Request Lenses
    createImageRecipe_additionalInstanceConfiguration,
    createImageRecipe_blockDeviceMappings,
    createImageRecipe_description,
    createImageRecipe_tags,
    createImageRecipe_workingDirectory,
    createImageRecipe_name,
    createImageRecipe_semanticVersion,
    createImageRecipe_components,
    createImageRecipe_parentImage,
    createImageRecipe_clientToken,

    -- * Destructuring the Response
    CreateImageRecipeResponse (..),
    newCreateImageRecipeResponse,

    -- * Response Lenses
    createImageRecipeResponse_clientToken,
    createImageRecipeResponse_imageRecipeArn,
    createImageRecipeResponse_requestId,
    createImageRecipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateImageRecipe' smart constructor.
data CreateImageRecipe = CreateImageRecipe'
  { -- | Specify additional settings and launch scripts for your build instances.
    additionalInstanceConfiguration :: Prelude.Maybe AdditionalInstanceConfiguration,
    -- | The block device mappings of the image recipe.
    blockDeviceMappings :: Prelude.Maybe [InstanceBlockDeviceMapping],
    -- | The description of the image recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags of the image recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The working directory used during build and test workflows.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | The name of the image recipe.
    name :: Prelude.Text,
    -- | The semantic version of the image recipe. This version follows the
    -- semantic version syntax.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Assignment:__ For the first three nodes you can assign any positive
    -- integer value, including zero, with an upper limit of 2^30-1, or
    -- 1073741823 for each node. Image Builder automatically assigns the build
    -- number to the fourth node.
    --
    -- __Patterns:__ You can use any numeric pattern that adheres to the
    -- assignment requirements for the nodes that you can assign. For example,
    -- you might choose a software version pattern, such as 1.0.0, or a date,
    -- such as 2021.01.01.
    semanticVersion :: Prelude.Text,
    -- | The components of the image recipe.
    components :: Prelude.NonEmpty ComponentConfiguration,
    -- | The base image of the image recipe. The value of the string can be the
    -- ARN of the base image or an AMI ID. The format for the ARN follows this
    -- example:
    -- @arn:aws:imagebuilder:us-west-2:aws:image\/windows-server-2016-english-full-base-x86\/x.x.x@.
    -- You can provide the specific version that you want to use, or you can
    -- use a wildcard in all of the fields. If you enter an AMI ID for the
    -- string value, you must have access to the AMI, and the AMI must be in
    -- the same Region in which you are using Image Builder.
    parentImage :: Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInstanceConfiguration', 'createImageRecipe_additionalInstanceConfiguration' - Specify additional settings and launch scripts for your build instances.
--
-- 'blockDeviceMappings', 'createImageRecipe_blockDeviceMappings' - The block device mappings of the image recipe.
--
-- 'description', 'createImageRecipe_description' - The description of the image recipe.
--
-- 'tags', 'createImageRecipe_tags' - The tags of the image recipe.
--
-- 'workingDirectory', 'createImageRecipe_workingDirectory' - The working directory used during build and test workflows.
--
-- 'name', 'createImageRecipe_name' - The name of the image recipe.
--
-- 'semanticVersion', 'createImageRecipe_semanticVersion' - The semantic version of the image recipe. This version follows the
-- semantic version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- 'components', 'createImageRecipe_components' - The components of the image recipe.
--
-- 'parentImage', 'createImageRecipe_parentImage' - The base image of the image recipe. The value of the string can be the
-- ARN of the base image or an AMI ID. The format for the ARN follows this
-- example:
-- @arn:aws:imagebuilder:us-west-2:aws:image\/windows-server-2016-english-full-base-x86\/x.x.x@.
-- You can provide the specific version that you want to use, or you can
-- use a wildcard in all of the fields. If you enter an AMI ID for the
-- string value, you must have access to the AMI, and the AMI must be in
-- the same Region in which you are using Image Builder.
--
-- 'clientToken', 'createImageRecipe_clientToken' - The idempotency token used to make this request idempotent.
newCreateImageRecipe ::
  -- | 'name'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  -- | 'components'
  Prelude.NonEmpty ComponentConfiguration ->
  -- | 'parentImage'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateImageRecipe
newCreateImageRecipe
  pName_
  pSemanticVersion_
  pComponents_
  pParentImage_
  pClientToken_ =
    CreateImageRecipe'
      { additionalInstanceConfiguration =
          Prelude.Nothing,
        blockDeviceMappings = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        workingDirectory = Prelude.Nothing,
        name = pName_,
        semanticVersion = pSemanticVersion_,
        components = Lens.coerced Lens.# pComponents_,
        parentImage = pParentImage_,
        clientToken = pClientToken_
      }

-- | Specify additional settings and launch scripts for your build instances.
createImageRecipe_additionalInstanceConfiguration :: Lens.Lens' CreateImageRecipe (Prelude.Maybe AdditionalInstanceConfiguration)
createImageRecipe_additionalInstanceConfiguration = Lens.lens (\CreateImageRecipe' {additionalInstanceConfiguration} -> additionalInstanceConfiguration) (\s@CreateImageRecipe' {} a -> s {additionalInstanceConfiguration = a} :: CreateImageRecipe)

-- | The block device mappings of the image recipe.
createImageRecipe_blockDeviceMappings :: Lens.Lens' CreateImageRecipe (Prelude.Maybe [InstanceBlockDeviceMapping])
createImageRecipe_blockDeviceMappings = Lens.lens (\CreateImageRecipe' {blockDeviceMappings} -> blockDeviceMappings) (\s@CreateImageRecipe' {} a -> s {blockDeviceMappings = a} :: CreateImageRecipe) Prelude.. Lens.mapping Lens.coerced

-- | The description of the image recipe.
createImageRecipe_description :: Lens.Lens' CreateImageRecipe (Prelude.Maybe Prelude.Text)
createImageRecipe_description = Lens.lens (\CreateImageRecipe' {description} -> description) (\s@CreateImageRecipe' {} a -> s {description = a} :: CreateImageRecipe)

-- | The tags of the image recipe.
createImageRecipe_tags :: Lens.Lens' CreateImageRecipe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createImageRecipe_tags = Lens.lens (\CreateImageRecipe' {tags} -> tags) (\s@CreateImageRecipe' {} a -> s {tags = a} :: CreateImageRecipe) Prelude.. Lens.mapping Lens.coerced

-- | The working directory used during build and test workflows.
createImageRecipe_workingDirectory :: Lens.Lens' CreateImageRecipe (Prelude.Maybe Prelude.Text)
createImageRecipe_workingDirectory = Lens.lens (\CreateImageRecipe' {workingDirectory} -> workingDirectory) (\s@CreateImageRecipe' {} a -> s {workingDirectory = a} :: CreateImageRecipe)

-- | The name of the image recipe.
createImageRecipe_name :: Lens.Lens' CreateImageRecipe Prelude.Text
createImageRecipe_name = Lens.lens (\CreateImageRecipe' {name} -> name) (\s@CreateImageRecipe' {} a -> s {name = a} :: CreateImageRecipe)

-- | The semantic version of the image recipe. This version follows the
-- semantic version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
createImageRecipe_semanticVersion :: Lens.Lens' CreateImageRecipe Prelude.Text
createImageRecipe_semanticVersion = Lens.lens (\CreateImageRecipe' {semanticVersion} -> semanticVersion) (\s@CreateImageRecipe' {} a -> s {semanticVersion = a} :: CreateImageRecipe)

-- | The components of the image recipe.
createImageRecipe_components :: Lens.Lens' CreateImageRecipe (Prelude.NonEmpty ComponentConfiguration)
createImageRecipe_components = Lens.lens (\CreateImageRecipe' {components} -> components) (\s@CreateImageRecipe' {} a -> s {components = a} :: CreateImageRecipe) Prelude.. Lens.coerced

-- | The base image of the image recipe. The value of the string can be the
-- ARN of the base image or an AMI ID. The format for the ARN follows this
-- example:
-- @arn:aws:imagebuilder:us-west-2:aws:image\/windows-server-2016-english-full-base-x86\/x.x.x@.
-- You can provide the specific version that you want to use, or you can
-- use a wildcard in all of the fields. If you enter an AMI ID for the
-- string value, you must have access to the AMI, and the AMI must be in
-- the same Region in which you are using Image Builder.
createImageRecipe_parentImage :: Lens.Lens' CreateImageRecipe Prelude.Text
createImageRecipe_parentImage = Lens.lens (\CreateImageRecipe' {parentImage} -> parentImage) (\s@CreateImageRecipe' {} a -> s {parentImage = a} :: CreateImageRecipe)

-- | The idempotency token used to make this request idempotent.
createImageRecipe_clientToken :: Lens.Lens' CreateImageRecipe Prelude.Text
createImageRecipe_clientToken = Lens.lens (\CreateImageRecipe' {clientToken} -> clientToken) (\s@CreateImageRecipe' {} a -> s {clientToken = a} :: CreateImageRecipe)

instance Core.AWSRequest CreateImageRecipe where
  type
    AWSResponse CreateImageRecipe =
      CreateImageRecipeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageRecipeResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "imageRecipeArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImageRecipe where
  hashWithSalt _salt CreateImageRecipe' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInstanceConfiguration
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workingDirectory
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` parentImage
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateImageRecipe where
  rnf CreateImageRecipe' {..} =
    Prelude.rnf additionalInstanceConfiguration
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workingDirectory
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf parentImage
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateImageRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateImageRecipe where
  toJSON CreateImageRecipe' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalInstanceConfiguration" Data..=)
              Prelude.<$> additionalInstanceConfiguration,
            ("blockDeviceMappings" Data..=)
              Prelude.<$> blockDeviceMappings,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            ("workingDirectory" Data..=)
              Prelude.<$> workingDirectory,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("semanticVersion" Data..= semanticVersion),
            Prelude.Just ("components" Data..= components),
            Prelude.Just ("parentImage" Data..= parentImage),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateImageRecipe where
  toPath = Prelude.const "/CreateImageRecipe"

instance Data.ToQuery CreateImageRecipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImageRecipeResponse' smart constructor.
data CreateImageRecipeResponse = CreateImageRecipeResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image recipe that was created by
    -- this request.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createImageRecipeResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'imageRecipeArn', 'createImageRecipeResponse_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that was created by
-- this request.
--
-- 'requestId', 'createImageRecipeResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'createImageRecipeResponse_httpStatus' - The response's http status code.
newCreateImageRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImageRecipeResponse
newCreateImageRecipeResponse pHttpStatus_ =
  CreateImageRecipeResponse'
    { clientToken =
        Prelude.Nothing,
      imageRecipeArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token used to make this request idempotent.
createImageRecipeResponse_clientToken :: Lens.Lens' CreateImageRecipeResponse (Prelude.Maybe Prelude.Text)
createImageRecipeResponse_clientToken = Lens.lens (\CreateImageRecipeResponse' {clientToken} -> clientToken) (\s@CreateImageRecipeResponse' {} a -> s {clientToken = a} :: CreateImageRecipeResponse)

-- | The Amazon Resource Name (ARN) of the image recipe that was created by
-- this request.
createImageRecipeResponse_imageRecipeArn :: Lens.Lens' CreateImageRecipeResponse (Prelude.Maybe Prelude.Text)
createImageRecipeResponse_imageRecipeArn = Lens.lens (\CreateImageRecipeResponse' {imageRecipeArn} -> imageRecipeArn) (\s@CreateImageRecipeResponse' {} a -> s {imageRecipeArn = a} :: CreateImageRecipeResponse)

-- | The request ID that uniquely identifies this request.
createImageRecipeResponse_requestId :: Lens.Lens' CreateImageRecipeResponse (Prelude.Maybe Prelude.Text)
createImageRecipeResponse_requestId = Lens.lens (\CreateImageRecipeResponse' {requestId} -> requestId) (\s@CreateImageRecipeResponse' {} a -> s {requestId = a} :: CreateImageRecipeResponse)

-- | The response's http status code.
createImageRecipeResponse_httpStatus :: Lens.Lens' CreateImageRecipeResponse Prelude.Int
createImageRecipeResponse_httpStatus = Lens.lens (\CreateImageRecipeResponse' {httpStatus} -> httpStatus) (\s@CreateImageRecipeResponse' {} a -> s {httpStatus = a} :: CreateImageRecipeResponse)

instance Prelude.NFData CreateImageRecipeResponse where
  rnf CreateImageRecipeResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
