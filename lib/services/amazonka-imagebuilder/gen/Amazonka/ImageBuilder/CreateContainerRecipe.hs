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
-- Module      : Amazonka.ImageBuilder.CreateContainerRecipe
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new container recipe. Container recipes define how images are
-- configured, tested, and assessed.
module Amazonka.ImageBuilder.CreateContainerRecipe
  ( -- * Creating a Request
    CreateContainerRecipe (..),
    newCreateContainerRecipe,

    -- * Request Lenses
    createContainerRecipe_tags,
    createContainerRecipe_instanceConfiguration,
    createContainerRecipe_description,
    createContainerRecipe_platformOverride,
    createContainerRecipe_kmsKeyId,
    createContainerRecipe_imageOsVersionOverride,
    createContainerRecipe_dockerfileTemplateData,
    createContainerRecipe_dockerfileTemplateUri,
    createContainerRecipe_workingDirectory,
    createContainerRecipe_containerType,
    createContainerRecipe_name,
    createContainerRecipe_semanticVersion,
    createContainerRecipe_components,
    createContainerRecipe_parentImage,
    createContainerRecipe_targetRepository,
    createContainerRecipe_clientToken,

    -- * Destructuring the Response
    CreateContainerRecipeResponse (..),
    newCreateContainerRecipeResponse,

    -- * Response Lenses
    createContainerRecipeResponse_clientToken,
    createContainerRecipeResponse_requestId,
    createContainerRecipeResponse_containerRecipeArn,
    createContainerRecipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContainerRecipe' smart constructor.
data CreateContainerRecipe = CreateContainerRecipe'
  { -- | Tags that are attached to the container recipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A group of options that can be used to configure an instance for
    -- building and testing container images.
    instanceConfiguration :: Prelude.Maybe InstanceConfiguration,
    -- | The description of the container recipe.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the operating system platform when you use a custom base
    -- image.
    platformOverride :: Prelude.Maybe Platform,
    -- | Identifies which KMS key is used to encrypt the container image.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the operating system version for the base image.
    imageOsVersionOverride :: Prelude.Maybe Prelude.Text,
    -- | The Dockerfile template used to build your image as an inline data blob.
    dockerfileTemplateData :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI for the Dockerfile that will be used to build your
    -- container image.
    dockerfileTemplateUri :: Prelude.Maybe Prelude.Text,
    -- | The working directory for use during build and test workflows.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | The type of container to create.
    containerType :: ContainerType,
    -- | The name of the container recipe.
    name :: Prelude.Text,
    -- | The semantic version of the container recipe. This version follows the
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
    -- | Components for build and test that are included in the container recipe.
    components :: Prelude.NonEmpty ComponentConfiguration,
    -- | The base image for the container recipe.
    parentImage :: Prelude.Text,
    -- | The destination repository for the container image.
    targetRepository :: TargetContainerRepository,
    -- | The client token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerRecipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContainerRecipe_tags' - Tags that are attached to the container recipe.
--
-- 'instanceConfiguration', 'createContainerRecipe_instanceConfiguration' - A group of options that can be used to configure an instance for
-- building and testing container images.
--
-- 'description', 'createContainerRecipe_description' - The description of the container recipe.
--
-- 'platformOverride', 'createContainerRecipe_platformOverride' - Specifies the operating system platform when you use a custom base
-- image.
--
-- 'kmsKeyId', 'createContainerRecipe_kmsKeyId' - Identifies which KMS key is used to encrypt the container image.
--
-- 'imageOsVersionOverride', 'createContainerRecipe_imageOsVersionOverride' - Specifies the operating system version for the base image.
--
-- 'dockerfileTemplateData', 'createContainerRecipe_dockerfileTemplateData' - The Dockerfile template used to build your image as an inline data blob.
--
-- 'dockerfileTemplateUri', 'createContainerRecipe_dockerfileTemplateUri' - The Amazon S3 URI for the Dockerfile that will be used to build your
-- container image.
--
-- 'workingDirectory', 'createContainerRecipe_workingDirectory' - The working directory for use during build and test workflows.
--
-- 'containerType', 'createContainerRecipe_containerType' - The type of container to create.
--
-- 'name', 'createContainerRecipe_name' - The name of the container recipe.
--
-- 'semanticVersion', 'createContainerRecipe_semanticVersion' - The semantic version of the container recipe. This version follows the
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
-- 'components', 'createContainerRecipe_components' - Components for build and test that are included in the container recipe.
--
-- 'parentImage', 'createContainerRecipe_parentImage' - The base image for the container recipe.
--
-- 'targetRepository', 'createContainerRecipe_targetRepository' - The destination repository for the container image.
--
-- 'clientToken', 'createContainerRecipe_clientToken' - The client token used to make this request idempotent.
newCreateContainerRecipe ::
  -- | 'containerType'
  ContainerType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  -- | 'components'
  Prelude.NonEmpty ComponentConfiguration ->
  -- | 'parentImage'
  Prelude.Text ->
  -- | 'targetRepository'
  TargetContainerRepository ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateContainerRecipe
newCreateContainerRecipe
  pContainerType_
  pName_
  pSemanticVersion_
  pComponents_
  pParentImage_
  pTargetRepository_
  pClientToken_ =
    CreateContainerRecipe'
      { tags = Prelude.Nothing,
        instanceConfiguration = Prelude.Nothing,
        description = Prelude.Nothing,
        platformOverride = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        imageOsVersionOverride = Prelude.Nothing,
        dockerfileTemplateData = Prelude.Nothing,
        dockerfileTemplateUri = Prelude.Nothing,
        workingDirectory = Prelude.Nothing,
        containerType = pContainerType_,
        name = pName_,
        semanticVersion = pSemanticVersion_,
        components = Lens.coerced Lens.# pComponents_,
        parentImage = pParentImage_,
        targetRepository = pTargetRepository_,
        clientToken = pClientToken_
      }

-- | Tags that are attached to the container recipe.
createContainerRecipe_tags :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createContainerRecipe_tags = Lens.lens (\CreateContainerRecipe' {tags} -> tags) (\s@CreateContainerRecipe' {} a -> s {tags = a} :: CreateContainerRecipe) Prelude.. Lens.mapping Lens.coerced

-- | A group of options that can be used to configure an instance for
-- building and testing container images.
createContainerRecipe_instanceConfiguration :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe InstanceConfiguration)
createContainerRecipe_instanceConfiguration = Lens.lens (\CreateContainerRecipe' {instanceConfiguration} -> instanceConfiguration) (\s@CreateContainerRecipe' {} a -> s {instanceConfiguration = a} :: CreateContainerRecipe)

-- | The description of the container recipe.
createContainerRecipe_description :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Prelude.Text)
createContainerRecipe_description = Lens.lens (\CreateContainerRecipe' {description} -> description) (\s@CreateContainerRecipe' {} a -> s {description = a} :: CreateContainerRecipe)

-- | Specifies the operating system platform when you use a custom base
-- image.
createContainerRecipe_platformOverride :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Platform)
createContainerRecipe_platformOverride = Lens.lens (\CreateContainerRecipe' {platformOverride} -> platformOverride) (\s@CreateContainerRecipe' {} a -> s {platformOverride = a} :: CreateContainerRecipe)

-- | Identifies which KMS key is used to encrypt the container image.
createContainerRecipe_kmsKeyId :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Prelude.Text)
createContainerRecipe_kmsKeyId = Lens.lens (\CreateContainerRecipe' {kmsKeyId} -> kmsKeyId) (\s@CreateContainerRecipe' {} a -> s {kmsKeyId = a} :: CreateContainerRecipe)

-- | Specifies the operating system version for the base image.
createContainerRecipe_imageOsVersionOverride :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Prelude.Text)
createContainerRecipe_imageOsVersionOverride = Lens.lens (\CreateContainerRecipe' {imageOsVersionOverride} -> imageOsVersionOverride) (\s@CreateContainerRecipe' {} a -> s {imageOsVersionOverride = a} :: CreateContainerRecipe)

-- | The Dockerfile template used to build your image as an inline data blob.
createContainerRecipe_dockerfileTemplateData :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Prelude.Text)
createContainerRecipe_dockerfileTemplateData = Lens.lens (\CreateContainerRecipe' {dockerfileTemplateData} -> dockerfileTemplateData) (\s@CreateContainerRecipe' {} a -> s {dockerfileTemplateData = a} :: CreateContainerRecipe)

-- | The Amazon S3 URI for the Dockerfile that will be used to build your
-- container image.
createContainerRecipe_dockerfileTemplateUri :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Prelude.Text)
createContainerRecipe_dockerfileTemplateUri = Lens.lens (\CreateContainerRecipe' {dockerfileTemplateUri} -> dockerfileTemplateUri) (\s@CreateContainerRecipe' {} a -> s {dockerfileTemplateUri = a} :: CreateContainerRecipe)

-- | The working directory for use during build and test workflows.
createContainerRecipe_workingDirectory :: Lens.Lens' CreateContainerRecipe (Prelude.Maybe Prelude.Text)
createContainerRecipe_workingDirectory = Lens.lens (\CreateContainerRecipe' {workingDirectory} -> workingDirectory) (\s@CreateContainerRecipe' {} a -> s {workingDirectory = a} :: CreateContainerRecipe)

-- | The type of container to create.
createContainerRecipe_containerType :: Lens.Lens' CreateContainerRecipe ContainerType
createContainerRecipe_containerType = Lens.lens (\CreateContainerRecipe' {containerType} -> containerType) (\s@CreateContainerRecipe' {} a -> s {containerType = a} :: CreateContainerRecipe)

-- | The name of the container recipe.
createContainerRecipe_name :: Lens.Lens' CreateContainerRecipe Prelude.Text
createContainerRecipe_name = Lens.lens (\CreateContainerRecipe' {name} -> name) (\s@CreateContainerRecipe' {} a -> s {name = a} :: CreateContainerRecipe)

-- | The semantic version of the container recipe. This version follows the
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
createContainerRecipe_semanticVersion :: Lens.Lens' CreateContainerRecipe Prelude.Text
createContainerRecipe_semanticVersion = Lens.lens (\CreateContainerRecipe' {semanticVersion} -> semanticVersion) (\s@CreateContainerRecipe' {} a -> s {semanticVersion = a} :: CreateContainerRecipe)

-- | Components for build and test that are included in the container recipe.
createContainerRecipe_components :: Lens.Lens' CreateContainerRecipe (Prelude.NonEmpty ComponentConfiguration)
createContainerRecipe_components = Lens.lens (\CreateContainerRecipe' {components} -> components) (\s@CreateContainerRecipe' {} a -> s {components = a} :: CreateContainerRecipe) Prelude.. Lens.coerced

-- | The base image for the container recipe.
createContainerRecipe_parentImage :: Lens.Lens' CreateContainerRecipe Prelude.Text
createContainerRecipe_parentImage = Lens.lens (\CreateContainerRecipe' {parentImage} -> parentImage) (\s@CreateContainerRecipe' {} a -> s {parentImage = a} :: CreateContainerRecipe)

-- | The destination repository for the container image.
createContainerRecipe_targetRepository :: Lens.Lens' CreateContainerRecipe TargetContainerRepository
createContainerRecipe_targetRepository = Lens.lens (\CreateContainerRecipe' {targetRepository} -> targetRepository) (\s@CreateContainerRecipe' {} a -> s {targetRepository = a} :: CreateContainerRecipe)

-- | The client token used to make this request idempotent.
createContainerRecipe_clientToken :: Lens.Lens' CreateContainerRecipe Prelude.Text
createContainerRecipe_clientToken = Lens.lens (\CreateContainerRecipe' {clientToken} -> clientToken) (\s@CreateContainerRecipe' {} a -> s {clientToken = a} :: CreateContainerRecipe)

instance Core.AWSRequest CreateContainerRecipe where
  type
    AWSResponse CreateContainerRecipe =
      CreateContainerRecipeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerRecipeResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "containerRecipeArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContainerRecipe where
  hashWithSalt _salt CreateContainerRecipe' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` platformOverride
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` imageOsVersionOverride
      `Prelude.hashWithSalt` dockerfileTemplateData
      `Prelude.hashWithSalt` dockerfileTemplateUri
      `Prelude.hashWithSalt` workingDirectory
      `Prelude.hashWithSalt` containerType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` parentImage
      `Prelude.hashWithSalt` targetRepository
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateContainerRecipe where
  rnf CreateContainerRecipe' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf platformOverride
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf imageOsVersionOverride
      `Prelude.seq` Prelude.rnf dockerfileTemplateData
      `Prelude.seq` Prelude.rnf dockerfileTemplateUri
      `Prelude.seq` Prelude.rnf workingDirectory
      `Prelude.seq` Prelude.rnf containerType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf parentImage
      `Prelude.seq` Prelude.rnf targetRepository
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateContainerRecipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContainerRecipe where
  toJSON CreateContainerRecipe' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("instanceConfiguration" Data..=)
              Prelude.<$> instanceConfiguration,
            ("description" Data..=) Prelude.<$> description,
            ("platformOverride" Data..=)
              Prelude.<$> platformOverride,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("imageOsVersionOverride" Data..=)
              Prelude.<$> imageOsVersionOverride,
            ("dockerfileTemplateData" Data..=)
              Prelude.<$> dockerfileTemplateData,
            ("dockerfileTemplateUri" Data..=)
              Prelude.<$> dockerfileTemplateUri,
            ("workingDirectory" Data..=)
              Prelude.<$> workingDirectory,
            Prelude.Just ("containerType" Data..= containerType),
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("semanticVersion" Data..= semanticVersion),
            Prelude.Just ("components" Data..= components),
            Prelude.Just ("parentImage" Data..= parentImage),
            Prelude.Just
              ("targetRepository" Data..= targetRepository),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateContainerRecipe where
  toPath = Prelude.const "/CreateContainerRecipe"

instance Data.ToQuery CreateContainerRecipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContainerRecipeResponse' smart constructor.
data CreateContainerRecipeResponse = CreateContainerRecipeResponse'
  { -- | The client token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Returns the Amazon Resource Name (ARN) of the container recipe that the
    -- request created.
    containerRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerRecipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createContainerRecipeResponse_clientToken' - The client token used to make this request idempotent.
--
-- 'requestId', 'createContainerRecipeResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'containerRecipeArn', 'createContainerRecipeResponse_containerRecipeArn' - Returns the Amazon Resource Name (ARN) of the container recipe that the
-- request created.
--
-- 'httpStatus', 'createContainerRecipeResponse_httpStatus' - The response's http status code.
newCreateContainerRecipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContainerRecipeResponse
newCreateContainerRecipeResponse pHttpStatus_ =
  CreateContainerRecipeResponse'
    { clientToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      containerRecipeArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The client token used to make this request idempotent.
createContainerRecipeResponse_clientToken :: Lens.Lens' CreateContainerRecipeResponse (Prelude.Maybe Prelude.Text)
createContainerRecipeResponse_clientToken = Lens.lens (\CreateContainerRecipeResponse' {clientToken} -> clientToken) (\s@CreateContainerRecipeResponse' {} a -> s {clientToken = a} :: CreateContainerRecipeResponse)

-- | The request ID that uniquely identifies this request.
createContainerRecipeResponse_requestId :: Lens.Lens' CreateContainerRecipeResponse (Prelude.Maybe Prelude.Text)
createContainerRecipeResponse_requestId = Lens.lens (\CreateContainerRecipeResponse' {requestId} -> requestId) (\s@CreateContainerRecipeResponse' {} a -> s {requestId = a} :: CreateContainerRecipeResponse)

-- | Returns the Amazon Resource Name (ARN) of the container recipe that the
-- request created.
createContainerRecipeResponse_containerRecipeArn :: Lens.Lens' CreateContainerRecipeResponse (Prelude.Maybe Prelude.Text)
createContainerRecipeResponse_containerRecipeArn = Lens.lens (\CreateContainerRecipeResponse' {containerRecipeArn} -> containerRecipeArn) (\s@CreateContainerRecipeResponse' {} a -> s {containerRecipeArn = a} :: CreateContainerRecipeResponse)

-- | The response's http status code.
createContainerRecipeResponse_httpStatus :: Lens.Lens' CreateContainerRecipeResponse Prelude.Int
createContainerRecipeResponse_httpStatus = Lens.lens (\CreateContainerRecipeResponse' {httpStatus} -> httpStatus) (\s@CreateContainerRecipeResponse' {} a -> s {httpStatus = a} :: CreateContainerRecipeResponse)

instance Prelude.NFData CreateContainerRecipeResponse where
  rnf CreateContainerRecipeResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf containerRecipeArn
      `Prelude.seq` Prelude.rnf httpStatus
