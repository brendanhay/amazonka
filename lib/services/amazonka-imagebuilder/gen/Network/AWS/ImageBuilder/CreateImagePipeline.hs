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
-- Module      : Network.AWS.ImageBuilder.CreateImagePipeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new image pipeline. Image pipelines enable you to automate the
-- creation and distribution of images.
module Network.AWS.ImageBuilder.CreateImagePipeline
  ( -- * Creating a Request
    CreateImagePipeline (..),
    newCreateImagePipeline,

    -- * Request Lenses
    createImagePipeline_status,
    createImagePipeline_containerRecipeArn,
    createImagePipeline_imageTestsConfiguration,
    createImagePipeline_schedule,
    createImagePipeline_enhancedImageMetadataEnabled,
    createImagePipeline_distributionConfigurationArn,
    createImagePipeline_imageRecipeArn,
    createImagePipeline_description,
    createImagePipeline_tags,
    createImagePipeline_name,
    createImagePipeline_infrastructureConfigurationArn,
    createImagePipeline_clientToken,

    -- * Destructuring the Response
    CreateImagePipelineResponse (..),
    newCreateImagePipelineResponse,

    -- * Response Lenses
    createImagePipelineResponse_requestId,
    createImagePipelineResponse_clientToken,
    createImagePipelineResponse_imagePipelineArn,
    createImagePipelineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImageBuilder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateImagePipeline' smart constructor.
data CreateImagePipeline = CreateImagePipeline'
  { -- | The status of the image pipeline.
    status :: Prelude.Maybe PipelineStatus,
    -- | The Amazon Resource Name (ARN) of the container recipe that is used to
    -- configure images created by this container pipeline.
    containerRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The image test configuration of the image pipeline.
    imageTestsConfiguration :: Prelude.Maybe ImageTestsConfiguration,
    -- | The schedule of the image pipeline.
    schedule :: Prelude.Maybe Schedule,
    -- | Collects additional information about the image being created, including
    -- the operating system (OS) version and package list. This information is
    -- used to enhance the overall experience of using EC2 Image Builder.
    -- Enabled by default.
    enhancedImageMetadataEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the distribution configuration that
    -- will be used to configure and distribute images created by this image
    -- pipeline.
    distributionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image recipe that will be used to
    -- configure images created by this image pipeline.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the image pipeline.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags of the image pipeline.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the image pipeline.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- will be used to build images created by this image pipeline.
    infrastructureConfigurationArn :: Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImagePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createImagePipeline_status' - The status of the image pipeline.
--
-- 'containerRecipeArn', 'createImagePipeline_containerRecipeArn' - The Amazon Resource Name (ARN) of the container recipe that is used to
-- configure images created by this container pipeline.
--
-- 'imageTestsConfiguration', 'createImagePipeline_imageTestsConfiguration' - The image test configuration of the image pipeline.
--
-- 'schedule', 'createImagePipeline_schedule' - The schedule of the image pipeline.
--
-- 'enhancedImageMetadataEnabled', 'createImagePipeline_enhancedImageMetadataEnabled' - Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
--
-- 'distributionConfigurationArn', 'createImagePipeline_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration that
-- will be used to configure and distribute images created by this image
-- pipeline.
--
-- 'imageRecipeArn', 'createImagePipeline_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that will be used to
-- configure images created by this image pipeline.
--
-- 'description', 'createImagePipeline_description' - The description of the image pipeline.
--
-- 'tags', 'createImagePipeline_tags' - The tags of the image pipeline.
--
-- 'name', 'createImagePipeline_name' - The name of the image pipeline.
--
-- 'infrastructureConfigurationArn', 'createImagePipeline_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- will be used to build images created by this image pipeline.
--
-- 'clientToken', 'createImagePipeline_clientToken' - The idempotency token used to make this request idempotent.
newCreateImagePipeline ::
  -- | 'name'
  Prelude.Text ->
  -- | 'infrastructureConfigurationArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateImagePipeline
newCreateImagePipeline
  pName_
  pInfrastructureConfigurationArn_
  pClientToken_ =
    CreateImagePipeline'
      { status = Prelude.Nothing,
        containerRecipeArn = Prelude.Nothing,
        imageTestsConfiguration = Prelude.Nothing,
        schedule = Prelude.Nothing,
        enhancedImageMetadataEnabled = Prelude.Nothing,
        distributionConfigurationArn = Prelude.Nothing,
        imageRecipeArn = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        infrastructureConfigurationArn =
          pInfrastructureConfigurationArn_,
        clientToken = pClientToken_
      }

-- | The status of the image pipeline.
createImagePipeline_status :: Lens.Lens' CreateImagePipeline (Prelude.Maybe PipelineStatus)
createImagePipeline_status = Lens.lens (\CreateImagePipeline' {status} -> status) (\s@CreateImagePipeline' {} a -> s {status = a} :: CreateImagePipeline)

-- | The Amazon Resource Name (ARN) of the container recipe that is used to
-- configure images created by this container pipeline.
createImagePipeline_containerRecipeArn :: Lens.Lens' CreateImagePipeline (Prelude.Maybe Prelude.Text)
createImagePipeline_containerRecipeArn = Lens.lens (\CreateImagePipeline' {containerRecipeArn} -> containerRecipeArn) (\s@CreateImagePipeline' {} a -> s {containerRecipeArn = a} :: CreateImagePipeline)

-- | The image test configuration of the image pipeline.
createImagePipeline_imageTestsConfiguration :: Lens.Lens' CreateImagePipeline (Prelude.Maybe ImageTestsConfiguration)
createImagePipeline_imageTestsConfiguration = Lens.lens (\CreateImagePipeline' {imageTestsConfiguration} -> imageTestsConfiguration) (\s@CreateImagePipeline' {} a -> s {imageTestsConfiguration = a} :: CreateImagePipeline)

-- | The schedule of the image pipeline.
createImagePipeline_schedule :: Lens.Lens' CreateImagePipeline (Prelude.Maybe Schedule)
createImagePipeline_schedule = Lens.lens (\CreateImagePipeline' {schedule} -> schedule) (\s@CreateImagePipeline' {} a -> s {schedule = a} :: CreateImagePipeline)

-- | Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
createImagePipeline_enhancedImageMetadataEnabled :: Lens.Lens' CreateImagePipeline (Prelude.Maybe Prelude.Bool)
createImagePipeline_enhancedImageMetadataEnabled = Lens.lens (\CreateImagePipeline' {enhancedImageMetadataEnabled} -> enhancedImageMetadataEnabled) (\s@CreateImagePipeline' {} a -> s {enhancedImageMetadataEnabled = a} :: CreateImagePipeline)

-- | The Amazon Resource Name (ARN) of the distribution configuration that
-- will be used to configure and distribute images created by this image
-- pipeline.
createImagePipeline_distributionConfigurationArn :: Lens.Lens' CreateImagePipeline (Prelude.Maybe Prelude.Text)
createImagePipeline_distributionConfigurationArn = Lens.lens (\CreateImagePipeline' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@CreateImagePipeline' {} a -> s {distributionConfigurationArn = a} :: CreateImagePipeline)

-- | The Amazon Resource Name (ARN) of the image recipe that will be used to
-- configure images created by this image pipeline.
createImagePipeline_imageRecipeArn :: Lens.Lens' CreateImagePipeline (Prelude.Maybe Prelude.Text)
createImagePipeline_imageRecipeArn = Lens.lens (\CreateImagePipeline' {imageRecipeArn} -> imageRecipeArn) (\s@CreateImagePipeline' {} a -> s {imageRecipeArn = a} :: CreateImagePipeline)

-- | The description of the image pipeline.
createImagePipeline_description :: Lens.Lens' CreateImagePipeline (Prelude.Maybe Prelude.Text)
createImagePipeline_description = Lens.lens (\CreateImagePipeline' {description} -> description) (\s@CreateImagePipeline' {} a -> s {description = a} :: CreateImagePipeline)

-- | The tags of the image pipeline.
createImagePipeline_tags :: Lens.Lens' CreateImagePipeline (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createImagePipeline_tags = Lens.lens (\CreateImagePipeline' {tags} -> tags) (\s@CreateImagePipeline' {} a -> s {tags = a} :: CreateImagePipeline) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image pipeline.
createImagePipeline_name :: Lens.Lens' CreateImagePipeline Prelude.Text
createImagePipeline_name = Lens.lens (\CreateImagePipeline' {name} -> name) (\s@CreateImagePipeline' {} a -> s {name = a} :: CreateImagePipeline)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- will be used to build images created by this image pipeline.
createImagePipeline_infrastructureConfigurationArn :: Lens.Lens' CreateImagePipeline Prelude.Text
createImagePipeline_infrastructureConfigurationArn = Lens.lens (\CreateImagePipeline' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@CreateImagePipeline' {} a -> s {infrastructureConfigurationArn = a} :: CreateImagePipeline)

-- | The idempotency token used to make this request idempotent.
createImagePipeline_clientToken :: Lens.Lens' CreateImagePipeline Prelude.Text
createImagePipeline_clientToken = Lens.lens (\CreateImagePipeline' {clientToken} -> clientToken) (\s@CreateImagePipeline' {} a -> s {clientToken = a} :: CreateImagePipeline)

instance Core.AWSRequest CreateImagePipeline where
  type
    AWSResponse CreateImagePipeline =
      CreateImagePipelineResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImagePipelineResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "imagePipelineArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImagePipeline

instance Prelude.NFData CreateImagePipeline

instance Core.ToHeaders CreateImagePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateImagePipeline where
  toJSON CreateImagePipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("containerRecipeArn" Core..=)
              Prelude.<$> containerRecipeArn,
            ("imageTestsConfiguration" Core..=)
              Prelude.<$> imageTestsConfiguration,
            ("schedule" Core..=) Prelude.<$> schedule,
            ("enhancedImageMetadataEnabled" Core..=)
              Prelude.<$> enhancedImageMetadataEnabled,
            ("distributionConfigurationArn" Core..=)
              Prelude.<$> distributionConfigurationArn,
            ("imageRecipeArn" Core..=)
              Prelude.<$> imageRecipeArn,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ( "infrastructureConfigurationArn"
                  Core..= infrastructureConfigurationArn
              ),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateImagePipeline where
  toPath = Prelude.const "/CreateImagePipeline"

instance Core.ToQuery CreateImagePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImagePipelineResponse' smart constructor.
data CreateImagePipelineResponse = CreateImagePipelineResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image pipeline that was created by
    -- this request.
    imagePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImagePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createImagePipelineResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'clientToken', 'createImagePipelineResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'imagePipelineArn', 'createImagePipelineResponse_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that was created by
-- this request.
--
-- 'httpStatus', 'createImagePipelineResponse_httpStatus' - The response's http status code.
newCreateImagePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImagePipelineResponse
newCreateImagePipelineResponse pHttpStatus_ =
  CreateImagePipelineResponse'
    { requestId =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      imagePipelineArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
createImagePipelineResponse_requestId :: Lens.Lens' CreateImagePipelineResponse (Prelude.Maybe Prelude.Text)
createImagePipelineResponse_requestId = Lens.lens (\CreateImagePipelineResponse' {requestId} -> requestId) (\s@CreateImagePipelineResponse' {} a -> s {requestId = a} :: CreateImagePipelineResponse)

-- | The idempotency token used to make this request idempotent.
createImagePipelineResponse_clientToken :: Lens.Lens' CreateImagePipelineResponse (Prelude.Maybe Prelude.Text)
createImagePipelineResponse_clientToken = Lens.lens (\CreateImagePipelineResponse' {clientToken} -> clientToken) (\s@CreateImagePipelineResponse' {} a -> s {clientToken = a} :: CreateImagePipelineResponse)

-- | The Amazon Resource Name (ARN) of the image pipeline that was created by
-- this request.
createImagePipelineResponse_imagePipelineArn :: Lens.Lens' CreateImagePipelineResponse (Prelude.Maybe Prelude.Text)
createImagePipelineResponse_imagePipelineArn = Lens.lens (\CreateImagePipelineResponse' {imagePipelineArn} -> imagePipelineArn) (\s@CreateImagePipelineResponse' {} a -> s {imagePipelineArn = a} :: CreateImagePipelineResponse)

-- | The response's http status code.
createImagePipelineResponse_httpStatus :: Lens.Lens' CreateImagePipelineResponse Prelude.Int
createImagePipelineResponse_httpStatus = Lens.lens (\CreateImagePipelineResponse' {httpStatus} -> httpStatus) (\s@CreateImagePipelineResponse' {} a -> s {httpStatus = a} :: CreateImagePipelineResponse)

instance Prelude.NFData CreateImagePipelineResponse
