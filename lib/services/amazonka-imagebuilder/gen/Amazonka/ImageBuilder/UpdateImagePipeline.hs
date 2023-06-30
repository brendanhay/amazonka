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
-- Module      : Amazonka.ImageBuilder.UpdateImagePipeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an image pipeline. Image pipelines enable you to automate the
-- creation and distribution of images.
--
-- UpdateImagePipeline does not support selective updates for the pipeline.
-- You must specify all of the required properties in the update request,
-- not just the properties that have changed.
module Amazonka.ImageBuilder.UpdateImagePipeline
  ( -- * Creating a Request
    UpdateImagePipeline (..),
    newUpdateImagePipeline,

    -- * Request Lenses
    updateImagePipeline_containerRecipeArn,
    updateImagePipeline_description,
    updateImagePipeline_distributionConfigurationArn,
    updateImagePipeline_enhancedImageMetadataEnabled,
    updateImagePipeline_imageRecipeArn,
    updateImagePipeline_imageTestsConfiguration,
    updateImagePipeline_schedule,
    updateImagePipeline_status,
    updateImagePipeline_imagePipelineArn,
    updateImagePipeline_infrastructureConfigurationArn,
    updateImagePipeline_clientToken,

    -- * Destructuring the Response
    UpdateImagePipelineResponse (..),
    newUpdateImagePipelineResponse,

    -- * Response Lenses
    updateImagePipelineResponse_clientToken,
    updateImagePipelineResponse_imagePipelineArn,
    updateImagePipelineResponse_requestId,
    updateImagePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateImagePipeline' smart constructor.
data UpdateImagePipeline = UpdateImagePipeline'
  { -- | The Amazon Resource Name (ARN) of the container pipeline to update.
    containerRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the image pipeline.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the distribution configuration that
    -- will be used to configure and distribute images updated by this image
    -- pipeline.
    distributionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | Collects additional information about the image being created, including
    -- the operating system (OS) version and package list. This information is
    -- used to enhance the overall experience of using EC2 Image Builder.
    -- Enabled by default.
    enhancedImageMetadataEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the image recipe that will be used to
    -- configure images updated by this image pipeline.
    imageRecipeArn :: Prelude.Maybe Prelude.Text,
    -- | The image test configuration of the image pipeline.
    imageTestsConfiguration :: Prelude.Maybe ImageTestsConfiguration,
    -- | The schedule of the image pipeline.
    schedule :: Prelude.Maybe Schedule,
    -- | The status of the image pipeline.
    status :: Prelude.Maybe PipelineStatus,
    -- | The Amazon Resource Name (ARN) of the image pipeline that you want to
    -- update.
    imagePipelineArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- will be used to build images updated by this image pipeline.
    infrastructureConfigurationArn :: Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImagePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerRecipeArn', 'updateImagePipeline_containerRecipeArn' - The Amazon Resource Name (ARN) of the container pipeline to update.
--
-- 'description', 'updateImagePipeline_description' - The description of the image pipeline.
--
-- 'distributionConfigurationArn', 'updateImagePipeline_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration that
-- will be used to configure and distribute images updated by this image
-- pipeline.
--
-- 'enhancedImageMetadataEnabled', 'updateImagePipeline_enhancedImageMetadataEnabled' - Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
--
-- 'imageRecipeArn', 'updateImagePipeline_imageRecipeArn' - The Amazon Resource Name (ARN) of the image recipe that will be used to
-- configure images updated by this image pipeline.
--
-- 'imageTestsConfiguration', 'updateImagePipeline_imageTestsConfiguration' - The image test configuration of the image pipeline.
--
-- 'schedule', 'updateImagePipeline_schedule' - The schedule of the image pipeline.
--
-- 'status', 'updateImagePipeline_status' - The status of the image pipeline.
--
-- 'imagePipelineArn', 'updateImagePipeline_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that you want to
-- update.
--
-- 'infrastructureConfigurationArn', 'updateImagePipeline_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- will be used to build images updated by this image pipeline.
--
-- 'clientToken', 'updateImagePipeline_clientToken' - The idempotency token used to make this request idempotent.
newUpdateImagePipeline ::
  -- | 'imagePipelineArn'
  Prelude.Text ->
  -- | 'infrastructureConfigurationArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateImagePipeline
newUpdateImagePipeline
  pImagePipelineArn_
  pInfrastructureConfigurationArn_
  pClientToken_ =
    UpdateImagePipeline'
      { containerRecipeArn =
          Prelude.Nothing,
        description = Prelude.Nothing,
        distributionConfigurationArn = Prelude.Nothing,
        enhancedImageMetadataEnabled = Prelude.Nothing,
        imageRecipeArn = Prelude.Nothing,
        imageTestsConfiguration = Prelude.Nothing,
        schedule = Prelude.Nothing,
        status = Prelude.Nothing,
        imagePipelineArn = pImagePipelineArn_,
        infrastructureConfigurationArn =
          pInfrastructureConfigurationArn_,
        clientToken = pClientToken_
      }

-- | The Amazon Resource Name (ARN) of the container pipeline to update.
updateImagePipeline_containerRecipeArn :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe Prelude.Text)
updateImagePipeline_containerRecipeArn = Lens.lens (\UpdateImagePipeline' {containerRecipeArn} -> containerRecipeArn) (\s@UpdateImagePipeline' {} a -> s {containerRecipeArn = a} :: UpdateImagePipeline)

-- | The description of the image pipeline.
updateImagePipeline_description :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe Prelude.Text)
updateImagePipeline_description = Lens.lens (\UpdateImagePipeline' {description} -> description) (\s@UpdateImagePipeline' {} a -> s {description = a} :: UpdateImagePipeline)

-- | The Amazon Resource Name (ARN) of the distribution configuration that
-- will be used to configure and distribute images updated by this image
-- pipeline.
updateImagePipeline_distributionConfigurationArn :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe Prelude.Text)
updateImagePipeline_distributionConfigurationArn = Lens.lens (\UpdateImagePipeline' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@UpdateImagePipeline' {} a -> s {distributionConfigurationArn = a} :: UpdateImagePipeline)

-- | Collects additional information about the image being created, including
-- the operating system (OS) version and package list. This information is
-- used to enhance the overall experience of using EC2 Image Builder.
-- Enabled by default.
updateImagePipeline_enhancedImageMetadataEnabled :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe Prelude.Bool)
updateImagePipeline_enhancedImageMetadataEnabled = Lens.lens (\UpdateImagePipeline' {enhancedImageMetadataEnabled} -> enhancedImageMetadataEnabled) (\s@UpdateImagePipeline' {} a -> s {enhancedImageMetadataEnabled = a} :: UpdateImagePipeline)

-- | The Amazon Resource Name (ARN) of the image recipe that will be used to
-- configure images updated by this image pipeline.
updateImagePipeline_imageRecipeArn :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe Prelude.Text)
updateImagePipeline_imageRecipeArn = Lens.lens (\UpdateImagePipeline' {imageRecipeArn} -> imageRecipeArn) (\s@UpdateImagePipeline' {} a -> s {imageRecipeArn = a} :: UpdateImagePipeline)

-- | The image test configuration of the image pipeline.
updateImagePipeline_imageTestsConfiguration :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe ImageTestsConfiguration)
updateImagePipeline_imageTestsConfiguration = Lens.lens (\UpdateImagePipeline' {imageTestsConfiguration} -> imageTestsConfiguration) (\s@UpdateImagePipeline' {} a -> s {imageTestsConfiguration = a} :: UpdateImagePipeline)

-- | The schedule of the image pipeline.
updateImagePipeline_schedule :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe Schedule)
updateImagePipeline_schedule = Lens.lens (\UpdateImagePipeline' {schedule} -> schedule) (\s@UpdateImagePipeline' {} a -> s {schedule = a} :: UpdateImagePipeline)

-- | The status of the image pipeline.
updateImagePipeline_status :: Lens.Lens' UpdateImagePipeline (Prelude.Maybe PipelineStatus)
updateImagePipeline_status = Lens.lens (\UpdateImagePipeline' {status} -> status) (\s@UpdateImagePipeline' {} a -> s {status = a} :: UpdateImagePipeline)

-- | The Amazon Resource Name (ARN) of the image pipeline that you want to
-- update.
updateImagePipeline_imagePipelineArn :: Lens.Lens' UpdateImagePipeline Prelude.Text
updateImagePipeline_imagePipelineArn = Lens.lens (\UpdateImagePipeline' {imagePipelineArn} -> imagePipelineArn) (\s@UpdateImagePipeline' {} a -> s {imagePipelineArn = a} :: UpdateImagePipeline)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- will be used to build images updated by this image pipeline.
updateImagePipeline_infrastructureConfigurationArn :: Lens.Lens' UpdateImagePipeline Prelude.Text
updateImagePipeline_infrastructureConfigurationArn = Lens.lens (\UpdateImagePipeline' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@UpdateImagePipeline' {} a -> s {infrastructureConfigurationArn = a} :: UpdateImagePipeline)

-- | The idempotency token used to make this request idempotent.
updateImagePipeline_clientToken :: Lens.Lens' UpdateImagePipeline Prelude.Text
updateImagePipeline_clientToken = Lens.lens (\UpdateImagePipeline' {clientToken} -> clientToken) (\s@UpdateImagePipeline' {} a -> s {clientToken = a} :: UpdateImagePipeline)

instance Core.AWSRequest UpdateImagePipeline where
  type
    AWSResponse UpdateImagePipeline =
      UpdateImagePipelineResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateImagePipelineResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "imagePipelineArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateImagePipeline where
  hashWithSalt _salt UpdateImagePipeline' {..} =
    _salt
      `Prelude.hashWithSalt` containerRecipeArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` distributionConfigurationArn
      `Prelude.hashWithSalt` enhancedImageMetadataEnabled
      `Prelude.hashWithSalt` imageRecipeArn
      `Prelude.hashWithSalt` imageTestsConfiguration
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` imagePipelineArn
      `Prelude.hashWithSalt` infrastructureConfigurationArn
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData UpdateImagePipeline where
  rnf UpdateImagePipeline' {..} =
    Prelude.rnf containerRecipeArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf distributionConfigurationArn
      `Prelude.seq` Prelude.rnf enhancedImageMetadataEnabled
      `Prelude.seq` Prelude.rnf imageRecipeArn
      `Prelude.seq` Prelude.rnf imageTestsConfiguration
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf infrastructureConfigurationArn
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders UpdateImagePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateImagePipeline where
  toJSON UpdateImagePipeline' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerRecipeArn" Data..=)
              Prelude.<$> containerRecipeArn,
            ("description" Data..=) Prelude.<$> description,
            ("distributionConfigurationArn" Data..=)
              Prelude.<$> distributionConfigurationArn,
            ("enhancedImageMetadataEnabled" Data..=)
              Prelude.<$> enhancedImageMetadataEnabled,
            ("imageRecipeArn" Data..=)
              Prelude.<$> imageRecipeArn,
            ("imageTestsConfiguration" Data..=)
              Prelude.<$> imageTestsConfiguration,
            ("schedule" Data..=) Prelude.<$> schedule,
            ("status" Data..=) Prelude.<$> status,
            Prelude.Just
              ("imagePipelineArn" Data..= imagePipelineArn),
            Prelude.Just
              ( "infrastructureConfigurationArn"
                  Data..= infrastructureConfigurationArn
              ),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath UpdateImagePipeline where
  toPath = Prelude.const "/UpdateImagePipeline"

instance Data.ToQuery UpdateImagePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateImagePipelineResponse' smart constructor.
data UpdateImagePipelineResponse = UpdateImagePipelineResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the image pipeline that was updated by
    -- this request.
    imagePipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImagePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateImagePipelineResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'imagePipelineArn', 'updateImagePipelineResponse_imagePipelineArn' - The Amazon Resource Name (ARN) of the image pipeline that was updated by
-- this request.
--
-- 'requestId', 'updateImagePipelineResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'updateImagePipelineResponse_httpStatus' - The response's http status code.
newUpdateImagePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateImagePipelineResponse
newUpdateImagePipelineResponse pHttpStatus_ =
  UpdateImagePipelineResponse'
    { clientToken =
        Prelude.Nothing,
      imagePipelineArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token used to make this request idempotent.
updateImagePipelineResponse_clientToken :: Lens.Lens' UpdateImagePipelineResponse (Prelude.Maybe Prelude.Text)
updateImagePipelineResponse_clientToken = Lens.lens (\UpdateImagePipelineResponse' {clientToken} -> clientToken) (\s@UpdateImagePipelineResponse' {} a -> s {clientToken = a} :: UpdateImagePipelineResponse)

-- | The Amazon Resource Name (ARN) of the image pipeline that was updated by
-- this request.
updateImagePipelineResponse_imagePipelineArn :: Lens.Lens' UpdateImagePipelineResponse (Prelude.Maybe Prelude.Text)
updateImagePipelineResponse_imagePipelineArn = Lens.lens (\UpdateImagePipelineResponse' {imagePipelineArn} -> imagePipelineArn) (\s@UpdateImagePipelineResponse' {} a -> s {imagePipelineArn = a} :: UpdateImagePipelineResponse)

-- | The request ID that uniquely identifies this request.
updateImagePipelineResponse_requestId :: Lens.Lens' UpdateImagePipelineResponse (Prelude.Maybe Prelude.Text)
updateImagePipelineResponse_requestId = Lens.lens (\UpdateImagePipelineResponse' {requestId} -> requestId) (\s@UpdateImagePipelineResponse' {} a -> s {requestId = a} :: UpdateImagePipelineResponse)

-- | The response's http status code.
updateImagePipelineResponse_httpStatus :: Lens.Lens' UpdateImagePipelineResponse Prelude.Int
updateImagePipelineResponse_httpStatus = Lens.lens (\UpdateImagePipelineResponse' {httpStatus} -> httpStatus) (\s@UpdateImagePipelineResponse' {} a -> s {httpStatus = a} :: UpdateImagePipelineResponse)

instance Prelude.NFData UpdateImagePipelineResponse where
  rnf UpdateImagePipelineResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf imagePipelineArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
