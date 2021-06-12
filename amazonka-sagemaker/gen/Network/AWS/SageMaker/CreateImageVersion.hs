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
-- Module      : Network.AWS.SageMaker.CreateImageVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of the SageMaker image specified by @ImageName@. The
-- version represents the Amazon Container Registry (ECR) container image
-- specified by @BaseImage@.
module Network.AWS.SageMaker.CreateImageVersion
  ( -- * Creating a Request
    CreateImageVersion (..),
    newCreateImageVersion,

    -- * Request Lenses
    createImageVersion_baseImage,
    createImageVersion_clientToken,
    createImageVersion_imageName,

    -- * Destructuring the Response
    CreateImageVersionResponse (..),
    newCreateImageVersionResponse,

    -- * Response Lenses
    createImageVersionResponse_imageVersionArn,
    createImageVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateImageVersion' smart constructor.
data CreateImageVersion = CreateImageVersion'
  { -- | The registry path of the container image to use as the starting point
    -- for this version. The path is an Amazon Container Registry (ECR) URI in
    -- the following format:
    --
    -- @\<acct-id>.dkr.ecr.\<region>.amazonaws.com\/\<repo-name[:tag] or [\@digest]>@
    baseImage :: Core.Text,
    -- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK
    -- for Python (Boto3), add a unique value to the call.
    clientToken :: Core.Text,
    -- | The @ImageName@ of the @Image@ to create a version of.
    imageName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseImage', 'createImageVersion_baseImage' - The registry path of the container image to use as the starting point
-- for this version. The path is an Amazon Container Registry (ECR) URI in
-- the following format:
--
-- @\<acct-id>.dkr.ecr.\<region>.amazonaws.com\/\<repo-name[:tag] or [\@digest]>@
--
-- 'clientToken', 'createImageVersion_clientToken' - A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK
-- for Python (Boto3), add a unique value to the call.
--
-- 'imageName', 'createImageVersion_imageName' - The @ImageName@ of the @Image@ to create a version of.
newCreateImageVersion ::
  -- | 'baseImage'
  Core.Text ->
  -- | 'clientToken'
  Core.Text ->
  -- | 'imageName'
  Core.Text ->
  CreateImageVersion
newCreateImageVersion
  pBaseImage_
  pClientToken_
  pImageName_ =
    CreateImageVersion'
      { baseImage = pBaseImage_,
        clientToken = pClientToken_,
        imageName = pImageName_
      }

-- | The registry path of the container image to use as the starting point
-- for this version. The path is an Amazon Container Registry (ECR) URI in
-- the following format:
--
-- @\<acct-id>.dkr.ecr.\<region>.amazonaws.com\/\<repo-name[:tag] or [\@digest]>@
createImageVersion_baseImage :: Lens.Lens' CreateImageVersion Core.Text
createImageVersion_baseImage = Lens.lens (\CreateImageVersion' {baseImage} -> baseImage) (\s@CreateImageVersion' {} a -> s {baseImage = a} :: CreateImageVersion)

-- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK
-- for Python (Boto3), add a unique value to the call.
createImageVersion_clientToken :: Lens.Lens' CreateImageVersion Core.Text
createImageVersion_clientToken = Lens.lens (\CreateImageVersion' {clientToken} -> clientToken) (\s@CreateImageVersion' {} a -> s {clientToken = a} :: CreateImageVersion)

-- | The @ImageName@ of the @Image@ to create a version of.
createImageVersion_imageName :: Lens.Lens' CreateImageVersion Core.Text
createImageVersion_imageName = Lens.lens (\CreateImageVersion' {imageName} -> imageName) (\s@CreateImageVersion' {} a -> s {imageName = a} :: CreateImageVersion)

instance Core.AWSRequest CreateImageVersion where
  type
    AWSResponse CreateImageVersion =
      CreateImageVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageVersionResponse'
            Core.<$> (x Core..?> "ImageVersionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateImageVersion

instance Core.NFData CreateImageVersion

instance Core.ToHeaders CreateImageVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateImageVersion" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateImageVersion where
  toJSON CreateImageVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BaseImage" Core..= baseImage),
            Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("ImageName" Core..= imageName)
          ]
      )

instance Core.ToPath CreateImageVersion where
  toPath = Core.const "/"

instance Core.ToQuery CreateImageVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateImageVersionResponse' smart constructor.
data CreateImageVersionResponse = CreateImageVersionResponse'
  { -- | The Amazon Resource Name (ARN) of the image version.
    imageVersionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageVersionArn', 'createImageVersionResponse_imageVersionArn' - The Amazon Resource Name (ARN) of the image version.
--
-- 'httpStatus', 'createImageVersionResponse_httpStatus' - The response's http status code.
newCreateImageVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateImageVersionResponse
newCreateImageVersionResponse pHttpStatus_ =
  CreateImageVersionResponse'
    { imageVersionArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the image version.
createImageVersionResponse_imageVersionArn :: Lens.Lens' CreateImageVersionResponse (Core.Maybe Core.Text)
createImageVersionResponse_imageVersionArn = Lens.lens (\CreateImageVersionResponse' {imageVersionArn} -> imageVersionArn) (\s@CreateImageVersionResponse' {} a -> s {imageVersionArn = a} :: CreateImageVersionResponse)

-- | The response's http status code.
createImageVersionResponse_httpStatus :: Lens.Lens' CreateImageVersionResponse Core.Int
createImageVersionResponse_httpStatus = Lens.lens (\CreateImageVersionResponse' {httpStatus} -> httpStatus) (\s@CreateImageVersionResponse' {} a -> s {httpStatus = a} :: CreateImageVersionResponse)

instance Core.NFData CreateImageVersionResponse
