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
-- Module      : Amazonka.SageMaker.CreateImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom SageMaker image. A SageMaker image is a set of image
-- versions. Each image version represents a container image stored in
-- Amazon Elastic Container Registry (ECR). For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image>.
module Amazonka.SageMaker.CreateImage
  ( -- * Creating a Request
    CreateImage (..),
    newCreateImage,

    -- * Request Lenses
    createImage_tags,
    createImage_displayName,
    createImage_description,
    createImage_imageName,
    createImage_roleArn,

    -- * Destructuring the Response
    CreateImageResponse (..),
    newCreateImageResponse,

    -- * Response Lenses
    createImageResponse_imageArn,
    createImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateImage' smart constructor.
data CreateImage = CreateImage'
  { -- | A list of tags to apply to the image.
    tags :: Prelude.Maybe [Tag],
    -- | The display name of the image. If not provided, @ImageName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the image. Must be unique to your account.
    imageName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createImage_tags' - A list of tags to apply to the image.
--
-- 'displayName', 'createImage_displayName' - The display name of the image. If not provided, @ImageName@ is
-- displayed.
--
-- 'description', 'createImage_description' - The description of the image.
--
-- 'imageName', 'createImage_imageName' - The name of the image. Must be unique to your account.
--
-- 'roleArn', 'createImage_roleArn' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
newCreateImage ::
  -- | 'imageName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateImage
newCreateImage pImageName_ pRoleArn_ =
  CreateImage'
    { tags = Prelude.Nothing,
      displayName = Prelude.Nothing,
      description = Prelude.Nothing,
      imageName = pImageName_,
      roleArn = pRoleArn_
    }

-- | A list of tags to apply to the image.
createImage_tags :: Lens.Lens' CreateImage (Prelude.Maybe [Tag])
createImage_tags = Lens.lens (\CreateImage' {tags} -> tags) (\s@CreateImage' {} a -> s {tags = a} :: CreateImage) Prelude.. Lens.mapping Lens.coerced

-- | The display name of the image. If not provided, @ImageName@ is
-- displayed.
createImage_displayName :: Lens.Lens' CreateImage (Prelude.Maybe Prelude.Text)
createImage_displayName = Lens.lens (\CreateImage' {displayName} -> displayName) (\s@CreateImage' {} a -> s {displayName = a} :: CreateImage)

-- | The description of the image.
createImage_description :: Lens.Lens' CreateImage (Prelude.Maybe Prelude.Text)
createImage_description = Lens.lens (\CreateImage' {description} -> description) (\s@CreateImage' {} a -> s {description = a} :: CreateImage)

-- | The name of the image. Must be unique to your account.
createImage_imageName :: Lens.Lens' CreateImage Prelude.Text
createImage_imageName = Lens.lens (\CreateImage' {imageName} -> imageName) (\s@CreateImage' {} a -> s {imageName = a} :: CreateImage)

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
createImage_roleArn :: Lens.Lens' CreateImage Prelude.Text
createImage_roleArn = Lens.lens (\CreateImage' {roleArn} -> roleArn) (\s@CreateImage' {} a -> s {roleArn = a} :: CreateImage)

instance Core.AWSRequest CreateImage where
  type AWSResponse CreateImage = CreateImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageResponse'
            Prelude.<$> (x Data..?> "ImageArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImage where
  hashWithSalt _salt CreateImage' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateImage where
  rnf CreateImage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf imageName
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateImage" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateImage where
  toJSON CreateImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("ImageName" Data..= imageName),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { -- | The Amazon Resource Name (ARN) of the image.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageArn', 'createImageResponse_imageArn' - The Amazon Resource Name (ARN) of the image.
--
-- 'httpStatus', 'createImageResponse_httpStatus' - The response's http status code.
newCreateImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImageResponse
newCreateImageResponse pHttpStatus_ =
  CreateImageResponse'
    { imageArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the image.
createImageResponse_imageArn :: Lens.Lens' CreateImageResponse (Prelude.Maybe Prelude.Text)
createImageResponse_imageArn = Lens.lens (\CreateImageResponse' {imageArn} -> imageArn) (\s@CreateImageResponse' {} a -> s {imageArn = a} :: CreateImageResponse)

-- | The response's http status code.
createImageResponse_httpStatus :: Lens.Lens' CreateImageResponse Prelude.Int
createImageResponse_httpStatus = Lens.lens (\CreateImageResponse' {httpStatus} -> httpStatus) (\s@CreateImageResponse' {} a -> s {httpStatus = a} :: CreateImageResponse)

instance Prelude.NFData CreateImageResponse where
  rnf CreateImageResponse' {..} =
    Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf httpStatus
