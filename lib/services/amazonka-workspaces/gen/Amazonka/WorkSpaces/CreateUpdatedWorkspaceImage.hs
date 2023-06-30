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
-- Module      : Amazonka.WorkSpaces.CreateUpdatedWorkspaceImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new updated WorkSpace image based on the specified source
-- image. The new updated WorkSpace image has the latest drivers and other
-- updates required by the Amazon WorkSpaces components.
--
-- To determine which WorkSpace images need to be updated with the latest
-- Amazon WorkSpaces requirements, use
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImages.html DescribeWorkspaceImages>.
--
-- -   Only Windows 10, Windows Sever 2016, and Windows Server 2019
--     WorkSpace images can be programmatically updated at this time.
--
-- -   Microsoft Windows updates and other application updates are not
--     included in the update process.
--
-- -   The source WorkSpace image is not deleted. You can delete the source
--     image after you\'ve verified your new updated image and created a
--     new bundle.
module Amazonka.WorkSpaces.CreateUpdatedWorkspaceImage
  ( -- * Creating a Request
    CreateUpdatedWorkspaceImage (..),
    newCreateUpdatedWorkspaceImage,

    -- * Request Lenses
    createUpdatedWorkspaceImage_tags,
    createUpdatedWorkspaceImage_name,
    createUpdatedWorkspaceImage_description,
    createUpdatedWorkspaceImage_sourceImageId,

    -- * Destructuring the Response
    CreateUpdatedWorkspaceImageResponse (..),
    newCreateUpdatedWorkspaceImageResponse,

    -- * Response Lenses
    createUpdatedWorkspaceImageResponse_imageId,
    createUpdatedWorkspaceImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCreateUpdatedWorkspaceImage' smart constructor.
data CreateUpdatedWorkspaceImage = CreateUpdatedWorkspaceImage'
  { -- | The tags that you want to add to the new updated WorkSpace image.
    --
    -- To add tags at the same time when you\'re creating the updated image,
    -- you must create an IAM policy that grants your IAM user permissions to
    -- use @workspaces:CreateTags@.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the new updated WorkSpace image.
    name :: Prelude.Text,
    -- | A description of whether updates for the WorkSpace image are available.
    description :: Prelude.Text,
    -- | The identifier of the source WorkSpace image.
    sourceImageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUpdatedWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createUpdatedWorkspaceImage_tags' - The tags that you want to add to the new updated WorkSpace image.
--
-- To add tags at the same time when you\'re creating the updated image,
-- you must create an IAM policy that grants your IAM user permissions to
-- use @workspaces:CreateTags@.
--
-- 'name', 'createUpdatedWorkspaceImage_name' - The name of the new updated WorkSpace image.
--
-- 'description', 'createUpdatedWorkspaceImage_description' - A description of whether updates for the WorkSpace image are available.
--
-- 'sourceImageId', 'createUpdatedWorkspaceImage_sourceImageId' - The identifier of the source WorkSpace image.
newCreateUpdatedWorkspaceImage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'sourceImageId'
  Prelude.Text ->
  CreateUpdatedWorkspaceImage
newCreateUpdatedWorkspaceImage
  pName_
  pDescription_
  pSourceImageId_ =
    CreateUpdatedWorkspaceImage'
      { tags =
          Prelude.Nothing,
        name = pName_,
        description = pDescription_,
        sourceImageId = pSourceImageId_
      }

-- | The tags that you want to add to the new updated WorkSpace image.
--
-- To add tags at the same time when you\'re creating the updated image,
-- you must create an IAM policy that grants your IAM user permissions to
-- use @workspaces:CreateTags@.
createUpdatedWorkspaceImage_tags :: Lens.Lens' CreateUpdatedWorkspaceImage (Prelude.Maybe [Tag])
createUpdatedWorkspaceImage_tags = Lens.lens (\CreateUpdatedWorkspaceImage' {tags} -> tags) (\s@CreateUpdatedWorkspaceImage' {} a -> s {tags = a} :: CreateUpdatedWorkspaceImage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new updated WorkSpace image.
createUpdatedWorkspaceImage_name :: Lens.Lens' CreateUpdatedWorkspaceImage Prelude.Text
createUpdatedWorkspaceImage_name = Lens.lens (\CreateUpdatedWorkspaceImage' {name} -> name) (\s@CreateUpdatedWorkspaceImage' {} a -> s {name = a} :: CreateUpdatedWorkspaceImage)

-- | A description of whether updates for the WorkSpace image are available.
createUpdatedWorkspaceImage_description :: Lens.Lens' CreateUpdatedWorkspaceImage Prelude.Text
createUpdatedWorkspaceImage_description = Lens.lens (\CreateUpdatedWorkspaceImage' {description} -> description) (\s@CreateUpdatedWorkspaceImage' {} a -> s {description = a} :: CreateUpdatedWorkspaceImage)

-- | The identifier of the source WorkSpace image.
createUpdatedWorkspaceImage_sourceImageId :: Lens.Lens' CreateUpdatedWorkspaceImage Prelude.Text
createUpdatedWorkspaceImage_sourceImageId = Lens.lens (\CreateUpdatedWorkspaceImage' {sourceImageId} -> sourceImageId) (\s@CreateUpdatedWorkspaceImage' {} a -> s {sourceImageId = a} :: CreateUpdatedWorkspaceImage)

instance Core.AWSRequest CreateUpdatedWorkspaceImage where
  type
    AWSResponse CreateUpdatedWorkspaceImage =
      CreateUpdatedWorkspaceImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUpdatedWorkspaceImageResponse'
            Prelude.<$> (x Data..?> "ImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUpdatedWorkspaceImage where
  hashWithSalt _salt CreateUpdatedWorkspaceImage' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceImageId

instance Prelude.NFData CreateUpdatedWorkspaceImage where
  rnf CreateUpdatedWorkspaceImage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceImageId

instance Data.ToHeaders CreateUpdatedWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CreateUpdatedWorkspaceImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUpdatedWorkspaceImage where
  toJSON CreateUpdatedWorkspaceImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just
              ("SourceImageId" Data..= sourceImageId)
          ]
      )

instance Data.ToPath CreateUpdatedWorkspaceImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUpdatedWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUpdatedWorkspaceImageResponse' smart constructor.
data CreateUpdatedWorkspaceImageResponse = CreateUpdatedWorkspaceImageResponse'
  { -- | The identifier of the new updated WorkSpace image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUpdatedWorkspaceImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'createUpdatedWorkspaceImageResponse_imageId' - The identifier of the new updated WorkSpace image.
--
-- 'httpStatus', 'createUpdatedWorkspaceImageResponse_httpStatus' - The response's http status code.
newCreateUpdatedWorkspaceImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUpdatedWorkspaceImageResponse
newCreateUpdatedWorkspaceImageResponse pHttpStatus_ =
  CreateUpdatedWorkspaceImageResponse'
    { imageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the new updated WorkSpace image.
createUpdatedWorkspaceImageResponse_imageId :: Lens.Lens' CreateUpdatedWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createUpdatedWorkspaceImageResponse_imageId = Lens.lens (\CreateUpdatedWorkspaceImageResponse' {imageId} -> imageId) (\s@CreateUpdatedWorkspaceImageResponse' {} a -> s {imageId = a} :: CreateUpdatedWorkspaceImageResponse)

-- | The response's http status code.
createUpdatedWorkspaceImageResponse_httpStatus :: Lens.Lens' CreateUpdatedWorkspaceImageResponse Prelude.Int
createUpdatedWorkspaceImageResponse_httpStatus = Lens.lens (\CreateUpdatedWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@CreateUpdatedWorkspaceImageResponse' {} a -> s {httpStatus = a} :: CreateUpdatedWorkspaceImageResponse)

instance
  Prelude.NFData
    CreateUpdatedWorkspaceImageResponse
  where
  rnf CreateUpdatedWorkspaceImageResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
