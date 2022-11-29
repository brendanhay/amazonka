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
-- Module      : Amazonka.WorkSpaces.CreateWorkspaceImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new WorkSpace image from an existing WorkSpace.
module Amazonka.WorkSpaces.CreateWorkspaceImage
  ( -- * Creating a Request
    CreateWorkspaceImage (..),
    newCreateWorkspaceImage,

    -- * Request Lenses
    createWorkspaceImage_tags,
    createWorkspaceImage_name,
    createWorkspaceImage_description,
    createWorkspaceImage_workspaceId,

    -- * Destructuring the Response
    CreateWorkspaceImageResponse (..),
    newCreateWorkspaceImageResponse,

    -- * Response Lenses
    createWorkspaceImageResponse_operatingSystem,
    createWorkspaceImageResponse_name,
    createWorkspaceImageResponse_created,
    createWorkspaceImageResponse_state,
    createWorkspaceImageResponse_description,
    createWorkspaceImageResponse_requiredTenancy,
    createWorkspaceImageResponse_ownerAccountId,
    createWorkspaceImageResponse_imageId,
    createWorkspaceImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCreateWorkspaceImage' smart constructor.
data CreateWorkspaceImage = CreateWorkspaceImage'
  { -- | The tags that you want to add to the new WorkSpace image. To add tags
    -- when you\'re creating the image, you must create an IAM policy that
    -- grants your IAM user permission to use @workspaces:CreateTags@.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the new WorkSpace image.
    name :: Prelude.Text,
    -- | The description of the new WorkSpace image.
    description :: Prelude.Text,
    -- | The identifier of the source WorkSpace
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorkspaceImage_tags' - The tags that you want to add to the new WorkSpace image. To add tags
-- when you\'re creating the image, you must create an IAM policy that
-- grants your IAM user permission to use @workspaces:CreateTags@.
--
-- 'name', 'createWorkspaceImage_name' - The name of the new WorkSpace image.
--
-- 'description', 'createWorkspaceImage_description' - The description of the new WorkSpace image.
--
-- 'workspaceId', 'createWorkspaceImage_workspaceId' - The identifier of the source WorkSpace
newCreateWorkspaceImage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  CreateWorkspaceImage
newCreateWorkspaceImage
  pName_
  pDescription_
  pWorkspaceId_ =
    CreateWorkspaceImage'
      { tags = Prelude.Nothing,
        name = pName_,
        description = pDescription_,
        workspaceId = pWorkspaceId_
      }

-- | The tags that you want to add to the new WorkSpace image. To add tags
-- when you\'re creating the image, you must create an IAM policy that
-- grants your IAM user permission to use @workspaces:CreateTags@.
createWorkspaceImage_tags :: Lens.Lens' CreateWorkspaceImage (Prelude.Maybe [Tag])
createWorkspaceImage_tags = Lens.lens (\CreateWorkspaceImage' {tags} -> tags) (\s@CreateWorkspaceImage' {} a -> s {tags = a} :: CreateWorkspaceImage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new WorkSpace image.
createWorkspaceImage_name :: Lens.Lens' CreateWorkspaceImage Prelude.Text
createWorkspaceImage_name = Lens.lens (\CreateWorkspaceImage' {name} -> name) (\s@CreateWorkspaceImage' {} a -> s {name = a} :: CreateWorkspaceImage)

-- | The description of the new WorkSpace image.
createWorkspaceImage_description :: Lens.Lens' CreateWorkspaceImage Prelude.Text
createWorkspaceImage_description = Lens.lens (\CreateWorkspaceImage' {description} -> description) (\s@CreateWorkspaceImage' {} a -> s {description = a} :: CreateWorkspaceImage)

-- | The identifier of the source WorkSpace
createWorkspaceImage_workspaceId :: Lens.Lens' CreateWorkspaceImage Prelude.Text
createWorkspaceImage_workspaceId = Lens.lens (\CreateWorkspaceImage' {workspaceId} -> workspaceId) (\s@CreateWorkspaceImage' {} a -> s {workspaceId = a} :: CreateWorkspaceImage)

instance Core.AWSRequest CreateWorkspaceImage where
  type
    AWSResponse CreateWorkspaceImage =
      CreateWorkspaceImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspaceImageResponse'
            Prelude.<$> (x Core..?> "OperatingSystem")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Created")
            Prelude.<*> (x Core..?> "State")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RequiredTenancy")
            Prelude.<*> (x Core..?> "OwnerAccountId")
            Prelude.<*> (x Core..?> "ImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkspaceImage where
  hashWithSalt _salt CreateWorkspaceImage' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData CreateWorkspaceImage where
  rnf CreateWorkspaceImage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders CreateWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.CreateWorkspaceImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorkspaceImage where
  toJSON CreateWorkspaceImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Description" Core..= description),
            Prelude.Just ("WorkspaceId" Core..= workspaceId)
          ]
      )

instance Core.ToPath CreateWorkspaceImage where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkspaceImageResponse' smart constructor.
data CreateWorkspaceImageResponse = CreateWorkspaceImageResponse'
  { -- | The operating system that the image is running.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date when the image was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The availability status of the image.
    state :: Prelude.Maybe WorkspaceImageState,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the image is running on dedicated hardware. When Bring
    -- Your Own License (BYOL) is enabled, this value is set to DEDICATED. For
    -- more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.htm Bring Your Own Windows Desktop Images.>.
    requiredTenancy :: Prelude.Maybe WorkspaceImageRequiredTenancy,
    -- | The identifier of the Amazon Web Services account that owns the image.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the new WorkSpace image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystem', 'createWorkspaceImageResponse_operatingSystem' - The operating system that the image is running.
--
-- 'name', 'createWorkspaceImageResponse_name' - The name of the image.
--
-- 'created', 'createWorkspaceImageResponse_created' - The date when the image was created.
--
-- 'state', 'createWorkspaceImageResponse_state' - The availability status of the image.
--
-- 'description', 'createWorkspaceImageResponse_description' - The description of the image.
--
-- 'requiredTenancy', 'createWorkspaceImageResponse_requiredTenancy' - Specifies whether the image is running on dedicated hardware. When Bring
-- Your Own License (BYOL) is enabled, this value is set to DEDICATED. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.htm Bring Your Own Windows Desktop Images.>.
--
-- 'ownerAccountId', 'createWorkspaceImageResponse_ownerAccountId' - The identifier of the Amazon Web Services account that owns the image.
--
-- 'imageId', 'createWorkspaceImageResponse_imageId' - The identifier of the new WorkSpace image.
--
-- 'httpStatus', 'createWorkspaceImageResponse_httpStatus' - The response's http status code.
newCreateWorkspaceImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkspaceImageResponse
newCreateWorkspaceImageResponse pHttpStatus_ =
  CreateWorkspaceImageResponse'
    { operatingSystem =
        Prelude.Nothing,
      name = Prelude.Nothing,
      created = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      requiredTenancy = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The operating system that the image is running.
createWorkspaceImageResponse_operatingSystem :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe OperatingSystem)
createWorkspaceImageResponse_operatingSystem = Lens.lens (\CreateWorkspaceImageResponse' {operatingSystem} -> operatingSystem) (\s@CreateWorkspaceImageResponse' {} a -> s {operatingSystem = a} :: CreateWorkspaceImageResponse)

-- | The name of the image.
createWorkspaceImageResponse_name :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_name = Lens.lens (\CreateWorkspaceImageResponse' {name} -> name) (\s@CreateWorkspaceImageResponse' {} a -> s {name = a} :: CreateWorkspaceImageResponse)

-- | The date when the image was created.
createWorkspaceImageResponse_created :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.UTCTime)
createWorkspaceImageResponse_created = Lens.lens (\CreateWorkspaceImageResponse' {created} -> created) (\s@CreateWorkspaceImageResponse' {} a -> s {created = a} :: CreateWorkspaceImageResponse) Prelude.. Lens.mapping Core._Time

-- | The availability status of the image.
createWorkspaceImageResponse_state :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe WorkspaceImageState)
createWorkspaceImageResponse_state = Lens.lens (\CreateWorkspaceImageResponse' {state} -> state) (\s@CreateWorkspaceImageResponse' {} a -> s {state = a} :: CreateWorkspaceImageResponse)

-- | The description of the image.
createWorkspaceImageResponse_description :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_description = Lens.lens (\CreateWorkspaceImageResponse' {description} -> description) (\s@CreateWorkspaceImageResponse' {} a -> s {description = a} :: CreateWorkspaceImageResponse)

-- | Specifies whether the image is running on dedicated hardware. When Bring
-- Your Own License (BYOL) is enabled, this value is set to DEDICATED. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.htm Bring Your Own Windows Desktop Images.>.
createWorkspaceImageResponse_requiredTenancy :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe WorkspaceImageRequiredTenancy)
createWorkspaceImageResponse_requiredTenancy = Lens.lens (\CreateWorkspaceImageResponse' {requiredTenancy} -> requiredTenancy) (\s@CreateWorkspaceImageResponse' {} a -> s {requiredTenancy = a} :: CreateWorkspaceImageResponse)

-- | The identifier of the Amazon Web Services account that owns the image.
createWorkspaceImageResponse_ownerAccountId :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_ownerAccountId = Lens.lens (\CreateWorkspaceImageResponse' {ownerAccountId} -> ownerAccountId) (\s@CreateWorkspaceImageResponse' {} a -> s {ownerAccountId = a} :: CreateWorkspaceImageResponse)

-- | The identifier of the new WorkSpace image.
createWorkspaceImageResponse_imageId :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_imageId = Lens.lens (\CreateWorkspaceImageResponse' {imageId} -> imageId) (\s@CreateWorkspaceImageResponse' {} a -> s {imageId = a} :: CreateWorkspaceImageResponse)

-- | The response's http status code.
createWorkspaceImageResponse_httpStatus :: Lens.Lens' CreateWorkspaceImageResponse Prelude.Int
createWorkspaceImageResponse_httpStatus = Lens.lens (\CreateWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspaceImageResponse' {} a -> s {httpStatus = a} :: CreateWorkspaceImageResponse)

instance Prelude.NFData CreateWorkspaceImageResponse where
  rnf CreateWorkspaceImageResponse' {..} =
    Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf requiredTenancy
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
