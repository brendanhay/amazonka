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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createWorkspaceImageResponse_created,
    createWorkspaceImageResponse_description,
    createWorkspaceImageResponse_imageId,
    createWorkspaceImageResponse_name,
    createWorkspaceImageResponse_operatingSystem,
    createWorkspaceImageResponse_ownerAccountId,
    createWorkspaceImageResponse_requiredTenancy,
    createWorkspaceImageResponse_state,
    createWorkspaceImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
            Prelude.<$> (x Data..?> "Created")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ImageId")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "OperatingSystem")
            Prelude.<*> (x Data..?> "OwnerAccountId")
            Prelude.<*> (x Data..?> "RequiredTenancy")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkspaceImage where
  hashWithSalt _salt CreateWorkspaceImage' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData CreateWorkspaceImage where
  rnf CreateWorkspaceImage' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders CreateWorkspaceImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CreateWorkspaceImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkspaceImage where
  toJSON CreateWorkspaceImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Description" Data..= description),
            Prelude.Just ("WorkspaceId" Data..= workspaceId)
          ]
      )

instance Data.ToPath CreateWorkspaceImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWorkspaceImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkspaceImageResponse' smart constructor.
data CreateWorkspaceImageResponse = CreateWorkspaceImageResponse'
  { -- | The date when the image was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The description of the image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the new WorkSpace image.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The name of the image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system that the image is running.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | The identifier of the Amazon Web Services account that owns the image.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the image is running on dedicated hardware. When Bring
    -- Your Own License (BYOL) is enabled, this value is set to DEDICATED. For
    -- more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.htm Bring Your Own Windows Desktop Images.>.
    requiredTenancy :: Prelude.Maybe WorkspaceImageRequiredTenancy,
    -- | The availability status of the image.
    state :: Prelude.Maybe WorkspaceImageState,
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
-- 'created', 'createWorkspaceImageResponse_created' - The date when the image was created.
--
-- 'description', 'createWorkspaceImageResponse_description' - The description of the image.
--
-- 'imageId', 'createWorkspaceImageResponse_imageId' - The identifier of the new WorkSpace image.
--
-- 'name', 'createWorkspaceImageResponse_name' - The name of the image.
--
-- 'operatingSystem', 'createWorkspaceImageResponse_operatingSystem' - The operating system that the image is running.
--
-- 'ownerAccountId', 'createWorkspaceImageResponse_ownerAccountId' - The identifier of the Amazon Web Services account that owns the image.
--
-- 'requiredTenancy', 'createWorkspaceImageResponse_requiredTenancy' - Specifies whether the image is running on dedicated hardware. When Bring
-- Your Own License (BYOL) is enabled, this value is set to DEDICATED. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.htm Bring Your Own Windows Desktop Images.>.
--
-- 'state', 'createWorkspaceImageResponse_state' - The availability status of the image.
--
-- 'httpStatus', 'createWorkspaceImageResponse_httpStatus' - The response's http status code.
newCreateWorkspaceImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkspaceImageResponse
newCreateWorkspaceImageResponse pHttpStatus_ =
  CreateWorkspaceImageResponse'
    { created =
        Prelude.Nothing,
      description = Prelude.Nothing,
      imageId = Prelude.Nothing,
      name = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      requiredTenancy = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date when the image was created.
createWorkspaceImageResponse_created :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.UTCTime)
createWorkspaceImageResponse_created = Lens.lens (\CreateWorkspaceImageResponse' {created} -> created) (\s@CreateWorkspaceImageResponse' {} a -> s {created = a} :: CreateWorkspaceImageResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the image.
createWorkspaceImageResponse_description :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_description = Lens.lens (\CreateWorkspaceImageResponse' {description} -> description) (\s@CreateWorkspaceImageResponse' {} a -> s {description = a} :: CreateWorkspaceImageResponse)

-- | The identifier of the new WorkSpace image.
createWorkspaceImageResponse_imageId :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_imageId = Lens.lens (\CreateWorkspaceImageResponse' {imageId} -> imageId) (\s@CreateWorkspaceImageResponse' {} a -> s {imageId = a} :: CreateWorkspaceImageResponse)

-- | The name of the image.
createWorkspaceImageResponse_name :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_name = Lens.lens (\CreateWorkspaceImageResponse' {name} -> name) (\s@CreateWorkspaceImageResponse' {} a -> s {name = a} :: CreateWorkspaceImageResponse)

-- | The operating system that the image is running.
createWorkspaceImageResponse_operatingSystem :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe OperatingSystem)
createWorkspaceImageResponse_operatingSystem = Lens.lens (\CreateWorkspaceImageResponse' {operatingSystem} -> operatingSystem) (\s@CreateWorkspaceImageResponse' {} a -> s {operatingSystem = a} :: CreateWorkspaceImageResponse)

-- | The identifier of the Amazon Web Services account that owns the image.
createWorkspaceImageResponse_ownerAccountId :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe Prelude.Text)
createWorkspaceImageResponse_ownerAccountId = Lens.lens (\CreateWorkspaceImageResponse' {ownerAccountId} -> ownerAccountId) (\s@CreateWorkspaceImageResponse' {} a -> s {ownerAccountId = a} :: CreateWorkspaceImageResponse)

-- | Specifies whether the image is running on dedicated hardware. When Bring
-- Your Own License (BYOL) is enabled, this value is set to DEDICATED. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.htm Bring Your Own Windows Desktop Images.>.
createWorkspaceImageResponse_requiredTenancy :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe WorkspaceImageRequiredTenancy)
createWorkspaceImageResponse_requiredTenancy = Lens.lens (\CreateWorkspaceImageResponse' {requiredTenancy} -> requiredTenancy) (\s@CreateWorkspaceImageResponse' {} a -> s {requiredTenancy = a} :: CreateWorkspaceImageResponse)

-- | The availability status of the image.
createWorkspaceImageResponse_state :: Lens.Lens' CreateWorkspaceImageResponse (Prelude.Maybe WorkspaceImageState)
createWorkspaceImageResponse_state = Lens.lens (\CreateWorkspaceImageResponse' {state} -> state) (\s@CreateWorkspaceImageResponse' {} a -> s {state = a} :: CreateWorkspaceImageResponse)

-- | The response's http status code.
createWorkspaceImageResponse_httpStatus :: Lens.Lens' CreateWorkspaceImageResponse Prelude.Int
createWorkspaceImageResponse_httpStatus = Lens.lens (\CreateWorkspaceImageResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspaceImageResponse' {} a -> s {httpStatus = a} :: CreateWorkspaceImageResponse)

instance Prelude.NFData CreateWorkspaceImageResponse where
  rnf CreateWorkspaceImageResponse' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf requiredTenancy
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
