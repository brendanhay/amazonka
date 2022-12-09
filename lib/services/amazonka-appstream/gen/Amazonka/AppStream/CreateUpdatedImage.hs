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
-- Module      : Amazonka.AppStream.CreateUpdatedImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new image with the latest Windows operating system updates,
-- driver updates, and AppStream 2.0 agent software.
--
-- For more information, see the \"Update an Image by Using Managed
-- AppStream 2.0 Image Updates\" section in
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/administer-images.html Administer Your AppStream 2.0 Images>,
-- in the /Amazon AppStream 2.0 Administration Guide/.
module Amazonka.AppStream.CreateUpdatedImage
  ( -- * Creating a Request
    CreateUpdatedImage (..),
    newCreateUpdatedImage,

    -- * Request Lenses
    createUpdatedImage_dryRun,
    createUpdatedImage_newImageDescription,
    createUpdatedImage_newImageDisplayName,
    createUpdatedImage_newImageTags,
    createUpdatedImage_existingImageName,
    createUpdatedImage_newImageName,

    -- * Destructuring the Response
    CreateUpdatedImageResponse (..),
    newCreateUpdatedImageResponse,

    -- * Response Lenses
    createUpdatedImageResponse_canUpdateImage,
    createUpdatedImageResponse_image,
    createUpdatedImageResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUpdatedImage' smart constructor.
data CreateUpdatedImage = CreateUpdatedImage'
  { -- | Indicates whether to display the status of image update availability
    -- before AppStream 2.0 initiates the process of creating a new updated
    -- image. If this value is set to @true@, AppStream 2.0 displays whether
    -- image updates are available. If this value is set to @false@, AppStream
    -- 2.0 initiates the process of creating a new updated image without
    -- displaying whether image updates are available.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The description to display for the new image.
    newImageDescription' :: Prelude.Maybe Prelude.Text,
    -- | The name to display for the new image.
    newImageDisplayName' :: Prelude.Maybe Prelude.Text,
    -- | The tags to associate with the new image. A tag is a key-value pair, and
    -- the value is optional. For example, Environment=Test. If you do not
    -- specify a value, Environment=.
    --
    -- Generally allowed characters are: letters, numbers, and spaces
    -- representable in UTF-8, and the following special characters:
    --
    -- _ . : \/ = + \\ - \@
    --
    -- If you do not specify a value, the value is set to an empty string.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
    -- in the /Amazon AppStream 2.0 Administration Guide/.
    newImageTags' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the image to update.
    existingImageName :: Prelude.Text,
    -- | The name of the new image. The name must be unique within the AWS
    -- account and Region.
    newImageName' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUpdatedImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createUpdatedImage_dryRun' - Indicates whether to display the status of image update availability
-- before AppStream 2.0 initiates the process of creating a new updated
-- image. If this value is set to @true@, AppStream 2.0 displays whether
-- image updates are available. If this value is set to @false@, AppStream
-- 2.0 initiates the process of creating a new updated image without
-- displaying whether image updates are available.
--
-- 'newImageDescription'', 'createUpdatedImage_newImageDescription' - The description to display for the new image.
--
-- 'newImageDisplayName'', 'createUpdatedImage_newImageDisplayName' - The name to display for the new image.
--
-- 'newImageTags'', 'createUpdatedImage_newImageTags' - The tags to associate with the new image. A tag is a key-value pair, and
-- the value is optional. For example, Environment=Test. If you do not
-- specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- If you do not specify a value, the value is set to an empty string.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
--
-- 'existingImageName', 'createUpdatedImage_existingImageName' - The name of the image to update.
--
-- 'newImageName'', 'createUpdatedImage_newImageName' - The name of the new image. The name must be unique within the AWS
-- account and Region.
newCreateUpdatedImage ::
  -- | 'existingImageName'
  Prelude.Text ->
  -- | 'newImageName''
  Prelude.Text ->
  CreateUpdatedImage
newCreateUpdatedImage
  pExistingImageName_
  pNewImageName_ =
    CreateUpdatedImage'
      { dryRun = Prelude.Nothing,
        newImageDescription' = Prelude.Nothing,
        newImageDisplayName' = Prelude.Nothing,
        newImageTags' = Prelude.Nothing,
        existingImageName = pExistingImageName_,
        newImageName' = pNewImageName_
      }

-- | Indicates whether to display the status of image update availability
-- before AppStream 2.0 initiates the process of creating a new updated
-- image. If this value is set to @true@, AppStream 2.0 displays whether
-- image updates are available. If this value is set to @false@, AppStream
-- 2.0 initiates the process of creating a new updated image without
-- displaying whether image updates are available.
createUpdatedImage_dryRun :: Lens.Lens' CreateUpdatedImage (Prelude.Maybe Prelude.Bool)
createUpdatedImage_dryRun = Lens.lens (\CreateUpdatedImage' {dryRun} -> dryRun) (\s@CreateUpdatedImage' {} a -> s {dryRun = a} :: CreateUpdatedImage)

-- | The description to display for the new image.
createUpdatedImage_newImageDescription :: Lens.Lens' CreateUpdatedImage (Prelude.Maybe Prelude.Text)
createUpdatedImage_newImageDescription = Lens.lens (\CreateUpdatedImage' {newImageDescription'} -> newImageDescription') (\s@CreateUpdatedImage' {} a -> s {newImageDescription' = a} :: CreateUpdatedImage)

-- | The name to display for the new image.
createUpdatedImage_newImageDisplayName :: Lens.Lens' CreateUpdatedImage (Prelude.Maybe Prelude.Text)
createUpdatedImage_newImageDisplayName = Lens.lens (\CreateUpdatedImage' {newImageDisplayName'} -> newImageDisplayName') (\s@CreateUpdatedImage' {} a -> s {newImageDisplayName' = a} :: CreateUpdatedImage)

-- | The tags to associate with the new image. A tag is a key-value pair, and
-- the value is optional. For example, Environment=Test. If you do not
-- specify a value, Environment=.
--
-- Generally allowed characters are: letters, numbers, and spaces
-- representable in UTF-8, and the following special characters:
--
-- _ . : \/ = + \\ - \@
--
-- If you do not specify a value, the value is set to an empty string.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html Tagging Your Resources>
-- in the /Amazon AppStream 2.0 Administration Guide/.
createUpdatedImage_newImageTags :: Lens.Lens' CreateUpdatedImage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUpdatedImage_newImageTags = Lens.lens (\CreateUpdatedImage' {newImageTags'} -> newImageTags') (\s@CreateUpdatedImage' {} a -> s {newImageTags' = a} :: CreateUpdatedImage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image to update.
createUpdatedImage_existingImageName :: Lens.Lens' CreateUpdatedImage Prelude.Text
createUpdatedImage_existingImageName = Lens.lens (\CreateUpdatedImage' {existingImageName} -> existingImageName) (\s@CreateUpdatedImage' {} a -> s {existingImageName = a} :: CreateUpdatedImage)

-- | The name of the new image. The name must be unique within the AWS
-- account and Region.
createUpdatedImage_newImageName :: Lens.Lens' CreateUpdatedImage Prelude.Text
createUpdatedImage_newImageName = Lens.lens (\CreateUpdatedImage' {newImageName'} -> newImageName') (\s@CreateUpdatedImage' {} a -> s {newImageName' = a} :: CreateUpdatedImage)

instance Core.AWSRequest CreateUpdatedImage where
  type
    AWSResponse CreateUpdatedImage =
      CreateUpdatedImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUpdatedImageResponse'
            Prelude.<$> (x Data..?> "canUpdateImage")
            Prelude.<*> (x Data..?> "image")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUpdatedImage where
  hashWithSalt _salt CreateUpdatedImage' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` newImageDescription'
      `Prelude.hashWithSalt` newImageDisplayName'
      `Prelude.hashWithSalt` newImageTags'
      `Prelude.hashWithSalt` existingImageName
      `Prelude.hashWithSalt` newImageName'

instance Prelude.NFData CreateUpdatedImage where
  rnf CreateUpdatedImage' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf newImageDescription'
      `Prelude.seq` Prelude.rnf newImageDisplayName'
      `Prelude.seq` Prelude.rnf newImageTags'
      `Prelude.seq` Prelude.rnf existingImageName
      `Prelude.seq` Prelude.rnf newImageName'

instance Data.ToHeaders CreateUpdatedImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.CreateUpdatedImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUpdatedImage where
  toJSON CreateUpdatedImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dryRun" Data..=) Prelude.<$> dryRun,
            ("newImageDescription" Data..=)
              Prelude.<$> newImageDescription',
            ("newImageDisplayName" Data..=)
              Prelude.<$> newImageDisplayName',
            ("newImageTags" Data..=) Prelude.<$> newImageTags',
            Prelude.Just
              ("existingImageName" Data..= existingImageName),
            Prelude.Just ("newImageName" Data..= newImageName')
          ]
      )

instance Data.ToPath CreateUpdatedImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUpdatedImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUpdatedImageResponse' smart constructor.
data CreateUpdatedImageResponse = CreateUpdatedImageResponse'
  { -- | Indicates whether a new image can be created.
    canUpdateImage :: Prelude.Maybe Prelude.Bool,
    image :: Prelude.Maybe Image,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUpdatedImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canUpdateImage', 'createUpdatedImageResponse_canUpdateImage' - Indicates whether a new image can be created.
--
-- 'image', 'createUpdatedImageResponse_image' - Undocumented member.
--
-- 'httpStatus', 'createUpdatedImageResponse_httpStatus' - The response's http status code.
newCreateUpdatedImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUpdatedImageResponse
newCreateUpdatedImageResponse pHttpStatus_ =
  CreateUpdatedImageResponse'
    { canUpdateImage =
        Prelude.Nothing,
      image = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether a new image can be created.
createUpdatedImageResponse_canUpdateImage :: Lens.Lens' CreateUpdatedImageResponse (Prelude.Maybe Prelude.Bool)
createUpdatedImageResponse_canUpdateImage = Lens.lens (\CreateUpdatedImageResponse' {canUpdateImage} -> canUpdateImage) (\s@CreateUpdatedImageResponse' {} a -> s {canUpdateImage = a} :: CreateUpdatedImageResponse)

-- | Undocumented member.
createUpdatedImageResponse_image :: Lens.Lens' CreateUpdatedImageResponse (Prelude.Maybe Image)
createUpdatedImageResponse_image = Lens.lens (\CreateUpdatedImageResponse' {image} -> image) (\s@CreateUpdatedImageResponse' {} a -> s {image = a} :: CreateUpdatedImageResponse)

-- | The response's http status code.
createUpdatedImageResponse_httpStatus :: Lens.Lens' CreateUpdatedImageResponse Prelude.Int
createUpdatedImageResponse_httpStatus = Lens.lens (\CreateUpdatedImageResponse' {httpStatus} -> httpStatus) (\s@CreateUpdatedImageResponse' {} a -> s {httpStatus = a} :: CreateUpdatedImageResponse)

instance Prelude.NFData CreateUpdatedImageResponse where
  rnf CreateUpdatedImageResponse' {..} =
    Prelude.rnf canUpdateImage
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf httpStatus
