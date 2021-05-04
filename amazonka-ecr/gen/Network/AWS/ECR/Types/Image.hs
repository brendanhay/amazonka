{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Image where

import Network.AWS.ECR.Types.ImageIdentifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Amazon ECR image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The image manifest associated with the image.
    imageManifest :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID associated with the registry containing the image.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository associated with the image.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | An object containing the image tag and image digest associated with an
    -- image.
    imageId :: Prelude.Maybe ImageIdentifier,
    -- | The manifest media type of the image.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageManifest', 'image_imageManifest' - The image manifest associated with the image.
--
-- 'registryId', 'image_registryId' - The AWS account ID associated with the registry containing the image.
--
-- 'repositoryName', 'image_repositoryName' - The name of the repository associated with the image.
--
-- 'imageId', 'image_imageId' - An object containing the image tag and image digest associated with an
-- image.
--
-- 'imageManifestMediaType', 'image_imageManifestMediaType' - The manifest media type of the image.
newImage ::
  Image
newImage =
  Image'
    { imageManifest = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      imageId = Prelude.Nothing,
      imageManifestMediaType = Prelude.Nothing
    }

-- | The image manifest associated with the image.
image_imageManifest :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageManifest = Lens.lens (\Image' {imageManifest} -> imageManifest) (\s@Image' {} a -> s {imageManifest = a} :: Image)

-- | The AWS account ID associated with the registry containing the image.
image_registryId :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_registryId = Lens.lens (\Image' {registryId} -> registryId) (\s@Image' {} a -> s {registryId = a} :: Image)

-- | The name of the repository associated with the image.
image_repositoryName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_repositoryName = Lens.lens (\Image' {repositoryName} -> repositoryName) (\s@Image' {} a -> s {repositoryName = a} :: Image)

-- | An object containing the image tag and image digest associated with an
-- image.
image_imageId :: Lens.Lens' Image (Prelude.Maybe ImageIdentifier)
image_imageId = Lens.lens (\Image' {imageId} -> imageId) (\s@Image' {} a -> s {imageId = a} :: Image)

-- | The manifest media type of the image.
image_imageManifestMediaType :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageManifestMediaType = Lens.lens (\Image' {imageManifestMediaType} -> imageManifestMediaType) (\s@Image' {} a -> s {imageManifestMediaType = a} :: Image)

instance Prelude.FromJSON Image where
  parseJSON =
    Prelude.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Prelude..:? "imageManifest")
            Prelude.<*> (x Prelude..:? "registryId")
            Prelude.<*> (x Prelude..:? "repositoryName")
            Prelude.<*> (x Prelude..:? "imageId")
            Prelude.<*> (x Prelude..:? "imageManifestMediaType")
      )

instance Prelude.Hashable Image

instance Prelude.NFData Image
