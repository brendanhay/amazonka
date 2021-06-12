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

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types.ImageIdentifier
import qualified Network.AWS.Lens as Lens

-- | An object representing an Amazon ECR image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The image manifest associated with the image.
    imageManifest :: Core.Maybe Core.Text,
    -- | The AWS account ID associated with the registry containing the image.
    registryId :: Core.Maybe Core.Text,
    -- | The name of the repository associated with the image.
    repositoryName :: Core.Maybe Core.Text,
    -- | An object containing the image tag and image digest associated with an
    -- image.
    imageId :: Core.Maybe ImageIdentifier,
    -- | The manifest media type of the image.
    imageManifestMediaType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { imageManifest = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      imageId = Core.Nothing,
      imageManifestMediaType = Core.Nothing
    }

-- | The image manifest associated with the image.
image_imageManifest :: Lens.Lens' Image (Core.Maybe Core.Text)
image_imageManifest = Lens.lens (\Image' {imageManifest} -> imageManifest) (\s@Image' {} a -> s {imageManifest = a} :: Image)

-- | The AWS account ID associated with the registry containing the image.
image_registryId :: Lens.Lens' Image (Core.Maybe Core.Text)
image_registryId = Lens.lens (\Image' {registryId} -> registryId) (\s@Image' {} a -> s {registryId = a} :: Image)

-- | The name of the repository associated with the image.
image_repositoryName :: Lens.Lens' Image (Core.Maybe Core.Text)
image_repositoryName = Lens.lens (\Image' {repositoryName} -> repositoryName) (\s@Image' {} a -> s {repositoryName = a} :: Image)

-- | An object containing the image tag and image digest associated with an
-- image.
image_imageId :: Lens.Lens' Image (Core.Maybe ImageIdentifier)
image_imageId = Lens.lens (\Image' {imageId} -> imageId) (\s@Image' {} a -> s {imageId = a} :: Image)

-- | The manifest media type of the image.
image_imageManifestMediaType :: Lens.Lens' Image (Core.Maybe Core.Text)
image_imageManifestMediaType = Lens.lens (\Image' {imageManifestMediaType} -> imageManifestMediaType) (\s@Image' {} a -> s {imageManifestMediaType = a} :: Image)

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Core.<$> (x Core..:? "imageManifest")
            Core.<*> (x Core..:? "registryId")
            Core.<*> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "imageId")
            Core.<*> (x Core..:? "imageManifestMediaType")
      )

instance Core.Hashable Image

instance Core.NFData Image
