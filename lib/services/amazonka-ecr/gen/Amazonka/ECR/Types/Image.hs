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
-- Module      : Amazonka.ECR.Types.Image
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.Image where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types.ImageIdentifier
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon ECR image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | The name of the repository associated with the image.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID associated with the registry
    -- containing the image.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The manifest media type of the image.
    imageManifestMediaType :: Prelude.Maybe Prelude.Text,
    -- | The image manifest associated with the image.
    imageManifest :: Prelude.Maybe Prelude.Text,
    -- | An object containing the image tag and image digest associated with an
    -- image.
    imageId :: Prelude.Maybe ImageIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'image_repositoryName' - The name of the repository associated with the image.
--
-- 'registryId', 'image_registryId' - The Amazon Web Services account ID associated with the registry
-- containing the image.
--
-- 'imageManifestMediaType', 'image_imageManifestMediaType' - The manifest media type of the image.
--
-- 'imageManifest', 'image_imageManifest' - The image manifest associated with the image.
--
-- 'imageId', 'image_imageId' - An object containing the image tag and image digest associated with an
-- image.
newImage ::
  Image
newImage =
  Image'
    { repositoryName = Prelude.Nothing,
      registryId = Prelude.Nothing,
      imageManifestMediaType = Prelude.Nothing,
      imageManifest = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | The name of the repository associated with the image.
image_repositoryName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_repositoryName = Lens.lens (\Image' {repositoryName} -> repositoryName) (\s@Image' {} a -> s {repositoryName = a} :: Image)

-- | The Amazon Web Services account ID associated with the registry
-- containing the image.
image_registryId :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_registryId = Lens.lens (\Image' {registryId} -> registryId) (\s@Image' {} a -> s {registryId = a} :: Image)

-- | The manifest media type of the image.
image_imageManifestMediaType :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageManifestMediaType = Lens.lens (\Image' {imageManifestMediaType} -> imageManifestMediaType) (\s@Image' {} a -> s {imageManifestMediaType = a} :: Image)

-- | The image manifest associated with the image.
image_imageManifest :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageManifest = Lens.lens (\Image' {imageManifest} -> imageManifest) (\s@Image' {} a -> s {imageManifest = a} :: Image)

-- | An object containing the image tag and image digest associated with an
-- image.
image_imageId :: Lens.Lens' Image (Prelude.Maybe ImageIdentifier)
image_imageId = Lens.lens (\Image' {imageId} -> imageId) (\s@Image' {} a -> s {imageId = a} :: Image)

instance Core.FromJSON Image where
  parseJSON =
    Core.withObject
      "Image"
      ( \x ->
          Image'
            Prelude.<$> (x Core..:? "repositoryName")
            Prelude.<*> (x Core..:? "registryId")
            Prelude.<*> (x Core..:? "imageManifestMediaType")
            Prelude.<*> (x Core..:? "imageManifest")
            Prelude.<*> (x Core..:? "imageId")
      )

instance Prelude.Hashable Image where
  hashWithSalt _salt Image' {..} =
    _salt `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` imageManifestMediaType
      `Prelude.hashWithSalt` imageManifest
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf imageManifestMediaType
      `Prelude.seq` Prelude.rnf imageManifest
      `Prelude.seq` Prelude.rnf imageId
