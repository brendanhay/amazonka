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
-- Module      : Amazonka.ECRPublic.Types.Layer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.Layer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECRPublic.Types.LayerAvailability
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon ECR image layer.
--
-- /See:/ 'newLayer' smart constructor.
data Layer = Layer'
  { -- | The size, in bytes, of the image layer.
    layerSize :: Prelude.Maybe Prelude.Integer,
    -- | The availability status of the image layer.
    layerAvailability :: Prelude.Maybe LayerAvailability,
    -- | The media type of the layer, such as
    -- @application\/vnd.docker.image.rootfs.diff.tar.gzip@ or
    -- @application\/vnd.oci.image.layer.v1.tar+gzip@.
    mediaType :: Prelude.Maybe Prelude.Text,
    -- | The @sha256@ digest of the image layer.
    layerDigest :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Layer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerSize', 'layer_layerSize' - The size, in bytes, of the image layer.
--
-- 'layerAvailability', 'layer_layerAvailability' - The availability status of the image layer.
--
-- 'mediaType', 'layer_mediaType' - The media type of the layer, such as
-- @application\/vnd.docker.image.rootfs.diff.tar.gzip@ or
-- @application\/vnd.oci.image.layer.v1.tar+gzip@.
--
-- 'layerDigest', 'layer_layerDigest' - The @sha256@ digest of the image layer.
newLayer ::
  Layer
newLayer =
  Layer'
    { layerSize = Prelude.Nothing,
      layerAvailability = Prelude.Nothing,
      mediaType = Prelude.Nothing,
      layerDigest = Prelude.Nothing
    }

-- | The size, in bytes, of the image layer.
layer_layerSize :: Lens.Lens' Layer (Prelude.Maybe Prelude.Integer)
layer_layerSize = Lens.lens (\Layer' {layerSize} -> layerSize) (\s@Layer' {} a -> s {layerSize = a} :: Layer)

-- | The availability status of the image layer.
layer_layerAvailability :: Lens.Lens' Layer (Prelude.Maybe LayerAvailability)
layer_layerAvailability = Lens.lens (\Layer' {layerAvailability} -> layerAvailability) (\s@Layer' {} a -> s {layerAvailability = a} :: Layer)

-- | The media type of the layer, such as
-- @application\/vnd.docker.image.rootfs.diff.tar.gzip@ or
-- @application\/vnd.oci.image.layer.v1.tar+gzip@.
layer_mediaType :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_mediaType = Lens.lens (\Layer' {mediaType} -> mediaType) (\s@Layer' {} a -> s {mediaType = a} :: Layer)

-- | The @sha256@ digest of the image layer.
layer_layerDigest :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_layerDigest = Lens.lens (\Layer' {layerDigest} -> layerDigest) (\s@Layer' {} a -> s {layerDigest = a} :: Layer)

instance Core.FromJSON Layer where
  parseJSON =
    Core.withObject
      "Layer"
      ( \x ->
          Layer'
            Prelude.<$> (x Core..:? "layerSize")
            Prelude.<*> (x Core..:? "layerAvailability")
            Prelude.<*> (x Core..:? "mediaType")
            Prelude.<*> (x Core..:? "layerDigest")
      )

instance Prelude.Hashable Layer where
  hashWithSalt _salt Layer' {..} =
    _salt `Prelude.hashWithSalt` layerSize
      `Prelude.hashWithSalt` layerAvailability
      `Prelude.hashWithSalt` mediaType
      `Prelude.hashWithSalt` layerDigest

instance Prelude.NFData Layer where
  rnf Layer' {..} =
    Prelude.rnf layerSize
      `Prelude.seq` Prelude.rnf layerAvailability
      `Prelude.seq` Prelude.rnf mediaType
      `Prelude.seq` Prelude.rnf layerDigest
