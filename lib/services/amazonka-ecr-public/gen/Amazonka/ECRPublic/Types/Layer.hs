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
import qualified Amazonka.Data as Data
import Amazonka.ECRPublic.Types.LayerAvailability
import qualified Amazonka.Prelude as Prelude

-- | An object representing an Amazon ECR image layer.
--
-- /See:/ 'newLayer' smart constructor.
data Layer = Layer'
  { -- | The availability status of the image layer.
    layerAvailability :: Prelude.Maybe LayerAvailability,
    -- | The @sha256@ digest of the image layer.
    layerDigest :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the image layer.
    layerSize :: Prelude.Maybe Prelude.Integer,
    -- | The media type of the layer, such as
    -- @application\/vnd.docker.image.rootfs.diff.tar.gzip@ or
    -- @application\/vnd.oci.image.layer.v1.tar+gzip@.
    mediaType :: Prelude.Maybe Prelude.Text
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
-- 'layerAvailability', 'layer_layerAvailability' - The availability status of the image layer.
--
-- 'layerDigest', 'layer_layerDigest' - The @sha256@ digest of the image layer.
--
-- 'layerSize', 'layer_layerSize' - The size, in bytes, of the image layer.
--
-- 'mediaType', 'layer_mediaType' - The media type of the layer, such as
-- @application\/vnd.docker.image.rootfs.diff.tar.gzip@ or
-- @application\/vnd.oci.image.layer.v1.tar+gzip@.
newLayer ::
  Layer
newLayer =
  Layer'
    { layerAvailability = Prelude.Nothing,
      layerDigest = Prelude.Nothing,
      layerSize = Prelude.Nothing,
      mediaType = Prelude.Nothing
    }

-- | The availability status of the image layer.
layer_layerAvailability :: Lens.Lens' Layer (Prelude.Maybe LayerAvailability)
layer_layerAvailability = Lens.lens (\Layer' {layerAvailability} -> layerAvailability) (\s@Layer' {} a -> s {layerAvailability = a} :: Layer)

-- | The @sha256@ digest of the image layer.
layer_layerDigest :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_layerDigest = Lens.lens (\Layer' {layerDigest} -> layerDigest) (\s@Layer' {} a -> s {layerDigest = a} :: Layer)

-- | The size, in bytes, of the image layer.
layer_layerSize :: Lens.Lens' Layer (Prelude.Maybe Prelude.Integer)
layer_layerSize = Lens.lens (\Layer' {layerSize} -> layerSize) (\s@Layer' {} a -> s {layerSize = a} :: Layer)

-- | The media type of the layer, such as
-- @application\/vnd.docker.image.rootfs.diff.tar.gzip@ or
-- @application\/vnd.oci.image.layer.v1.tar+gzip@.
layer_mediaType :: Lens.Lens' Layer (Prelude.Maybe Prelude.Text)
layer_mediaType = Lens.lens (\Layer' {mediaType} -> mediaType) (\s@Layer' {} a -> s {mediaType = a} :: Layer)

instance Data.FromJSON Layer where
  parseJSON =
    Data.withObject
      "Layer"
      ( \x ->
          Layer'
            Prelude.<$> (x Data..:? "layerAvailability")
            Prelude.<*> (x Data..:? "layerDigest")
            Prelude.<*> (x Data..:? "layerSize")
            Prelude.<*> (x Data..:? "mediaType")
      )

instance Prelude.Hashable Layer where
  hashWithSalt _salt Layer' {..} =
    _salt `Prelude.hashWithSalt` layerAvailability
      `Prelude.hashWithSalt` layerDigest
      `Prelude.hashWithSalt` layerSize
      `Prelude.hashWithSalt` mediaType

instance Prelude.NFData Layer where
  rnf Layer' {..} =
    Prelude.rnf layerAvailability
      `Prelude.seq` Prelude.rnf layerDigest
      `Prelude.seq` Prelude.rnf layerSize
      `Prelude.seq` Prelude.rnf mediaType
