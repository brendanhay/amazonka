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
-- Module      : Network.AWS.ECR.Types.Layer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Layer where

import Network.AWS.ECR.Types.LayerAvailability
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Layer where
  parseJSON =
    Prelude.withObject
      "Layer"
      ( \x ->
          Layer'
            Prelude.<$> (x Prelude..:? "layerSize")
            Prelude.<*> (x Prelude..:? "layerAvailability")
            Prelude.<*> (x Prelude..:? "mediaType")
            Prelude.<*> (x Prelude..:? "layerDigest")
      )

instance Prelude.Hashable Layer

instance Prelude.NFData Layer
