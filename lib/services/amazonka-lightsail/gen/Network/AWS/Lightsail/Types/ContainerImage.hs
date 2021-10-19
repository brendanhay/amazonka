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
-- Module      : Network.AWS.Lightsail.Types.ContainerImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerImage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a container image that is registered to an Amazon Lightsail
-- container service.
--
-- /See:/ 'newContainerImage' smart constructor.
data ContainerImage = ContainerImage'
  { -- | The name of the container image.
    image :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the container image was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The digest of the container image.
    digest :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'image', 'containerImage_image' - The name of the container image.
--
-- 'createdAt', 'containerImage_createdAt' - The timestamp when the container image was created.
--
-- 'digest', 'containerImage_digest' - The digest of the container image.
newContainerImage ::
  ContainerImage
newContainerImage =
  ContainerImage'
    { image = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      digest = Prelude.Nothing
    }

-- | The name of the container image.
containerImage_image :: Lens.Lens' ContainerImage (Prelude.Maybe Prelude.Text)
containerImage_image = Lens.lens (\ContainerImage' {image} -> image) (\s@ContainerImage' {} a -> s {image = a} :: ContainerImage)

-- | The timestamp when the container image was created.
containerImage_createdAt :: Lens.Lens' ContainerImage (Prelude.Maybe Prelude.UTCTime)
containerImage_createdAt = Lens.lens (\ContainerImage' {createdAt} -> createdAt) (\s@ContainerImage' {} a -> s {createdAt = a} :: ContainerImage) Prelude.. Lens.mapping Core._Time

-- | The digest of the container image.
containerImage_digest :: Lens.Lens' ContainerImage (Prelude.Maybe Prelude.Text)
containerImage_digest = Lens.lens (\ContainerImage' {digest} -> digest) (\s@ContainerImage' {} a -> s {digest = a} :: ContainerImage)

instance Core.FromJSON ContainerImage where
  parseJSON =
    Core.withObject
      "ContainerImage"
      ( \x ->
          ContainerImage'
            Prelude.<$> (x Core..:? "image")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "digest")
      )

instance Prelude.Hashable ContainerImage

instance Prelude.NFData ContainerImage
