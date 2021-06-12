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
-- Module      : Network.AWS.EC2.Types.DiskImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImage where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageDetail
import Network.AWS.EC2.Types.VolumeDetail
import qualified Network.AWS.Lens as Lens

-- | Describes a disk image.
--
-- /See:/ 'newDiskImage' smart constructor.
data DiskImage = DiskImage'
  { -- | Information about the volume.
    volume :: Core.Maybe VolumeDetail,
    -- | Information about the disk image.
    image :: Core.Maybe DiskImageDetail,
    -- | A description of the disk image.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiskImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volume', 'diskImage_volume' - Information about the volume.
--
-- 'image', 'diskImage_image' - Information about the disk image.
--
-- 'description', 'diskImage_description' - A description of the disk image.
newDiskImage ::
  DiskImage
newDiskImage =
  DiskImage'
    { volume = Core.Nothing,
      image = Core.Nothing,
      description = Core.Nothing
    }

-- | Information about the volume.
diskImage_volume :: Lens.Lens' DiskImage (Core.Maybe VolumeDetail)
diskImage_volume = Lens.lens (\DiskImage' {volume} -> volume) (\s@DiskImage' {} a -> s {volume = a} :: DiskImage)

-- | Information about the disk image.
diskImage_image :: Lens.Lens' DiskImage (Core.Maybe DiskImageDetail)
diskImage_image = Lens.lens (\DiskImage' {image} -> image) (\s@DiskImage' {} a -> s {image = a} :: DiskImage)

-- | A description of the disk image.
diskImage_description :: Lens.Lens' DiskImage (Core.Maybe Core.Text)
diskImage_description = Lens.lens (\DiskImage' {description} -> description) (\s@DiskImage' {} a -> s {description = a} :: DiskImage)

instance Core.Hashable DiskImage

instance Core.NFData DiskImage

instance Core.ToQuery DiskImage where
  toQuery DiskImage' {..} =
    Core.mconcat
      [ "Volume" Core.=: volume,
        "Image" Core.=: image,
        "Description" Core.=: description
      ]
