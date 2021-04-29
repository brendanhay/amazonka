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
-- Module      : Network.AWS.EC2.Types.DiskImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImage where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DiskImageDetail
import Network.AWS.EC2.Types.VolumeDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a disk image.
--
-- /See:/ 'newDiskImage' smart constructor.
data DiskImage = DiskImage'
  { -- | Information about the volume.
    volume :: Prelude.Maybe VolumeDetail,
    -- | Information about the disk image.
    image :: Prelude.Maybe DiskImageDetail,
    -- | A description of the disk image.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { volume = Prelude.Nothing,
      image = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Information about the volume.
diskImage_volume :: Lens.Lens' DiskImage (Prelude.Maybe VolumeDetail)
diskImage_volume = Lens.lens (\DiskImage' {volume} -> volume) (\s@DiskImage' {} a -> s {volume = a} :: DiskImage)

-- | Information about the disk image.
diskImage_image :: Lens.Lens' DiskImage (Prelude.Maybe DiskImageDetail)
diskImage_image = Lens.lens (\DiskImage' {image} -> image) (\s@DiskImage' {} a -> s {image = a} :: DiskImage)

-- | A description of the disk image.
diskImage_description :: Lens.Lens' DiskImage (Prelude.Maybe Prelude.Text)
diskImage_description = Lens.lens (\DiskImage' {description} -> description) (\s@DiskImage' {} a -> s {description = a} :: DiskImage)

instance Prelude.Hashable DiskImage

instance Prelude.NFData DiskImage

instance Prelude.ToQuery DiskImage where
  toQuery DiskImage' {..} =
    Prelude.mconcat
      [ "Volume" Prelude.=: volume,
        "Image" Prelude.=: image,
        "Description" Prelude.=: description
      ]
