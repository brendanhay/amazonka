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
-- Module      : Amazonka.EC2.Types.DiskImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DiskImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DiskImageDetail
import Amazonka.EC2.Types.VolumeDetail
import qualified Amazonka.Prelude as Prelude

-- | Describes a disk image.
--
-- /See:/ 'newDiskImage' smart constructor.
data DiskImage = DiskImage'
  { -- | A description of the disk image.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the volume.
    volume :: Prelude.Maybe VolumeDetail,
    -- | Information about the disk image.
    image :: Prelude.Maybe DiskImageDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'diskImage_description' - A description of the disk image.
--
-- 'volume', 'diskImage_volume' - Information about the volume.
--
-- 'image', 'diskImage_image' - Information about the disk image.
newDiskImage ::
  DiskImage
newDiskImage =
  DiskImage'
    { description = Prelude.Nothing,
      volume = Prelude.Nothing,
      image = Prelude.Nothing
    }

-- | A description of the disk image.
diskImage_description :: Lens.Lens' DiskImage (Prelude.Maybe Prelude.Text)
diskImage_description = Lens.lens (\DiskImage' {description} -> description) (\s@DiskImage' {} a -> s {description = a} :: DiskImage)

-- | Information about the volume.
diskImage_volume :: Lens.Lens' DiskImage (Prelude.Maybe VolumeDetail)
diskImage_volume = Lens.lens (\DiskImage' {volume} -> volume) (\s@DiskImage' {} a -> s {volume = a} :: DiskImage)

-- | Information about the disk image.
diskImage_image :: Lens.Lens' DiskImage (Prelude.Maybe DiskImageDetail)
diskImage_image = Lens.lens (\DiskImage' {image} -> image) (\s@DiskImage' {} a -> s {image = a} :: DiskImage)

instance Prelude.Hashable DiskImage where
  hashWithSalt _salt DiskImage' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` volume
      `Prelude.hashWithSalt` image

instance Prelude.NFData DiskImage where
  rnf DiskImage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf volume
      `Prelude.seq` Prelude.rnf image

instance Core.ToQuery DiskImage where
  toQuery DiskImage' {..} =
    Prelude.mconcat
      [ "Description" Core.=: description,
        "Volume" Core.=: volume,
        "Image" Core.=: image
      ]
