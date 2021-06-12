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
-- Module      : Network.AWS.EC2.Types.DiskImageVolumeDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DiskImageVolumeDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a disk image volume.
--
-- /See:/ 'newDiskImageVolumeDescription' smart constructor.
data DiskImageVolumeDescription = DiskImageVolumeDescription'
  { -- | The volume identifier.
    id :: Core.Maybe Core.Text,
    -- | The size of the volume, in GiB.
    size :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiskImageVolumeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'diskImageVolumeDescription_id' - The volume identifier.
--
-- 'size', 'diskImageVolumeDescription_size' - The size of the volume, in GiB.
newDiskImageVolumeDescription ::
  DiskImageVolumeDescription
newDiskImageVolumeDescription =
  DiskImageVolumeDescription'
    { id = Core.Nothing,
      size = Core.Nothing
    }

-- | The volume identifier.
diskImageVolumeDescription_id :: Lens.Lens' DiskImageVolumeDescription (Core.Maybe Core.Text)
diskImageVolumeDescription_id = Lens.lens (\DiskImageVolumeDescription' {id} -> id) (\s@DiskImageVolumeDescription' {} a -> s {id = a} :: DiskImageVolumeDescription)

-- | The size of the volume, in GiB.
diskImageVolumeDescription_size :: Lens.Lens' DiskImageVolumeDescription (Core.Maybe Core.Integer)
diskImageVolumeDescription_size = Lens.lens (\DiskImageVolumeDescription' {size} -> size) (\s@DiskImageVolumeDescription' {} a -> s {size = a} :: DiskImageVolumeDescription)

instance Core.FromXML DiskImageVolumeDescription where
  parseXML x =
    DiskImageVolumeDescription'
      Core.<$> (x Core..@? "id") Core.<*> (x Core..@? "size")

instance Core.Hashable DiskImageVolumeDescription

instance Core.NFData DiskImageVolumeDescription
