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
-- Module      : Amazonka.EC2.Types.DiskImageVolumeDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DiskImageVolumeDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a disk image volume.
--
-- /See:/ 'newDiskImageVolumeDescription' smart constructor.
data DiskImageVolumeDescription = DiskImageVolumeDescription'
  { -- | The volume identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The size of the volume, in GiB.
    size :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { id = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The volume identifier.
diskImageVolumeDescription_id :: Lens.Lens' DiskImageVolumeDescription (Prelude.Maybe Prelude.Text)
diskImageVolumeDescription_id = Lens.lens (\DiskImageVolumeDescription' {id} -> id) (\s@DiskImageVolumeDescription' {} a -> s {id = a} :: DiskImageVolumeDescription)

-- | The size of the volume, in GiB.
diskImageVolumeDescription_size :: Lens.Lens' DiskImageVolumeDescription (Prelude.Maybe Prelude.Integer)
diskImageVolumeDescription_size = Lens.lens (\DiskImageVolumeDescription' {size} -> size) (\s@DiskImageVolumeDescription' {} a -> s {size = a} :: DiskImageVolumeDescription)

instance Data.FromXML DiskImageVolumeDescription where
  parseXML x =
    DiskImageVolumeDescription'
      Prelude.<$> (x Data..@? "id")
      Prelude.<*> (x Data..@? "size")

instance Prelude.Hashable DiskImageVolumeDescription where
  hashWithSalt _salt DiskImageVolumeDescription' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` size

instance Prelude.NFData DiskImageVolumeDescription where
  rnf DiskImageVolumeDescription' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf size
