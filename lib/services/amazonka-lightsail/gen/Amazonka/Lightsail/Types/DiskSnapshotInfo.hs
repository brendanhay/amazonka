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
-- Module      : Amazonka.Lightsail.Types.DiskSnapshotInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DiskSnapshotInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a disk snapshot.
--
-- /See:/ 'newDiskSnapshotInfo' smart constructor.
data DiskSnapshotInfo = DiskSnapshotInfo'
  { -- | The size of the disk in GB (e.g., @32@).
    sizeInGb :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskSnapshotInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInGb', 'diskSnapshotInfo_sizeInGb' - The size of the disk in GB (e.g., @32@).
newDiskSnapshotInfo ::
  DiskSnapshotInfo
newDiskSnapshotInfo =
  DiskSnapshotInfo' {sizeInGb = Prelude.Nothing}

-- | The size of the disk in GB (e.g., @32@).
diskSnapshotInfo_sizeInGb :: Lens.Lens' DiskSnapshotInfo (Prelude.Maybe Prelude.Int)
diskSnapshotInfo_sizeInGb = Lens.lens (\DiskSnapshotInfo' {sizeInGb} -> sizeInGb) (\s@DiskSnapshotInfo' {} a -> s {sizeInGb = a} :: DiskSnapshotInfo)

instance Data.FromJSON DiskSnapshotInfo where
  parseJSON =
    Data.withObject
      "DiskSnapshotInfo"
      ( \x ->
          DiskSnapshotInfo'
            Prelude.<$> (x Data..:? "sizeInGb")
      )

instance Prelude.Hashable DiskSnapshotInfo where
  hashWithSalt _salt DiskSnapshotInfo' {..} =
    _salt `Prelude.hashWithSalt` sizeInGb

instance Prelude.NFData DiskSnapshotInfo where
  rnf DiskSnapshotInfo' {..} = Prelude.rnf sizeInGb
