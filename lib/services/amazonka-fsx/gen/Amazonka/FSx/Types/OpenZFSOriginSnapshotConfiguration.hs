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
-- Module      : Amazonka.FSx.Types.OpenZFSOriginSnapshotConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSOriginSnapshotConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSCopyStrategy
import qualified Amazonka.Prelude as Prelude

-- | The snapshot configuration to use when creating an OpenZFS volume from a
-- snapshot.
--
-- /See:/ 'newOpenZFSOriginSnapshotConfiguration' smart constructor.
data OpenZFSOriginSnapshotConfiguration = OpenZFSOriginSnapshotConfiguration'
  { -- | The strategy used when copying data from the snapshot to the new volume.
    --
    -- -   @CLONE@ - The new volume references the data in the origin snapshot.
    --     Cloning a snapshot is faster than copying the data from a snapshot
    --     to a new volume and doesn\'t consume disk throughput. However, the
    --     origin snapshot can\'t be deleted if there is a volume using its
    --     copied data.
    --
    -- -   @FULL_COPY@ - Copies all data from the snapshot to the new volume.
    copyStrategy :: Prelude.Maybe OpenZFSCopyStrategy,
    snapshotARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenZFSOriginSnapshotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyStrategy', 'openZFSOriginSnapshotConfiguration_copyStrategy' - The strategy used when copying data from the snapshot to the new volume.
--
-- -   @CLONE@ - The new volume references the data in the origin snapshot.
--     Cloning a snapshot is faster than copying the data from a snapshot
--     to a new volume and doesn\'t consume disk throughput. However, the
--     origin snapshot can\'t be deleted if there is a volume using its
--     copied data.
--
-- -   @FULL_COPY@ - Copies all data from the snapshot to the new volume.
--
-- 'snapshotARN', 'openZFSOriginSnapshotConfiguration_snapshotARN' - Undocumented member.
newOpenZFSOriginSnapshotConfiguration ::
  OpenZFSOriginSnapshotConfiguration
newOpenZFSOriginSnapshotConfiguration =
  OpenZFSOriginSnapshotConfiguration'
    { copyStrategy =
        Prelude.Nothing,
      snapshotARN = Prelude.Nothing
    }

-- | The strategy used when copying data from the snapshot to the new volume.
--
-- -   @CLONE@ - The new volume references the data in the origin snapshot.
--     Cloning a snapshot is faster than copying the data from a snapshot
--     to a new volume and doesn\'t consume disk throughput. However, the
--     origin snapshot can\'t be deleted if there is a volume using its
--     copied data.
--
-- -   @FULL_COPY@ - Copies all data from the snapshot to the new volume.
openZFSOriginSnapshotConfiguration_copyStrategy :: Lens.Lens' OpenZFSOriginSnapshotConfiguration (Prelude.Maybe OpenZFSCopyStrategy)
openZFSOriginSnapshotConfiguration_copyStrategy = Lens.lens (\OpenZFSOriginSnapshotConfiguration' {copyStrategy} -> copyStrategy) (\s@OpenZFSOriginSnapshotConfiguration' {} a -> s {copyStrategy = a} :: OpenZFSOriginSnapshotConfiguration)

-- | Undocumented member.
openZFSOriginSnapshotConfiguration_snapshotARN :: Lens.Lens' OpenZFSOriginSnapshotConfiguration (Prelude.Maybe Prelude.Text)
openZFSOriginSnapshotConfiguration_snapshotARN = Lens.lens (\OpenZFSOriginSnapshotConfiguration' {snapshotARN} -> snapshotARN) (\s@OpenZFSOriginSnapshotConfiguration' {} a -> s {snapshotARN = a} :: OpenZFSOriginSnapshotConfiguration)

instance
  Data.FromJSON
    OpenZFSOriginSnapshotConfiguration
  where
  parseJSON =
    Data.withObject
      "OpenZFSOriginSnapshotConfiguration"
      ( \x ->
          OpenZFSOriginSnapshotConfiguration'
            Prelude.<$> (x Data..:? "CopyStrategy")
            Prelude.<*> (x Data..:? "SnapshotARN")
      )

instance
  Prelude.Hashable
    OpenZFSOriginSnapshotConfiguration
  where
  hashWithSalt
    _salt
    OpenZFSOriginSnapshotConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` copyStrategy
        `Prelude.hashWithSalt` snapshotARN

instance
  Prelude.NFData
    OpenZFSOriginSnapshotConfiguration
  where
  rnf OpenZFSOriginSnapshotConfiguration' {..} =
    Prelude.rnf copyStrategy
      `Prelude.seq` Prelude.rnf snapshotARN
