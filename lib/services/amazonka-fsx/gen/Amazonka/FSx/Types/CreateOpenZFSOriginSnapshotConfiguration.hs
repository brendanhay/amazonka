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
-- Module      : Amazonka.FSx.Types.CreateOpenZFSOriginSnapshotConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateOpenZFSOriginSnapshotConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSCopyStrategy
import qualified Amazonka.Prelude as Prelude

-- | The snapshot configuration to use when creating an OpenZFS volume from a
-- snapshot.
--
-- /See:/ 'newCreateOpenZFSOriginSnapshotConfiguration' smart constructor.
data CreateOpenZFSOriginSnapshotConfiguration = CreateOpenZFSOriginSnapshotConfiguration'
  { snapshotARN :: Prelude.Text,
    -- | The strategy used when copying data from the snapshot to the new volume.
    --
    -- -   @CLONE@ - The new volume references the data in the origin snapshot.
    --     Cloning a snapshot is faster than copying data from the snapshot to
    --     a new volume and doesn\'t consume disk throughput. However, the
    --     origin snapshot can\'t be deleted if there is a volume using its
    --     copied data.
    --
    -- -   @FULL_COPY@ - Copies all data from the snapshot to the new volume.
    copyStrategy :: OpenZFSCopyStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOpenZFSOriginSnapshotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotARN', 'createOpenZFSOriginSnapshotConfiguration_snapshotARN' - Undocumented member.
--
-- 'copyStrategy', 'createOpenZFSOriginSnapshotConfiguration_copyStrategy' - The strategy used when copying data from the snapshot to the new volume.
--
-- -   @CLONE@ - The new volume references the data in the origin snapshot.
--     Cloning a snapshot is faster than copying data from the snapshot to
--     a new volume and doesn\'t consume disk throughput. However, the
--     origin snapshot can\'t be deleted if there is a volume using its
--     copied data.
--
-- -   @FULL_COPY@ - Copies all data from the snapshot to the new volume.
newCreateOpenZFSOriginSnapshotConfiguration ::
  -- | 'snapshotARN'
  Prelude.Text ->
  -- | 'copyStrategy'
  OpenZFSCopyStrategy ->
  CreateOpenZFSOriginSnapshotConfiguration
newCreateOpenZFSOriginSnapshotConfiguration
  pSnapshotARN_
  pCopyStrategy_ =
    CreateOpenZFSOriginSnapshotConfiguration'
      { snapshotARN =
          pSnapshotARN_,
        copyStrategy = pCopyStrategy_
      }

-- | Undocumented member.
createOpenZFSOriginSnapshotConfiguration_snapshotARN :: Lens.Lens' CreateOpenZFSOriginSnapshotConfiguration Prelude.Text
createOpenZFSOriginSnapshotConfiguration_snapshotARN = Lens.lens (\CreateOpenZFSOriginSnapshotConfiguration' {snapshotARN} -> snapshotARN) (\s@CreateOpenZFSOriginSnapshotConfiguration' {} a -> s {snapshotARN = a} :: CreateOpenZFSOriginSnapshotConfiguration)

-- | The strategy used when copying data from the snapshot to the new volume.
--
-- -   @CLONE@ - The new volume references the data in the origin snapshot.
--     Cloning a snapshot is faster than copying data from the snapshot to
--     a new volume and doesn\'t consume disk throughput. However, the
--     origin snapshot can\'t be deleted if there is a volume using its
--     copied data.
--
-- -   @FULL_COPY@ - Copies all data from the snapshot to the new volume.
createOpenZFSOriginSnapshotConfiguration_copyStrategy :: Lens.Lens' CreateOpenZFSOriginSnapshotConfiguration OpenZFSCopyStrategy
createOpenZFSOriginSnapshotConfiguration_copyStrategy = Lens.lens (\CreateOpenZFSOriginSnapshotConfiguration' {copyStrategy} -> copyStrategy) (\s@CreateOpenZFSOriginSnapshotConfiguration' {} a -> s {copyStrategy = a} :: CreateOpenZFSOriginSnapshotConfiguration)

instance
  Prelude.Hashable
    CreateOpenZFSOriginSnapshotConfiguration
  where
  hashWithSalt
    _salt
    CreateOpenZFSOriginSnapshotConfiguration' {..} =
      _salt `Prelude.hashWithSalt` snapshotARN
        `Prelude.hashWithSalt` copyStrategy

instance
  Prelude.NFData
    CreateOpenZFSOriginSnapshotConfiguration
  where
  rnf CreateOpenZFSOriginSnapshotConfiguration' {..} =
    Prelude.rnf snapshotARN
      `Prelude.seq` Prelude.rnf copyStrategy

instance
  Data.ToJSON
    CreateOpenZFSOriginSnapshotConfiguration
  where
  toJSON CreateOpenZFSOriginSnapshotConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SnapshotARN" Data..= snapshotARN),
            Prelude.Just ("CopyStrategy" Data..= copyStrategy)
          ]
      )
