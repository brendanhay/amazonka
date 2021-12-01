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
-- Module      : Amazonka.CostExplorer.Types.DiskResourceUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.DiskResourceUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The field that contains a list of disk (local storage) metrics that are
-- associated with the current instance.
--
-- /See:/ 'newDiskResourceUtilization' smart constructor.
data DiskResourceUtilization = DiskResourceUtilization'
  { -- | The maximum number of write operations per second.
    diskWriteOpsPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of read operations per second.
    diskReadOpsPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The maximum write throughput operations per second.
    diskWriteBytesPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The maximum read throughput operations per second.
    diskReadBytesPerSecond :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiskResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskWriteOpsPerSecond', 'diskResourceUtilization_diskWriteOpsPerSecond' - The maximum number of write operations per second.
--
-- 'diskReadOpsPerSecond', 'diskResourceUtilization_diskReadOpsPerSecond' - The maximum number of read operations per second.
--
-- 'diskWriteBytesPerSecond', 'diskResourceUtilization_diskWriteBytesPerSecond' - The maximum write throughput operations per second.
--
-- 'diskReadBytesPerSecond', 'diskResourceUtilization_diskReadBytesPerSecond' - The maximum read throughput operations per second.
newDiskResourceUtilization ::
  DiskResourceUtilization
newDiskResourceUtilization =
  DiskResourceUtilization'
    { diskWriteOpsPerSecond =
        Prelude.Nothing,
      diskReadOpsPerSecond = Prelude.Nothing,
      diskWriteBytesPerSecond = Prelude.Nothing,
      diskReadBytesPerSecond = Prelude.Nothing
    }

-- | The maximum number of write operations per second.
diskResourceUtilization_diskWriteOpsPerSecond :: Lens.Lens' DiskResourceUtilization (Prelude.Maybe Prelude.Text)
diskResourceUtilization_diskWriteOpsPerSecond = Lens.lens (\DiskResourceUtilization' {diskWriteOpsPerSecond} -> diskWriteOpsPerSecond) (\s@DiskResourceUtilization' {} a -> s {diskWriteOpsPerSecond = a} :: DiskResourceUtilization)

-- | The maximum number of read operations per second.
diskResourceUtilization_diskReadOpsPerSecond :: Lens.Lens' DiskResourceUtilization (Prelude.Maybe Prelude.Text)
diskResourceUtilization_diskReadOpsPerSecond = Lens.lens (\DiskResourceUtilization' {diskReadOpsPerSecond} -> diskReadOpsPerSecond) (\s@DiskResourceUtilization' {} a -> s {diskReadOpsPerSecond = a} :: DiskResourceUtilization)

-- | The maximum write throughput operations per second.
diskResourceUtilization_diskWriteBytesPerSecond :: Lens.Lens' DiskResourceUtilization (Prelude.Maybe Prelude.Text)
diskResourceUtilization_diskWriteBytesPerSecond = Lens.lens (\DiskResourceUtilization' {diskWriteBytesPerSecond} -> diskWriteBytesPerSecond) (\s@DiskResourceUtilization' {} a -> s {diskWriteBytesPerSecond = a} :: DiskResourceUtilization)

-- | The maximum read throughput operations per second.
diskResourceUtilization_diskReadBytesPerSecond :: Lens.Lens' DiskResourceUtilization (Prelude.Maybe Prelude.Text)
diskResourceUtilization_diskReadBytesPerSecond = Lens.lens (\DiskResourceUtilization' {diskReadBytesPerSecond} -> diskReadBytesPerSecond) (\s@DiskResourceUtilization' {} a -> s {diskReadBytesPerSecond = a} :: DiskResourceUtilization)

instance Core.FromJSON DiskResourceUtilization where
  parseJSON =
    Core.withObject
      "DiskResourceUtilization"
      ( \x ->
          DiskResourceUtilization'
            Prelude.<$> (x Core..:? "DiskWriteOpsPerSecond")
            Prelude.<*> (x Core..:? "DiskReadOpsPerSecond")
            Prelude.<*> (x Core..:? "DiskWriteBytesPerSecond")
            Prelude.<*> (x Core..:? "DiskReadBytesPerSecond")
      )

instance Prelude.Hashable DiskResourceUtilization where
  hashWithSalt salt' DiskResourceUtilization' {..} =
    salt' `Prelude.hashWithSalt` diskReadBytesPerSecond
      `Prelude.hashWithSalt` diskWriteBytesPerSecond
      `Prelude.hashWithSalt` diskReadOpsPerSecond
      `Prelude.hashWithSalt` diskWriteOpsPerSecond

instance Prelude.NFData DiskResourceUtilization where
  rnf DiskResourceUtilization' {..} =
    Prelude.rnf diskWriteOpsPerSecond
      `Prelude.seq` Prelude.rnf diskReadBytesPerSecond
      `Prelude.seq` Prelude.rnf diskWriteBytesPerSecond
      `Prelude.seq` Prelude.rnf diskReadOpsPerSecond
