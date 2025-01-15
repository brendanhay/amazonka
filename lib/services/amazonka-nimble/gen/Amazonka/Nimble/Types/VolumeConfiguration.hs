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
-- Module      : Amazonka.Nimble.Types.VolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.VolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Custom volume configuration for the root volumes that are attached to
-- streaming sessions.
--
-- This parameter is only allowed when @sessionPersistenceMode@ is
-- @ACTIVATED@.
--
-- /See:/ 'newVolumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { -- | The number of I\/O operations per second for the root volume that is
    -- attached to streaming session.
    iops :: Prelude.Maybe Prelude.Natural,
    -- | The size of the root volume that is attached to the streaming session.
    -- The root volume size is measured in GiBs.
    size :: Prelude.Maybe Prelude.Natural,
    -- | The throughput to provision for the root volume that is attached to the
    -- streaming session. The throughput is measured in MiB\/s.
    throughput :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iops', 'volumeConfiguration_iops' - The number of I\/O operations per second for the root volume that is
-- attached to streaming session.
--
-- 'size', 'volumeConfiguration_size' - The size of the root volume that is attached to the streaming session.
-- The root volume size is measured in GiBs.
--
-- 'throughput', 'volumeConfiguration_throughput' - The throughput to provision for the root volume that is attached to the
-- streaming session. The throughput is measured in MiB\/s.
newVolumeConfiguration ::
  VolumeConfiguration
newVolumeConfiguration =
  VolumeConfiguration'
    { iops = Prelude.Nothing,
      size = Prelude.Nothing,
      throughput = Prelude.Nothing
    }

-- | The number of I\/O operations per second for the root volume that is
-- attached to streaming session.
volumeConfiguration_iops :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Natural)
volumeConfiguration_iops = Lens.lens (\VolumeConfiguration' {iops} -> iops) (\s@VolumeConfiguration' {} a -> s {iops = a} :: VolumeConfiguration)

-- | The size of the root volume that is attached to the streaming session.
-- The root volume size is measured in GiBs.
volumeConfiguration_size :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Natural)
volumeConfiguration_size = Lens.lens (\VolumeConfiguration' {size} -> size) (\s@VolumeConfiguration' {} a -> s {size = a} :: VolumeConfiguration)

-- | The throughput to provision for the root volume that is attached to the
-- streaming session. The throughput is measured in MiB\/s.
volumeConfiguration_throughput :: Lens.Lens' VolumeConfiguration (Prelude.Maybe Prelude.Natural)
volumeConfiguration_throughput = Lens.lens (\VolumeConfiguration' {throughput} -> throughput) (\s@VolumeConfiguration' {} a -> s {throughput = a} :: VolumeConfiguration)

instance Data.FromJSON VolumeConfiguration where
  parseJSON =
    Data.withObject
      "VolumeConfiguration"
      ( \x ->
          VolumeConfiguration'
            Prelude.<$> (x Data..:? "iops")
            Prelude.<*> (x Data..:? "size")
            Prelude.<*> (x Data..:? "throughput")
      )

instance Prelude.Hashable VolumeConfiguration where
  hashWithSalt _salt VolumeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` throughput

instance Prelude.NFData VolumeConfiguration where
  rnf VolumeConfiguration' {..} =
    Prelude.rnf iops `Prelude.seq`
      Prelude.rnf size `Prelude.seq`
        Prelude.rnf throughput

instance Data.ToJSON VolumeConfiguration where
  toJSON VolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iops" Data..=) Prelude.<$> iops,
            ("size" Data..=) Prelude.<$> size,
            ("throughput" Data..=) Prelude.<$> throughput
          ]
      )
