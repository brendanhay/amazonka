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
-- Module      : Amazonka.MGN.Types.LaunchTemplateDiskConf
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LaunchTemplateDiskConf where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Launch template disk configuration.
--
-- /See:/ 'newLaunchTemplateDiskConf' smart constructor.
data LaunchTemplateDiskConf = LaunchTemplateDiskConf'
  { -- | Launch template disk iops configuration.
    iops :: Prelude.Maybe Prelude.Natural,
    -- | Launch template disk throughput configuration.
    throughput :: Prelude.Maybe Prelude.Natural,
    -- | Launch template disk volume type configuration.
    volumeType :: Prelude.Maybe VolumeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateDiskConf' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iops', 'launchTemplateDiskConf_iops' - Launch template disk iops configuration.
--
-- 'throughput', 'launchTemplateDiskConf_throughput' - Launch template disk throughput configuration.
--
-- 'volumeType', 'launchTemplateDiskConf_volumeType' - Launch template disk volume type configuration.
newLaunchTemplateDiskConf ::
  LaunchTemplateDiskConf
newLaunchTemplateDiskConf =
  LaunchTemplateDiskConf'
    { iops = Prelude.Nothing,
      throughput = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | Launch template disk iops configuration.
launchTemplateDiskConf_iops :: Lens.Lens' LaunchTemplateDiskConf (Prelude.Maybe Prelude.Natural)
launchTemplateDiskConf_iops = Lens.lens (\LaunchTemplateDiskConf' {iops} -> iops) (\s@LaunchTemplateDiskConf' {} a -> s {iops = a} :: LaunchTemplateDiskConf)

-- | Launch template disk throughput configuration.
launchTemplateDiskConf_throughput :: Lens.Lens' LaunchTemplateDiskConf (Prelude.Maybe Prelude.Natural)
launchTemplateDiskConf_throughput = Lens.lens (\LaunchTemplateDiskConf' {throughput} -> throughput) (\s@LaunchTemplateDiskConf' {} a -> s {throughput = a} :: LaunchTemplateDiskConf)

-- | Launch template disk volume type configuration.
launchTemplateDiskConf_volumeType :: Lens.Lens' LaunchTemplateDiskConf (Prelude.Maybe VolumeType)
launchTemplateDiskConf_volumeType = Lens.lens (\LaunchTemplateDiskConf' {volumeType} -> volumeType) (\s@LaunchTemplateDiskConf' {} a -> s {volumeType = a} :: LaunchTemplateDiskConf)

instance Data.FromJSON LaunchTemplateDiskConf where
  parseJSON =
    Data.withObject
      "LaunchTemplateDiskConf"
      ( \x ->
          LaunchTemplateDiskConf'
            Prelude.<$> (x Data..:? "iops")
            Prelude.<*> (x Data..:? "throughput")
            Prelude.<*> (x Data..:? "volumeType")
      )

instance Prelude.Hashable LaunchTemplateDiskConf where
  hashWithSalt _salt LaunchTemplateDiskConf' {..} =
    _salt
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` throughput
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData LaunchTemplateDiskConf where
  rnf LaunchTemplateDiskConf' {..} =
    Prelude.rnf iops
      `Prelude.seq` Prelude.rnf throughput
      `Prelude.seq` Prelude.rnf volumeType

instance Data.ToJSON LaunchTemplateDiskConf where
  toJSON LaunchTemplateDiskConf' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iops" Data..=) Prelude.<$> iops,
            ("throughput" Data..=) Prelude.<$> throughput,
            ("volumeType" Data..=) Prelude.<$> volumeType
          ]
      )
