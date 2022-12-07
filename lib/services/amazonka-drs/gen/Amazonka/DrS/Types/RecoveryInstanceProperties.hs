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
-- Module      : Amazonka.DrS.Types.RecoveryInstanceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.RecoveryInstanceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.CPU
import Amazonka.DrS.Types.IdentificationHints
import Amazonka.DrS.Types.NetworkInterface
import Amazonka.DrS.Types.OS
import Amazonka.DrS.Types.RecoveryInstanceDisk
import qualified Amazonka.Prelude as Prelude

-- | Properties of the Recovery Instance machine.
--
-- /See:/ 'newRecoveryInstanceProperties' smart constructor.
data RecoveryInstanceProperties = RecoveryInstanceProperties'
  { -- | Operating system.
    os :: Prelude.Maybe OS,
    -- | An array of CPUs.
    cpus :: Prelude.Maybe [CPU],
    -- | The amount of RAM in bytes.
    ramBytes :: Prelude.Maybe Prelude.Natural,
    -- | An array of disks.
    disks :: Prelude.Maybe [RecoveryInstanceDisk],
    -- | Hints used to uniquely identify a machine.
    identificationHints :: Prelude.Maybe IdentificationHints,
    -- | The date and time the Recovery Instance properties were last updated on.
    lastUpdatedDateTime :: Prelude.Maybe Prelude.Text,
    -- | An array of network interfaces.
    networkInterfaces :: Prelude.Maybe [NetworkInterface]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryInstanceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'os', 'recoveryInstanceProperties_os' - Operating system.
--
-- 'cpus', 'recoveryInstanceProperties_cpus' - An array of CPUs.
--
-- 'ramBytes', 'recoveryInstanceProperties_ramBytes' - The amount of RAM in bytes.
--
-- 'disks', 'recoveryInstanceProperties_disks' - An array of disks.
--
-- 'identificationHints', 'recoveryInstanceProperties_identificationHints' - Hints used to uniquely identify a machine.
--
-- 'lastUpdatedDateTime', 'recoveryInstanceProperties_lastUpdatedDateTime' - The date and time the Recovery Instance properties were last updated on.
--
-- 'networkInterfaces', 'recoveryInstanceProperties_networkInterfaces' - An array of network interfaces.
newRecoveryInstanceProperties ::
  RecoveryInstanceProperties
newRecoveryInstanceProperties =
  RecoveryInstanceProperties'
    { os = Prelude.Nothing,
      cpus = Prelude.Nothing,
      ramBytes = Prelude.Nothing,
      disks = Prelude.Nothing,
      identificationHints = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      networkInterfaces = Prelude.Nothing
    }

-- | Operating system.
recoveryInstanceProperties_os :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe OS)
recoveryInstanceProperties_os = Lens.lens (\RecoveryInstanceProperties' {os} -> os) (\s@RecoveryInstanceProperties' {} a -> s {os = a} :: RecoveryInstanceProperties)

-- | An array of CPUs.
recoveryInstanceProperties_cpus :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe [CPU])
recoveryInstanceProperties_cpus = Lens.lens (\RecoveryInstanceProperties' {cpus} -> cpus) (\s@RecoveryInstanceProperties' {} a -> s {cpus = a} :: RecoveryInstanceProperties) Prelude.. Lens.mapping Lens.coerced

-- | The amount of RAM in bytes.
recoveryInstanceProperties_ramBytes :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe Prelude.Natural)
recoveryInstanceProperties_ramBytes = Lens.lens (\RecoveryInstanceProperties' {ramBytes} -> ramBytes) (\s@RecoveryInstanceProperties' {} a -> s {ramBytes = a} :: RecoveryInstanceProperties)

-- | An array of disks.
recoveryInstanceProperties_disks :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe [RecoveryInstanceDisk])
recoveryInstanceProperties_disks = Lens.lens (\RecoveryInstanceProperties' {disks} -> disks) (\s@RecoveryInstanceProperties' {} a -> s {disks = a} :: RecoveryInstanceProperties) Prelude.. Lens.mapping Lens.coerced

-- | Hints used to uniquely identify a machine.
recoveryInstanceProperties_identificationHints :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe IdentificationHints)
recoveryInstanceProperties_identificationHints = Lens.lens (\RecoveryInstanceProperties' {identificationHints} -> identificationHints) (\s@RecoveryInstanceProperties' {} a -> s {identificationHints = a} :: RecoveryInstanceProperties)

-- | The date and time the Recovery Instance properties were last updated on.
recoveryInstanceProperties_lastUpdatedDateTime :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe Prelude.Text)
recoveryInstanceProperties_lastUpdatedDateTime = Lens.lens (\RecoveryInstanceProperties' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RecoveryInstanceProperties' {} a -> s {lastUpdatedDateTime = a} :: RecoveryInstanceProperties)

-- | An array of network interfaces.
recoveryInstanceProperties_networkInterfaces :: Lens.Lens' RecoveryInstanceProperties (Prelude.Maybe [NetworkInterface])
recoveryInstanceProperties_networkInterfaces = Lens.lens (\RecoveryInstanceProperties' {networkInterfaces} -> networkInterfaces) (\s@RecoveryInstanceProperties' {} a -> s {networkInterfaces = a} :: RecoveryInstanceProperties) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecoveryInstanceProperties where
  parseJSON =
    Data.withObject
      "RecoveryInstanceProperties"
      ( \x ->
          RecoveryInstanceProperties'
            Prelude.<$> (x Data..:? "os")
            Prelude.<*> (x Data..:? "cpus" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ramBytes")
            Prelude.<*> (x Data..:? "disks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "identificationHints")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> ( x Data..:? "networkInterfaces"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RecoveryInstanceProperties where
  hashWithSalt _salt RecoveryInstanceProperties' {..} =
    _salt `Prelude.hashWithSalt` os
      `Prelude.hashWithSalt` cpus
      `Prelude.hashWithSalt` ramBytes
      `Prelude.hashWithSalt` disks
      `Prelude.hashWithSalt` identificationHints
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` networkInterfaces

instance Prelude.NFData RecoveryInstanceProperties where
  rnf RecoveryInstanceProperties' {..} =
    Prelude.rnf os
      `Prelude.seq` Prelude.rnf cpus
      `Prelude.seq` Prelude.rnf ramBytes
      `Prelude.seq` Prelude.rnf disks
      `Prelude.seq` Prelude.rnf identificationHints
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf networkInterfaces
