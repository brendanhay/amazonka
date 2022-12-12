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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails

-- | >Linux-specific modifications that are applied to the container, such as
-- Linux kernel capabilities.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails = AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'
  { -- | The Linux capabilities for the container that are added to or dropped
    -- from the default configuration provided by Docker.
    capabilities :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails,
    -- | The host devices to expose to the container.
    devices :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails],
    -- | Whether to run an @init@ process inside the container that forwards
    -- signals and reaps processes.
    initProcessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The total amount of swap memory (in MiB) that a container can use.
    maxSwap :: Prelude.Maybe Prelude.Int,
    -- | The value for the size (in MiB) of the __\/dev\/shm__ volume.
    sharedMemorySize :: Prelude.Maybe Prelude.Int,
    -- | Configures the container\'s memory swappiness behavior. Determines how
    -- aggressively pages are swapped. The higher the value, the more
    -- aggressive the swappiness. The default is 60.
    swappiness :: Prelude.Maybe Prelude.Int,
    -- | The container path, mount options, and size (in MiB) of the tmpfs mount.
    tmpfs :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities' - The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
--
-- 'devices', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices' - The host devices to expose to the container.
--
-- 'initProcessEnabled', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled' - Whether to run an @init@ process inside the container that forwards
-- signals and reaps processes.
--
-- 'maxSwap', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap' - The total amount of swap memory (in MiB) that a container can use.
--
-- 'sharedMemorySize', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize' - The value for the size (in MiB) of the __\/dev\/shm__ volume.
--
-- 'swappiness', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness' - Configures the container\'s memory swappiness behavior. Determines how
-- aggressively pages are swapped. The higher the value, the more
-- aggressive the swappiness. The default is 60.
--
-- 'tmpfs', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs' - The container path, mount options, and size (in MiB) of the tmpfs mount.
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'
    { capabilities =
        Prelude.Nothing,
      devices =
        Prelude.Nothing,
      initProcessEnabled =
        Prelude.Nothing,
      maxSwap =
        Prelude.Nothing,
      sharedMemorySize =
        Prelude.Nothing,
      swappiness =
        Prelude.Nothing,
      tmpfs =
        Prelude.Nothing
    }

-- | The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {capabilities} -> capabilities) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {capabilities = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The host devices to expose to the container.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {devices} -> devices) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {devices = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether to run an @init@ process inside the container that forwards
-- signals and reaps processes.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {initProcessEnabled} -> initProcessEnabled) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {initProcessEnabled = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The total amount of swap memory (in MiB) that a container can use.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {maxSwap} -> maxSwap) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {maxSwap = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The value for the size (in MiB) of the __\/dev\/shm__ volume.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {sharedMemorySize} -> sharedMemorySize) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {sharedMemorySize = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | Configures the container\'s memory swappiness behavior. Determines how
-- aggressively pages are swapped. The higher the value, the more
-- aggressive the swappiness. The default is 60.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {swappiness} -> swappiness) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {swappiness = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The container path, mount options, and size (in MiB) of the tmpfs mount.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {tmpfs} -> tmpfs) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {tmpfs = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'
            Prelude.<$> (x Data..:? "Capabilities")
              Prelude.<*> (x Data..:? "Devices" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "InitProcessEnabled")
              Prelude.<*> (x Data..:? "MaxSwap")
              Prelude.<*> (x Data..:? "SharedMemorySize")
              Prelude.<*> (x Data..:? "Swappiness")
              Prelude.<*> (x Data..:? "Tmpfs" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {..} =
      _salt `Prelude.hashWithSalt` capabilities
        `Prelude.hashWithSalt` devices
        `Prelude.hashWithSalt` initProcessEnabled
        `Prelude.hashWithSalt` maxSwap
        `Prelude.hashWithSalt` sharedMemorySize
        `Prelude.hashWithSalt` swappiness
        `Prelude.hashWithSalt` tmpfs

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {..} =
      Prelude.rnf capabilities
        `Prelude.seq` Prelude.rnf devices
        `Prelude.seq` Prelude.rnf initProcessEnabled
        `Prelude.seq` Prelude.rnf maxSwap
        `Prelude.seq` Prelude.rnf sharedMemorySize
        `Prelude.seq` Prelude.rnf swappiness
        `Prelude.seq` Prelude.rnf tmpfs

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Capabilities" Data..=) Prelude.<$> capabilities,
              ("Devices" Data..=) Prelude.<$> devices,
              ("InitProcessEnabled" Data..=)
                Prelude.<$> initProcessEnabled,
              ("MaxSwap" Data..=) Prelude.<$> maxSwap,
              ("SharedMemorySize" Data..=)
                Prelude.<$> sharedMemorySize,
              ("Swappiness" Data..=) Prelude.<$> swappiness,
              ("Tmpfs" Data..=) Prelude.<$> tmpfs
            ]
        )
