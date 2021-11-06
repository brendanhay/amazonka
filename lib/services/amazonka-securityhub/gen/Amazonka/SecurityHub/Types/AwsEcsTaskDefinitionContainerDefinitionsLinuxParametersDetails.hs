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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails

-- | >Linux-specific modifications that are applied to the container, such as
-- Linux kernel capabilities.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails = AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'
  { -- | The value for the size (in MiB) of the __\/dev\/shm__ volume.
    sharedMemorySize :: Prelude.Maybe Prelude.Int,
    -- | The container path, mount options, and size (in MiB) of the tmpfs mount.
    tmpfs :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails],
    -- | Whether to run an @init@ process inside the container that forwards
    -- signals and reaps processes.
    initProcessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Configures the container\'s memory swappiness behavior. Determines how
    -- aggressively pages are swapped. The higher the value, the more
    -- aggressive the swappiness. The default is 60.
    swappiness :: Prelude.Maybe Prelude.Int,
    -- | The host devices to expose to the container.
    devices :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails],
    -- | The Linux capabilities for the container that are added to or dropped
    -- from the default configuration provided by Docker.
    capabilities :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails,
    -- | The total amount of swap memory (in MiB) that a container can use.
    maxSwap :: Prelude.Maybe Prelude.Int
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
-- 'sharedMemorySize', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize' - The value for the size (in MiB) of the __\/dev\/shm__ volume.
--
-- 'tmpfs', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs' - The container path, mount options, and size (in MiB) of the tmpfs mount.
--
-- 'initProcessEnabled', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled' - Whether to run an @init@ process inside the container that forwards
-- signals and reaps processes.
--
-- 'swappiness', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness' - Configures the container\'s memory swappiness behavior. Determines how
-- aggressively pages are swapped. The higher the value, the more
-- aggressive the swappiness. The default is 60.
--
-- 'devices', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices' - The host devices to expose to the container.
--
-- 'capabilities', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities' - The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
--
-- 'maxSwap', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap' - The total amount of swap memory (in MiB) that a container can use.
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'
    { sharedMemorySize =
        Prelude.Nothing,
      tmpfs =
        Prelude.Nothing,
      initProcessEnabled =
        Prelude.Nothing,
      swappiness =
        Prelude.Nothing,
      devices =
        Prelude.Nothing,
      capabilities =
        Prelude.Nothing,
      maxSwap =
        Prelude.Nothing
    }

-- | The value for the size (in MiB) of the __\/dev\/shm__ volume.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_sharedMemorySize = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {sharedMemorySize} -> sharedMemorySize) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {sharedMemorySize = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The container path, mount options, and size (in MiB) of the tmpfs mount.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_tmpfs = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {tmpfs} -> tmpfs) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {tmpfs = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether to run an @init@ process inside the container that forwards
-- signals and reaps processes.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_initProcessEnabled = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {initProcessEnabled} -> initProcessEnabled) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {initProcessEnabled = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | Configures the container\'s memory swappiness behavior. Determines how
-- aggressively pages are swapped. The higher the value, the more
-- aggressive the swappiness. The default is 60.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_swappiness = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {swappiness} -> swappiness) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {swappiness = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The host devices to expose to the container.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_devices = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {devices} -> devices) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {devices = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Linux capabilities for the container that are added to or dropped
-- from the default configuration provided by Docker.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersCapabilitiesDetails)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_capabilities = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {capabilities} -> capabilities) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {capabilities = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

-- | The total amount of swap memory (in MiB) that a container can use.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails_maxSwap = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {maxSwap} -> maxSwap) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {} a -> s {maxSwap = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails'
            Prelude.<$> (x Core..:? "SharedMemorySize")
              Prelude.<*> (x Core..:? "Tmpfs" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "InitProcessEnabled")
              Prelude.<*> (x Core..:? "Swappiness")
              Prelude.<*> (x Core..:? "Devices" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Capabilities")
              Prelude.<*> (x Core..:? "MaxSwap")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("SharedMemorySize" Core..=)
                Prelude.<$> sharedMemorySize,
              ("Tmpfs" Core..=) Prelude.<$> tmpfs,
              ("InitProcessEnabled" Core..=)
                Prelude.<$> initProcessEnabled,
              ("Swappiness" Core..=) Prelude.<$> swappiness,
              ("Devices" Core..=) Prelude.<$> devices,
              ("Capabilities" Core..=) Prelude.<$> capabilities,
              ("MaxSwap" Core..=) Prelude.<$> maxSwap
            ]
        )
