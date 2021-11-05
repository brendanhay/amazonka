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
-- Module      : Network.AWS.GreengrassV2.Types.LambdaContainerParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GreengrassV2.Types.LambdaContainerParams where

import qualified Network.AWS.Core as Core
import Network.AWS.GreengrassV2.Types.LambdaDeviceMount
import Network.AWS.GreengrassV2.Types.LambdaVolumeMount
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a container in which Lambda functions run on
-- Greengrass core devices.
--
-- /See:/ 'newLambdaContainerParams' smart constructor.
data LambdaContainerParams = LambdaContainerParams'
  { -- | Whether or not the container can read information from the device\'s
    -- @\/sys@ folder.
    --
    -- Default: @false@
    mountROSysfs :: Prelude.Maybe Prelude.Bool,
    -- | The memory size of the container, expressed in kilobytes.
    --
    -- Default: @16384@ (16 MB)
    memorySizeInKB :: Prelude.Maybe Prelude.Int,
    -- | The list of system devices that the container can access.
    devices :: Prelude.Maybe [LambdaDeviceMount],
    -- | The list of volumes that the container can access.
    volumes :: Prelude.Maybe [LambdaVolumeMount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaContainerParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountROSysfs', 'lambdaContainerParams_mountROSysfs' - Whether or not the container can read information from the device\'s
-- @\/sys@ folder.
--
-- Default: @false@
--
-- 'memorySizeInKB', 'lambdaContainerParams_memorySizeInKB' - The memory size of the container, expressed in kilobytes.
--
-- Default: @16384@ (16 MB)
--
-- 'devices', 'lambdaContainerParams_devices' - The list of system devices that the container can access.
--
-- 'volumes', 'lambdaContainerParams_volumes' - The list of volumes that the container can access.
newLambdaContainerParams ::
  LambdaContainerParams
newLambdaContainerParams =
  LambdaContainerParams'
    { mountROSysfs =
        Prelude.Nothing,
      memorySizeInKB = Prelude.Nothing,
      devices = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | Whether or not the container can read information from the device\'s
-- @\/sys@ folder.
--
-- Default: @false@
lambdaContainerParams_mountROSysfs :: Lens.Lens' LambdaContainerParams (Prelude.Maybe Prelude.Bool)
lambdaContainerParams_mountROSysfs = Lens.lens (\LambdaContainerParams' {mountROSysfs} -> mountROSysfs) (\s@LambdaContainerParams' {} a -> s {mountROSysfs = a} :: LambdaContainerParams)

-- | The memory size of the container, expressed in kilobytes.
--
-- Default: @16384@ (16 MB)
lambdaContainerParams_memorySizeInKB :: Lens.Lens' LambdaContainerParams (Prelude.Maybe Prelude.Int)
lambdaContainerParams_memorySizeInKB = Lens.lens (\LambdaContainerParams' {memorySizeInKB} -> memorySizeInKB) (\s@LambdaContainerParams' {} a -> s {memorySizeInKB = a} :: LambdaContainerParams)

-- | The list of system devices that the container can access.
lambdaContainerParams_devices :: Lens.Lens' LambdaContainerParams (Prelude.Maybe [LambdaDeviceMount])
lambdaContainerParams_devices = Lens.lens (\LambdaContainerParams' {devices} -> devices) (\s@LambdaContainerParams' {} a -> s {devices = a} :: LambdaContainerParams) Prelude.. Lens.mapping Lens.coerced

-- | The list of volumes that the container can access.
lambdaContainerParams_volumes :: Lens.Lens' LambdaContainerParams (Prelude.Maybe [LambdaVolumeMount])
lambdaContainerParams_volumes = Lens.lens (\LambdaContainerParams' {volumes} -> volumes) (\s@LambdaContainerParams' {} a -> s {volumes = a} :: LambdaContainerParams) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable LambdaContainerParams

instance Prelude.NFData LambdaContainerParams

instance Core.ToJSON LambdaContainerParams where
  toJSON LambdaContainerParams' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("mountROSysfs" Core..=) Prelude.<$> mountROSysfs,
            ("memorySizeInKB" Core..=)
              Prelude.<$> memorySizeInKB,
            ("devices" Core..=) Prelude.<$> devices,
            ("volumes" Core..=) Prelude.<$> volumes
          ]
      )
