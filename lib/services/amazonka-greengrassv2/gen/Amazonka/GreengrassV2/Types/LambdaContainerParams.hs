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
-- Module      : Amazonka.GreengrassV2.Types.LambdaContainerParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaContainerParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.LambdaDeviceMount
import Amazonka.GreengrassV2.Types.LambdaVolumeMount
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a container in which Lambda functions run on
-- Greengrass core devices.
--
-- /See:/ 'newLambdaContainerParams' smart constructor.
data LambdaContainerParams = LambdaContainerParams'
  { -- | The list of system devices that the container can access.
    devices :: Prelude.Maybe [LambdaDeviceMount],
    -- | The memory size of the container, expressed in kilobytes.
    --
    -- Default: @16384@ (16 MB)
    memorySizeInKB :: Prelude.Maybe Prelude.Int,
    -- | Whether or not the container can read information from the device\'s
    -- @\/sys@ folder.
    --
    -- Default: @false@
    mountROSysfs :: Prelude.Maybe Prelude.Bool,
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
-- 'devices', 'lambdaContainerParams_devices' - The list of system devices that the container can access.
--
-- 'memorySizeInKB', 'lambdaContainerParams_memorySizeInKB' - The memory size of the container, expressed in kilobytes.
--
-- Default: @16384@ (16 MB)
--
-- 'mountROSysfs', 'lambdaContainerParams_mountROSysfs' - Whether or not the container can read information from the device\'s
-- @\/sys@ folder.
--
-- Default: @false@
--
-- 'volumes', 'lambdaContainerParams_volumes' - The list of volumes that the container can access.
newLambdaContainerParams ::
  LambdaContainerParams
newLambdaContainerParams =
  LambdaContainerParams'
    { devices = Prelude.Nothing,
      memorySizeInKB = Prelude.Nothing,
      mountROSysfs = Prelude.Nothing,
      volumes = Prelude.Nothing
    }

-- | The list of system devices that the container can access.
lambdaContainerParams_devices :: Lens.Lens' LambdaContainerParams (Prelude.Maybe [LambdaDeviceMount])
lambdaContainerParams_devices = Lens.lens (\LambdaContainerParams' {devices} -> devices) (\s@LambdaContainerParams' {} a -> s {devices = a} :: LambdaContainerParams) Prelude.. Lens.mapping Lens.coerced

-- | The memory size of the container, expressed in kilobytes.
--
-- Default: @16384@ (16 MB)
lambdaContainerParams_memorySizeInKB :: Lens.Lens' LambdaContainerParams (Prelude.Maybe Prelude.Int)
lambdaContainerParams_memorySizeInKB = Lens.lens (\LambdaContainerParams' {memorySizeInKB} -> memorySizeInKB) (\s@LambdaContainerParams' {} a -> s {memorySizeInKB = a} :: LambdaContainerParams)

-- | Whether or not the container can read information from the device\'s
-- @\/sys@ folder.
--
-- Default: @false@
lambdaContainerParams_mountROSysfs :: Lens.Lens' LambdaContainerParams (Prelude.Maybe Prelude.Bool)
lambdaContainerParams_mountROSysfs = Lens.lens (\LambdaContainerParams' {mountROSysfs} -> mountROSysfs) (\s@LambdaContainerParams' {} a -> s {mountROSysfs = a} :: LambdaContainerParams)

-- | The list of volumes that the container can access.
lambdaContainerParams_volumes :: Lens.Lens' LambdaContainerParams (Prelude.Maybe [LambdaVolumeMount])
lambdaContainerParams_volumes = Lens.lens (\LambdaContainerParams' {volumes} -> volumes) (\s@LambdaContainerParams' {} a -> s {volumes = a} :: LambdaContainerParams) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable LambdaContainerParams where
  hashWithSalt _salt LambdaContainerParams' {..} =
    _salt
      `Prelude.hashWithSalt` devices
      `Prelude.hashWithSalt` memorySizeInKB
      `Prelude.hashWithSalt` mountROSysfs
      `Prelude.hashWithSalt` volumes

instance Prelude.NFData LambdaContainerParams where
  rnf LambdaContainerParams' {..} =
    Prelude.rnf devices `Prelude.seq`
      Prelude.rnf memorySizeInKB `Prelude.seq`
        Prelude.rnf mountROSysfs `Prelude.seq`
          Prelude.rnf volumes

instance Data.ToJSON LambdaContainerParams where
  toJSON LambdaContainerParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("devices" Data..=) Prelude.<$> devices,
            ("memorySizeInKB" Data..=)
              Prelude.<$> memorySizeInKB,
            ("mountROSysfs" Data..=) Prelude.<$> mountROSysfs,
            ("volumes" Data..=) Prelude.<$> volumes
          ]
      )
