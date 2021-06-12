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
-- Module      : Network.AWS.DeviceFarm.Types.DevicePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePool where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.DevicePoolType
import Network.AWS.DeviceFarm.Types.Rule
import qualified Network.AWS.Lens as Lens

-- | Represents a collection of device types.
--
-- /See:/ 'newDevicePool' smart constructor.
data DevicePool = DevicePool'
  { -- | Information about the device pool\'s rules.
    rules :: Core.Maybe [Rule],
    -- | The device pool\'s ARN.
    arn :: Core.Maybe Core.Text,
    -- | The device pool\'s name.
    name :: Core.Maybe Core.Text,
    -- | The number of devices that Device Farm can add to your device pool.
    -- Device Farm adds devices that are available and meet the criteria that
    -- you assign for the @rules@ parameter. Depending on how many devices meet
    -- these constraints, your device pool might contain fewer devices than the
    -- value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs
    -- that you incur by running tests.
    maxDevices :: Core.Maybe Core.Int,
    -- | The device pool\'s description.
    description :: Core.Maybe Core.Text,
    -- | The device pool\'s type.
    --
    -- Allowed values include:
    --
    -- -   CURATED: A device pool that is created and managed by AWS Device
    --     Farm.
    --
    -- -   PRIVATE: A device pool that is created and managed by the device
    --     pool developer.
    type' :: Core.Maybe DevicePoolType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'devicePool_rules' - Information about the device pool\'s rules.
--
-- 'arn', 'devicePool_arn' - The device pool\'s ARN.
--
-- 'name', 'devicePool_name' - The device pool\'s name.
--
-- 'maxDevices', 'devicePool_maxDevices' - The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and meet the criteria that
-- you assign for the @rules@ parameter. Depending on how many devices meet
-- these constraints, your device pool might contain fewer devices than the
-- value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
--
-- 'description', 'devicePool_description' - The device pool\'s description.
--
-- 'type'', 'devicePool_type' - The device pool\'s type.
--
-- Allowed values include:
--
-- -   CURATED: A device pool that is created and managed by AWS Device
--     Farm.
--
-- -   PRIVATE: A device pool that is created and managed by the device
--     pool developer.
newDevicePool ::
  DevicePool
newDevicePool =
  DevicePool'
    { rules = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      maxDevices = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing
    }

-- | Information about the device pool\'s rules.
devicePool_rules :: Lens.Lens' DevicePool (Core.Maybe [Rule])
devicePool_rules = Lens.lens (\DevicePool' {rules} -> rules) (\s@DevicePool' {} a -> s {rules = a} :: DevicePool) Core.. Lens.mapping Lens._Coerce

-- | The device pool\'s ARN.
devicePool_arn :: Lens.Lens' DevicePool (Core.Maybe Core.Text)
devicePool_arn = Lens.lens (\DevicePool' {arn} -> arn) (\s@DevicePool' {} a -> s {arn = a} :: DevicePool)

-- | The device pool\'s name.
devicePool_name :: Lens.Lens' DevicePool (Core.Maybe Core.Text)
devicePool_name = Lens.lens (\DevicePool' {name} -> name) (\s@DevicePool' {} a -> s {name = a} :: DevicePool)

-- | The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and meet the criteria that
-- you assign for the @rules@ parameter. Depending on how many devices meet
-- these constraints, your device pool might contain fewer devices than the
-- value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
devicePool_maxDevices :: Lens.Lens' DevicePool (Core.Maybe Core.Int)
devicePool_maxDevices = Lens.lens (\DevicePool' {maxDevices} -> maxDevices) (\s@DevicePool' {} a -> s {maxDevices = a} :: DevicePool)

-- | The device pool\'s description.
devicePool_description :: Lens.Lens' DevicePool (Core.Maybe Core.Text)
devicePool_description = Lens.lens (\DevicePool' {description} -> description) (\s@DevicePool' {} a -> s {description = a} :: DevicePool)

-- | The device pool\'s type.
--
-- Allowed values include:
--
-- -   CURATED: A device pool that is created and managed by AWS Device
--     Farm.
--
-- -   PRIVATE: A device pool that is created and managed by the device
--     pool developer.
devicePool_type :: Lens.Lens' DevicePool (Core.Maybe DevicePoolType)
devicePool_type = Lens.lens (\DevicePool' {type'} -> type') (\s@DevicePool' {} a -> s {type' = a} :: DevicePool)

instance Core.FromJSON DevicePool where
  parseJSON =
    Core.withObject
      "DevicePool"
      ( \x ->
          DevicePool'
            Core.<$> (x Core..:? "rules" Core..!= Core.mempty)
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "maxDevices")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable DevicePool

instance Core.NFData DevicePool
