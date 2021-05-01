{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DeviceFarm.Types.DevicePoolType
import Network.AWS.DeviceFarm.Types.Rule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a collection of device types.
--
-- /See:/ 'newDevicePool' smart constructor.
data DevicePool = DevicePool'
  { -- | Information about the device pool\'s rules.
    rules :: Prelude.Maybe [Rule],
    -- | The device pool\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The device pool\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of devices that Device Farm can add to your device pool.
    -- Device Farm adds devices that are available and meet the criteria that
    -- you assign for the @rules@ parameter. Depending on how many devices meet
    -- these constraints, your device pool might contain fewer devices than the
    -- value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs
    -- that you incur by running tests.
    maxDevices :: Prelude.Maybe Prelude.Int,
    -- | The device pool\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device pool\'s type.
    --
    -- Allowed values include:
    --
    -- -   CURATED: A device pool that is created and managed by AWS Device
    --     Farm.
    --
    -- -   PRIVATE: A device pool that is created and managed by the device
    --     pool developer.
    type' :: Prelude.Maybe DevicePoolType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { rules = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      maxDevices = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Information about the device pool\'s rules.
devicePool_rules :: Lens.Lens' DevicePool (Prelude.Maybe [Rule])
devicePool_rules = Lens.lens (\DevicePool' {rules} -> rules) (\s@DevicePool' {} a -> s {rules = a} :: DevicePool) Prelude.. Lens.mapping Prelude._Coerce

-- | The device pool\'s ARN.
devicePool_arn :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Text)
devicePool_arn = Lens.lens (\DevicePool' {arn} -> arn) (\s@DevicePool' {} a -> s {arn = a} :: DevicePool)

-- | The device pool\'s name.
devicePool_name :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Text)
devicePool_name = Lens.lens (\DevicePool' {name} -> name) (\s@DevicePool' {} a -> s {name = a} :: DevicePool)

-- | The number of devices that Device Farm can add to your device pool.
-- Device Farm adds devices that are available and meet the criteria that
-- you assign for the @rules@ parameter. Depending on how many devices meet
-- these constraints, your device pool might contain fewer devices than the
-- value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs
-- that you incur by running tests.
devicePool_maxDevices :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Int)
devicePool_maxDevices = Lens.lens (\DevicePool' {maxDevices} -> maxDevices) (\s@DevicePool' {} a -> s {maxDevices = a} :: DevicePool)

-- | The device pool\'s description.
devicePool_description :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Text)
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
devicePool_type :: Lens.Lens' DevicePool (Prelude.Maybe DevicePoolType)
devicePool_type = Lens.lens (\DevicePool' {type'} -> type') (\s@DevicePool' {} a -> s {type' = a} :: DevicePool)

instance Prelude.FromJSON DevicePool where
  parseJSON =
    Prelude.withObject
      "DevicePool"
      ( \x ->
          DevicePool'
            Prelude.<$> (x Prelude..:? "rules" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "maxDevices")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable DevicePool

instance Prelude.NFData DevicePool
