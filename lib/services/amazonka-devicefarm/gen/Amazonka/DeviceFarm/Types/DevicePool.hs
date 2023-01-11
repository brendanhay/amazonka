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
-- Module      : Amazonka.DeviceFarm.Types.DevicePool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.DevicePool where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.DevicePoolType
import Amazonka.DeviceFarm.Types.Rule
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of device types.
--
-- /See:/ 'newDevicePool' smart constructor.
data DevicePool = DevicePool'
  { -- | The device pool\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The device pool\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of devices that Device Farm can add to your device pool.
    -- Device Farm adds devices that are available and meet the criteria that
    -- you assign for the @rules@ parameter. Depending on how many devices meet
    -- these constraints, your device pool might contain fewer devices than the
    -- value for this parameter.
    --
    -- By specifying the maximum number of devices, you can control the costs
    -- that you incur by running tests.
    maxDevices :: Prelude.Maybe Prelude.Int,
    -- | The device pool\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the device pool\'s rules.
    rules :: Prelude.Maybe [Rule],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DevicePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'devicePool_arn' - The device pool\'s ARN.
--
-- 'description', 'devicePool_description' - The device pool\'s description.
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
-- 'name', 'devicePool_name' - The device pool\'s name.
--
-- 'rules', 'devicePool_rules' - Information about the device pool\'s rules.
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
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      maxDevices = Prelude.Nothing,
      name = Prelude.Nothing,
      rules = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The device pool\'s ARN.
devicePool_arn :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Text)
devicePool_arn = Lens.lens (\DevicePool' {arn} -> arn) (\s@DevicePool' {} a -> s {arn = a} :: DevicePool)

-- | The device pool\'s description.
devicePool_description :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Text)
devicePool_description = Lens.lens (\DevicePool' {description} -> description) (\s@DevicePool' {} a -> s {description = a} :: DevicePool)

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

-- | The device pool\'s name.
devicePool_name :: Lens.Lens' DevicePool (Prelude.Maybe Prelude.Text)
devicePool_name = Lens.lens (\DevicePool' {name} -> name) (\s@DevicePool' {} a -> s {name = a} :: DevicePool)

-- | Information about the device pool\'s rules.
devicePool_rules :: Lens.Lens' DevicePool (Prelude.Maybe [Rule])
devicePool_rules = Lens.lens (\DevicePool' {rules} -> rules) (\s@DevicePool' {} a -> s {rules = a} :: DevicePool) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromJSON DevicePool where
  parseJSON =
    Data.withObject
      "DevicePool"
      ( \x ->
          DevicePool'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "maxDevices")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable DevicePool where
  hashWithSalt _salt DevicePool' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxDevices
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DevicePool where
  rnf DevicePool' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxDevices
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf type'
