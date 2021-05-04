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
-- Module      : Network.AWS.Lightsail.Types.ContainerServicePower
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServicePower where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the powers that can be specified for an Amazon Lightsail
-- container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base
-- price of the container service.
--
-- /See:/ 'newContainerServicePower' smart constructor.
data ContainerServicePower = ContainerServicePower'
  { -- | The amount of RAM (in GB) of the power.
    ramSizeInGb :: Prelude.Maybe Prelude.Double,
    -- | A Boolean value indicating whether the power is active and can be
    -- specified for container services.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | The friendly name of the power (e.g., @nano@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of vCPUs included in the power.
    cpuCount :: Prelude.Maybe Prelude.Double,
    -- | The monthly price of the power in USD.
    price :: Prelude.Maybe Prelude.Double,
    -- | The ID of the power (e.g., @nano-1@).
    powerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerServicePower' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ramSizeInGb', 'containerServicePower_ramSizeInGb' - The amount of RAM (in GB) of the power.
--
-- 'isActive', 'containerServicePower_isActive' - A Boolean value indicating whether the power is active and can be
-- specified for container services.
--
-- 'name', 'containerServicePower_name' - The friendly name of the power (e.g., @nano@).
--
-- 'cpuCount', 'containerServicePower_cpuCount' - The number of vCPUs included in the power.
--
-- 'price', 'containerServicePower_price' - The monthly price of the power in USD.
--
-- 'powerId', 'containerServicePower_powerId' - The ID of the power (e.g., @nano-1@).
newContainerServicePower ::
  ContainerServicePower
newContainerServicePower =
  ContainerServicePower'
    { ramSizeInGb =
        Prelude.Nothing,
      isActive = Prelude.Nothing,
      name = Prelude.Nothing,
      cpuCount = Prelude.Nothing,
      price = Prelude.Nothing,
      powerId = Prelude.Nothing
    }

-- | The amount of RAM (in GB) of the power.
containerServicePower_ramSizeInGb :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Double)
containerServicePower_ramSizeInGb = Lens.lens (\ContainerServicePower' {ramSizeInGb} -> ramSizeInGb) (\s@ContainerServicePower' {} a -> s {ramSizeInGb = a} :: ContainerServicePower)

-- | A Boolean value indicating whether the power is active and can be
-- specified for container services.
containerServicePower_isActive :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Bool)
containerServicePower_isActive = Lens.lens (\ContainerServicePower' {isActive} -> isActive) (\s@ContainerServicePower' {} a -> s {isActive = a} :: ContainerServicePower)

-- | The friendly name of the power (e.g., @nano@).
containerServicePower_name :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Text)
containerServicePower_name = Lens.lens (\ContainerServicePower' {name} -> name) (\s@ContainerServicePower' {} a -> s {name = a} :: ContainerServicePower)

-- | The number of vCPUs included in the power.
containerServicePower_cpuCount :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Double)
containerServicePower_cpuCount = Lens.lens (\ContainerServicePower' {cpuCount} -> cpuCount) (\s@ContainerServicePower' {} a -> s {cpuCount = a} :: ContainerServicePower)

-- | The monthly price of the power in USD.
containerServicePower_price :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Double)
containerServicePower_price = Lens.lens (\ContainerServicePower' {price} -> price) (\s@ContainerServicePower' {} a -> s {price = a} :: ContainerServicePower)

-- | The ID of the power (e.g., @nano-1@).
containerServicePower_powerId :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Text)
containerServicePower_powerId = Lens.lens (\ContainerServicePower' {powerId} -> powerId) (\s@ContainerServicePower' {} a -> s {powerId = a} :: ContainerServicePower)

instance Prelude.FromJSON ContainerServicePower where
  parseJSON =
    Prelude.withObject
      "ContainerServicePower"
      ( \x ->
          ContainerServicePower'
            Prelude.<$> (x Prelude..:? "ramSizeInGb")
            Prelude.<*> (x Prelude..:? "isActive")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "cpuCount")
            Prelude.<*> (x Prelude..:? "price")
            Prelude.<*> (x Prelude..:? "powerId")
      )

instance Prelude.Hashable ContainerServicePower

instance Prelude.NFData ContainerServicePower
