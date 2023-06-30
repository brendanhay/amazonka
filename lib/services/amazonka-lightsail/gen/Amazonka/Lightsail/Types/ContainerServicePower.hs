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
-- Module      : Amazonka.Lightsail.Types.ContainerServicePower
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServicePower where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the powers that can be specified for an Amazon Lightsail
-- container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base
-- price of the container service.
--
-- /See:/ 'newContainerServicePower' smart constructor.
data ContainerServicePower = ContainerServicePower'
  { -- | The number of vCPUs included in the power.
    cpuCount :: Prelude.Maybe Prelude.Double,
    -- | A Boolean value indicating whether the power is active and can be
    -- specified for container services.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | The friendly name of the power (e.g., @nano@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the power (e.g., @nano-1@).
    powerId :: Prelude.Maybe Prelude.Text,
    -- | The monthly price of the power in USD.
    price :: Prelude.Maybe Prelude.Double,
    -- | The amount of RAM (in GB) of the power.
    ramSizeInGb :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServicePower' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCount', 'containerServicePower_cpuCount' - The number of vCPUs included in the power.
--
-- 'isActive', 'containerServicePower_isActive' - A Boolean value indicating whether the power is active and can be
-- specified for container services.
--
-- 'name', 'containerServicePower_name' - The friendly name of the power (e.g., @nano@).
--
-- 'powerId', 'containerServicePower_powerId' - The ID of the power (e.g., @nano-1@).
--
-- 'price', 'containerServicePower_price' - The monthly price of the power in USD.
--
-- 'ramSizeInGb', 'containerServicePower_ramSizeInGb' - The amount of RAM (in GB) of the power.
newContainerServicePower ::
  ContainerServicePower
newContainerServicePower =
  ContainerServicePower'
    { cpuCount = Prelude.Nothing,
      isActive = Prelude.Nothing,
      name = Prelude.Nothing,
      powerId = Prelude.Nothing,
      price = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing
    }

-- | The number of vCPUs included in the power.
containerServicePower_cpuCount :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Double)
containerServicePower_cpuCount = Lens.lens (\ContainerServicePower' {cpuCount} -> cpuCount) (\s@ContainerServicePower' {} a -> s {cpuCount = a} :: ContainerServicePower)

-- | A Boolean value indicating whether the power is active and can be
-- specified for container services.
containerServicePower_isActive :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Bool)
containerServicePower_isActive = Lens.lens (\ContainerServicePower' {isActive} -> isActive) (\s@ContainerServicePower' {} a -> s {isActive = a} :: ContainerServicePower)

-- | The friendly name of the power (e.g., @nano@).
containerServicePower_name :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Text)
containerServicePower_name = Lens.lens (\ContainerServicePower' {name} -> name) (\s@ContainerServicePower' {} a -> s {name = a} :: ContainerServicePower)

-- | The ID of the power (e.g., @nano-1@).
containerServicePower_powerId :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Text)
containerServicePower_powerId = Lens.lens (\ContainerServicePower' {powerId} -> powerId) (\s@ContainerServicePower' {} a -> s {powerId = a} :: ContainerServicePower)

-- | The monthly price of the power in USD.
containerServicePower_price :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Double)
containerServicePower_price = Lens.lens (\ContainerServicePower' {price} -> price) (\s@ContainerServicePower' {} a -> s {price = a} :: ContainerServicePower)

-- | The amount of RAM (in GB) of the power.
containerServicePower_ramSizeInGb :: Lens.Lens' ContainerServicePower (Prelude.Maybe Prelude.Double)
containerServicePower_ramSizeInGb = Lens.lens (\ContainerServicePower' {ramSizeInGb} -> ramSizeInGb) (\s@ContainerServicePower' {} a -> s {ramSizeInGb = a} :: ContainerServicePower)

instance Data.FromJSON ContainerServicePower where
  parseJSON =
    Data.withObject
      "ContainerServicePower"
      ( \x ->
          ContainerServicePower'
            Prelude.<$> (x Data..:? "cpuCount")
            Prelude.<*> (x Data..:? "isActive")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "powerId")
            Prelude.<*> (x Data..:? "price")
            Prelude.<*> (x Data..:? "ramSizeInGb")
      )

instance Prelude.Hashable ContainerServicePower where
  hashWithSalt _salt ContainerServicePower' {..} =
    _salt
      `Prelude.hashWithSalt` cpuCount
      `Prelude.hashWithSalt` isActive
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` powerId
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` ramSizeInGb

instance Prelude.NFData ContainerServicePower where
  rnf ContainerServicePower' {..} =
    Prelude.rnf cpuCount
      `Prelude.seq` Prelude.rnf isActive
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf powerId
      `Prelude.seq` Prelude.rnf price
      `Prelude.seq` Prelude.rnf ramSizeInGb
