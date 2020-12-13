{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServicePower
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServicePower
  ( ContainerServicePower (..),

    -- * Smart constructor
    mkContainerServicePower,

    -- * Lenses
    cspPowerId,
    cspCpuCount,
    cspName,
    cspPrice,
    cspIsActive,
    cspRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the powers that can be specified for an Amazon Lightsail container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
--
-- /See:/ 'mkContainerServicePower' smart constructor.
data ContainerServicePower = ContainerServicePower'
  { -- | The ID of the power (e.g., @nano-1@ ).
    powerId :: Lude.Maybe Lude.Text,
    -- | The number of vCPUs included in the power.
    cpuCount :: Lude.Maybe Lude.Double,
    -- | The friendly name of the power (e.g., @nano@ ).
    name :: Lude.Maybe Lude.Text,
    -- | The monthly price of the power in USD.
    price :: Lude.Maybe Lude.Double,
    -- | A Boolean value indicating whether the power is active and can be specified for container services.
    isActive :: Lude.Maybe Lude.Bool,
    -- | The amount of RAM (in GB) of the power.
    ramSizeInGb :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerServicePower' with the minimum fields required to make a request.
--
-- * 'powerId' - The ID of the power (e.g., @nano-1@ ).
-- * 'cpuCount' - The number of vCPUs included in the power.
-- * 'name' - The friendly name of the power (e.g., @nano@ ).
-- * 'price' - The monthly price of the power in USD.
-- * 'isActive' - A Boolean value indicating whether the power is active and can be specified for container services.
-- * 'ramSizeInGb' - The amount of RAM (in GB) of the power.
mkContainerServicePower ::
  ContainerServicePower
mkContainerServicePower =
  ContainerServicePower'
    { powerId = Lude.Nothing,
      cpuCount = Lude.Nothing,
      name = Lude.Nothing,
      price = Lude.Nothing,
      isActive = Lude.Nothing,
      ramSizeInGb = Lude.Nothing
    }

-- | The ID of the power (e.g., @nano-1@ ).
--
-- /Note:/ Consider using 'powerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspPowerId :: Lens.Lens' ContainerServicePower (Lude.Maybe Lude.Text)
cspPowerId = Lens.lens (powerId :: ContainerServicePower -> Lude.Maybe Lude.Text) (\s a -> s {powerId = a} :: ContainerServicePower)
{-# DEPRECATED cspPowerId "Use generic-lens or generic-optics with 'powerId' instead." #-}

-- | The number of vCPUs included in the power.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspCpuCount :: Lens.Lens' ContainerServicePower (Lude.Maybe Lude.Double)
cspCpuCount = Lens.lens (cpuCount :: ContainerServicePower -> Lude.Maybe Lude.Double) (\s a -> s {cpuCount = a} :: ContainerServicePower)
{-# DEPRECATED cspCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The friendly name of the power (e.g., @nano@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspName :: Lens.Lens' ContainerServicePower (Lude.Maybe Lude.Text)
cspName = Lens.lens (name :: ContainerServicePower -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ContainerServicePower)
{-# DEPRECATED cspName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The monthly price of the power in USD.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspPrice :: Lens.Lens' ContainerServicePower (Lude.Maybe Lude.Double)
cspPrice = Lens.lens (price :: ContainerServicePower -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: ContainerServicePower)
{-# DEPRECATED cspPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | A Boolean value indicating whether the power is active and can be specified for container services.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspIsActive :: Lens.Lens' ContainerServicePower (Lude.Maybe Lude.Bool)
cspIsActive = Lens.lens (isActive :: ContainerServicePower -> Lude.Maybe Lude.Bool) (\s a -> s {isActive = a} :: ContainerServicePower)
{-# DEPRECATED cspIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | The amount of RAM (in GB) of the power.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspRamSizeInGb :: Lens.Lens' ContainerServicePower (Lude.Maybe Lude.Double)
cspRamSizeInGb = Lens.lens (ramSizeInGb :: ContainerServicePower -> Lude.Maybe Lude.Double) (\s a -> s {ramSizeInGb = a} :: ContainerServicePower)
{-# DEPRECATED cspRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Lude.FromJSON ContainerServicePower where
  parseJSON =
    Lude.withObject
      "ContainerServicePower"
      ( \x ->
          ContainerServicePower'
            Lude.<$> (x Lude..:? "powerId")
            Lude.<*> (x Lude..:? "cpuCount")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "price")
            Lude.<*> (x Lude..:? "isActive")
            Lude.<*> (x Lude..:? "ramSizeInGb")
      )
