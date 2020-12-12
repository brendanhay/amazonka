{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Bundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Bundle
  ( Bundle (..),

    -- * Smart constructor
    mkBundle,

    -- * Lenses
    bunCpuCount,
    bunTransferPerMonthInGb,
    bunBundleId,
    bunInstanceType,
    bunName,
    bunPower,
    bunDiskSizeInGb,
    bunSupportedPlatforms,
    bunPrice,
    bunIsActive,
    bunRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.InstancePlatform
import qualified Network.AWS.Prelude as Lude

-- | Describes a bundle, which is a set of specs describing your virtual private server (or /instance/ ).
--
-- /See:/ 'mkBundle' smart constructor.
data Bundle = Bundle'
  { cpuCount :: Lude.Maybe Lude.Int,
    transferPerMonthInGb :: Lude.Maybe Lude.Int,
    bundleId :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    power :: Lude.Maybe Lude.Int,
    diskSizeInGb :: Lude.Maybe Lude.Int,
    supportedPlatforms :: Lude.Maybe [InstancePlatform],
    price :: Lude.Maybe Lude.Double,
    isActive :: Lude.Maybe Lude.Bool,
    ramSizeInGb :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Bundle' with the minimum fields required to make a request.
--
-- * 'bundleId' - The bundle ID (e.g., @micro_1_0@ ).
-- * 'cpuCount' - The number of vCPUs included in the bundle (e.g., @2@ ).
-- * 'diskSizeInGb' - The size of the SSD (e.g., @30@ ).
-- * 'instanceType' - The Amazon EC2 instance type (e.g., @t2.micro@ ).
-- * 'isActive' - A Boolean value indicating whether the bundle is active.
-- * 'name' - A friendly name for the bundle (e.g., @Micro@ ).
-- * 'power' - A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
-- * 'price' - The price in US dollars (e.g., @5.0@ ) of the bundle.
-- * 'ramSizeInGb' - The amount of RAM in GB (e.g., @2.0@ ).
-- * 'supportedPlatforms' - The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
-- * 'transferPerMonthInGb' - The data transfer rate per month in GB (e.g., @2000@ ).
mkBundle ::
  Bundle
mkBundle =
  Bundle'
    { cpuCount = Lude.Nothing,
      transferPerMonthInGb = Lude.Nothing,
      bundleId = Lude.Nothing,
      instanceType = Lude.Nothing,
      name = Lude.Nothing,
      power = Lude.Nothing,
      diskSizeInGb = Lude.Nothing,
      supportedPlatforms = Lude.Nothing,
      price = Lude.Nothing,
      isActive = Lude.Nothing,
      ramSizeInGb = Lude.Nothing
    }

-- | The number of vCPUs included in the bundle (e.g., @2@ ).
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunCpuCount :: Lens.Lens' Bundle (Lude.Maybe Lude.Int)
bunCpuCount = Lens.lens (cpuCount :: Bundle -> Lude.Maybe Lude.Int) (\s a -> s {cpuCount = a} :: Bundle)
{-# DEPRECATED bunCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | The data transfer rate per month in GB (e.g., @2000@ ).
--
-- /Note:/ Consider using 'transferPerMonthInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunTransferPerMonthInGb :: Lens.Lens' Bundle (Lude.Maybe Lude.Int)
bunTransferPerMonthInGb = Lens.lens (transferPerMonthInGb :: Bundle -> Lude.Maybe Lude.Int) (\s a -> s {transferPerMonthInGb = a} :: Bundle)
{-# DEPRECATED bunTransferPerMonthInGb "Use generic-lens or generic-optics with 'transferPerMonthInGb' instead." #-}

-- | The bundle ID (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunBundleId :: Lens.Lens' Bundle (Lude.Maybe Lude.Text)
bunBundleId = Lens.lens (bundleId :: Bundle -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: Bundle)
{-# DEPRECATED bunBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The Amazon EC2 instance type (e.g., @t2.micro@ ).
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunInstanceType :: Lens.Lens' Bundle (Lude.Maybe Lude.Text)
bunInstanceType = Lens.lens (instanceType :: Bundle -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: Bundle)
{-# DEPRECATED bunInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A friendly name for the bundle (e.g., @Micro@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunName :: Lens.Lens' Bundle (Lude.Maybe Lude.Text)
bunName = Lens.lens (name :: Bundle -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Bundle)
{-# DEPRECATED bunName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A numeric value that represents the power of the bundle (e.g., @500@ ). You can use the bundle's power value in conjunction with a blueprint's minimum power value to determine whether the blueprint will run on the bundle. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500.
--
-- /Note:/ Consider using 'power' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunPower :: Lens.Lens' Bundle (Lude.Maybe Lude.Int)
bunPower = Lens.lens (power :: Bundle -> Lude.Maybe Lude.Int) (\s a -> s {power = a} :: Bundle)
{-# DEPRECATED bunPower "Use generic-lens or generic-optics with 'power' instead." #-}

-- | The size of the SSD (e.g., @30@ ).
--
-- /Note:/ Consider using 'diskSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunDiskSizeInGb :: Lens.Lens' Bundle (Lude.Maybe Lude.Int)
bunDiskSizeInGb = Lens.lens (diskSizeInGb :: Bundle -> Lude.Maybe Lude.Int) (\s a -> s {diskSizeInGb = a} :: Bundle)
{-# DEPRECATED bunDiskSizeInGb "Use generic-lens or generic-optics with 'diskSizeInGb' instead." #-}

-- | The operating system platform (Linux/Unix-based or Windows Server-based) that the bundle supports. You can only launch a @WINDOWS@ bundle on a blueprint that supports the @WINDOWS@ platform. @LINUX_UNIX@ blueprints require a @LINUX_UNIX@ bundle.
--
-- /Note:/ Consider using 'supportedPlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunSupportedPlatforms :: Lens.Lens' Bundle (Lude.Maybe [InstancePlatform])
bunSupportedPlatforms = Lens.lens (supportedPlatforms :: Bundle -> Lude.Maybe [InstancePlatform]) (\s a -> s {supportedPlatforms = a} :: Bundle)
{-# DEPRECATED bunSupportedPlatforms "Use generic-lens or generic-optics with 'supportedPlatforms' instead." #-}

-- | The price in US dollars (e.g., @5.0@ ) of the bundle.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunPrice :: Lens.Lens' Bundle (Lude.Maybe Lude.Double)
bunPrice = Lens.lens (price :: Bundle -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: Bundle)
{-# DEPRECATED bunPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | A Boolean value indicating whether the bundle is active.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunIsActive :: Lens.Lens' Bundle (Lude.Maybe Lude.Bool)
bunIsActive = Lens.lens (isActive :: Bundle -> Lude.Maybe Lude.Bool) (\s a -> s {isActive = a} :: Bundle)
{-# DEPRECATED bunIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | The amount of RAM in GB (e.g., @2.0@ ).
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bunRamSizeInGb :: Lens.Lens' Bundle (Lude.Maybe Lude.Double)
bunRamSizeInGb = Lens.lens (ramSizeInGb :: Bundle -> Lude.Maybe Lude.Double) (\s a -> s {ramSizeInGb = a} :: Bundle)
{-# DEPRECATED bunRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Lude.FromJSON Bundle where
  parseJSON =
    Lude.withObject
      "Bundle"
      ( \x ->
          Bundle'
            Lude.<$> (x Lude..:? "cpuCount")
            Lude.<*> (x Lude..:? "transferPerMonthInGb")
            Lude.<*> (x Lude..:? "bundleId")
            Lude.<*> (x Lude..:? "instanceType")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "power")
            Lude.<*> (x Lude..:? "diskSizeInGb")
            Lude.<*> (x Lude..:? "supportedPlatforms" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "price")
            Lude.<*> (x Lude..:? "isActive")
            Lude.<*> (x Lude..:? "ramSizeInGb")
      )
