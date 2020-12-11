-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesConfiguration
  ( ReservedInstancesConfiguration (..),

    -- * Smart constructor
    mkReservedInstancesConfiguration,

    -- * Lenses
    ricPlatform,
    ricInstanceCount,
    ricInstanceType,
    ricAvailabilityZone,
    ricScope,
  )
where

import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Scope
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration settings for the modified Reserved Instances.
--
-- /See:/ 'mkReservedInstancesConfiguration' smart constructor.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration'
  { platform ::
      Lude.Maybe Lude.Text,
    instanceCount ::
      Lude.Maybe Lude.Int,
    instanceType ::
      Lude.Maybe InstanceType,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    scope :: Lude.Maybe Scope
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstancesConfiguration' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone for the modified Reserved Instances.
-- * 'instanceCount' - The number of modified Reserved Instances.
-- * 'instanceType' - The instance type for the modified Reserved Instances.
-- * 'platform' - The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
-- * 'scope' - Whether the Reserved Instance is applied to instances in a Region or instances in a specific Availability Zone.
mkReservedInstancesConfiguration ::
  ReservedInstancesConfiguration
mkReservedInstancesConfiguration =
  ReservedInstancesConfiguration'
    { platform = Lude.Nothing,
      instanceCount = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      scope = Lude.Nothing
    }

-- | The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricPlatform :: Lens.Lens' ReservedInstancesConfiguration (Lude.Maybe Lude.Text)
ricPlatform = Lens.lens (platform :: ReservedInstancesConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: ReservedInstancesConfiguration)
{-# DEPRECATED ricPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The number of modified Reserved Instances.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricInstanceCount :: Lens.Lens' ReservedInstancesConfiguration (Lude.Maybe Lude.Int)
ricInstanceCount = Lens.lens (instanceCount :: ReservedInstancesConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ReservedInstancesConfiguration)
{-# DEPRECATED ricInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The instance type for the modified Reserved Instances.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricInstanceType :: Lens.Lens' ReservedInstancesConfiguration (Lude.Maybe InstanceType)
ricInstanceType = Lens.lens (instanceType :: ReservedInstancesConfiguration -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: ReservedInstancesConfiguration)
{-# DEPRECATED ricInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone for the modified Reserved Instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricAvailabilityZone :: Lens.Lens' ReservedInstancesConfiguration (Lude.Maybe Lude.Text)
ricAvailabilityZone = Lens.lens (availabilityZone :: ReservedInstancesConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ReservedInstancesConfiguration)
{-# DEPRECATED ricAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Whether the Reserved Instance is applied to instances in a Region or instances in a specific Availability Zone.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricScope :: Lens.Lens' ReservedInstancesConfiguration (Lude.Maybe Scope)
ricScope = Lens.lens (scope :: ReservedInstancesConfiguration -> Lude.Maybe Scope) (\s a -> s {scope = a} :: ReservedInstancesConfiguration)
{-# DEPRECATED ricScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Lude.FromXML ReservedInstancesConfiguration where
  parseXML x =
    ReservedInstancesConfiguration'
      Lude.<$> (x Lude..@? "platform")
      Lude.<*> (x Lude..@? "instanceCount")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "scope")

instance Lude.ToQuery ReservedInstancesConfiguration where
  toQuery ReservedInstancesConfiguration' {..} =
    Lude.mconcat
      [ "Platform" Lude.=: platform,
        "InstanceCount" Lude.=: instanceCount,
        "InstanceType" Lude.=: instanceType,
        "AvailabilityZone" Lude.=: availabilityZone,
        "Scope" Lude.=: scope
      ]
