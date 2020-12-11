-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetsInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFleetsInstances
  ( DescribeFleetsInstances (..),

    -- * Smart constructor
    mkDescribeFleetsInstances,

    -- * Lenses
    dfiPlatform,
    dfiLifecycle,
    dfiLaunchTemplateAndOverrides,
    dfiInstanceType,
    dfiInstanceIds,
  )
where

import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.EC2.Types.PlatformValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the instances that were launched by the fleet.
--
-- /See:/ 'mkDescribeFleetsInstances' smart constructor.
data DescribeFleetsInstances = DescribeFleetsInstances'
  { platform ::
      Lude.Maybe PlatformValues,
    lifecycle :: Lude.Maybe InstanceLifecycle,
    launchTemplateAndOverrides ::
      Lude.Maybe
        LaunchTemplateAndOverridesResponse,
    instanceType :: Lude.Maybe InstanceType,
    instanceIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetsInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances.
-- * 'instanceType' - The instance type.
-- * 'launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
-- * 'lifecycle' - Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
-- * 'platform' - The value is @Windows@ for Windows instances. Otherwise, the value is blank.
mkDescribeFleetsInstances ::
  DescribeFleetsInstances
mkDescribeFleetsInstances =
  DescribeFleetsInstances'
    { platform = Lude.Nothing,
      lifecycle = Lude.Nothing,
      launchTemplateAndOverrides = Lude.Nothing,
      instanceType = Lude.Nothing,
      instanceIds = Lude.Nothing
    }

-- | The value is @Windows@ for Windows instances. Otherwise, the value is blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiPlatform :: Lens.Lens' DescribeFleetsInstances (Lude.Maybe PlatformValues)
dfiPlatform = Lens.lens (platform :: DescribeFleetsInstances -> Lude.Maybe PlatformValues) (\s a -> s {platform = a} :: DescribeFleetsInstances)
{-# DEPRECATED dfiPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiLifecycle :: Lens.Lens' DescribeFleetsInstances (Lude.Maybe InstanceLifecycle)
dfiLifecycle = Lens.lens (lifecycle :: DescribeFleetsInstances -> Lude.Maybe InstanceLifecycle) (\s a -> s {lifecycle = a} :: DescribeFleetsInstances)
{-# DEPRECATED dfiLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiLaunchTemplateAndOverrides :: Lens.Lens' DescribeFleetsInstances (Lude.Maybe LaunchTemplateAndOverridesResponse)
dfiLaunchTemplateAndOverrides = Lens.lens (launchTemplateAndOverrides :: DescribeFleetsInstances -> Lude.Maybe LaunchTemplateAndOverridesResponse) (\s a -> s {launchTemplateAndOverrides = a} :: DescribeFleetsInstances)
{-# DEPRECATED dfiLaunchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiInstanceType :: Lens.Lens' DescribeFleetsInstances (Lude.Maybe InstanceType)
dfiInstanceType = Lens.lens (instanceType :: DescribeFleetsInstances -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: DescribeFleetsInstances)
{-# DEPRECATED dfiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiInstanceIds :: Lens.Lens' DescribeFleetsInstances (Lude.Maybe [Lude.Text])
dfiInstanceIds = Lens.lens (instanceIds :: DescribeFleetsInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeFleetsInstances)
{-# DEPRECATED dfiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Lude.FromXML DescribeFleetsInstances where
  parseXML x =
    DescribeFleetsInstances'
      Lude.<$> (x Lude..@? "platform")
      Lude.<*> (x Lude..@? "lifecycle")
      Lude.<*> (x Lude..@? "launchTemplateAndOverrides")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> ( x Lude..@? "instanceIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
