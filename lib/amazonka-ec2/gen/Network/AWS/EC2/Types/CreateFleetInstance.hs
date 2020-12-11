-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateFleetInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateFleetInstance
  ( CreateFleetInstance (..),

    -- * Smart constructor
    mkCreateFleetInstance,

    -- * Lenses
    cfiPlatform,
    cfiLifecycle,
    cfiLaunchTemplateAndOverrides,
    cfiInstanceType,
    cfiInstanceIds,
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
-- /See:/ 'mkCreateFleetInstance' smart constructor.
data CreateFleetInstance = CreateFleetInstance'
  { platform ::
      Lude.Maybe PlatformValues,
    lifecycle :: Lude.Maybe InstanceLifecycle,
    launchTemplateAndOverrides ::
      Lude.Maybe LaunchTemplateAndOverridesResponse,
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

-- | Creates a value of 'CreateFleetInstance' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances.
-- * 'instanceType' - The instance type.
-- * 'launchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
-- * 'lifecycle' - Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
-- * 'platform' - The value is @Windows@ for Windows instances. Otherwise, the value is blank.
mkCreateFleetInstance ::
  CreateFleetInstance
mkCreateFleetInstance =
  CreateFleetInstance'
    { platform = Lude.Nothing,
      lifecycle = Lude.Nothing,
      launchTemplateAndOverrides = Lude.Nothing,
      instanceType = Lude.Nothing,
      instanceIds = Lude.Nothing
    }

-- | The value is @Windows@ for Windows instances. Otherwise, the value is blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiPlatform :: Lens.Lens' CreateFleetInstance (Lude.Maybe PlatformValues)
cfiPlatform = Lens.lens (platform :: CreateFleetInstance -> Lude.Maybe PlatformValues) (\s a -> s {platform = a} :: CreateFleetInstance)
{-# DEPRECATED cfiPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Indicates if the instance that was launched is a Spot Instance or On-Demand Instance.
--
-- /Note:/ Consider using 'lifecycle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiLifecycle :: Lens.Lens' CreateFleetInstance (Lude.Maybe InstanceLifecycle)
cfiLifecycle = Lens.lens (lifecycle :: CreateFleetInstance -> Lude.Maybe InstanceLifecycle) (\s a -> s {lifecycle = a} :: CreateFleetInstance)
{-# DEPRECATED cfiLifecycle "Use generic-lens or generic-optics with 'lifecycle' instead." #-}

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- /Note:/ Consider using 'launchTemplateAndOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiLaunchTemplateAndOverrides :: Lens.Lens' CreateFleetInstance (Lude.Maybe LaunchTemplateAndOverridesResponse)
cfiLaunchTemplateAndOverrides = Lens.lens (launchTemplateAndOverrides :: CreateFleetInstance -> Lude.Maybe LaunchTemplateAndOverridesResponse) (\s a -> s {launchTemplateAndOverrides = a} :: CreateFleetInstance)
{-# DEPRECATED cfiLaunchTemplateAndOverrides "Use generic-lens or generic-optics with 'launchTemplateAndOverrides' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiInstanceType :: Lens.Lens' CreateFleetInstance (Lude.Maybe InstanceType)
cfiInstanceType = Lens.lens (instanceType :: CreateFleetInstance -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: CreateFleetInstance)
{-# DEPRECATED cfiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The IDs of the instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfiInstanceIds :: Lens.Lens' CreateFleetInstance (Lude.Maybe [Lude.Text])
cfiInstanceIds = Lens.lens (instanceIds :: CreateFleetInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: CreateFleetInstance)
{-# DEPRECATED cfiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Lude.FromXML CreateFleetInstance where
  parseXML x =
    CreateFleetInstance'
      Lude.<$> (x Lude..@? "platform")
      Lude.<*> (x Lude..@? "lifecycle")
      Lude.<*> (x Lude..@? "launchTemplateAndOverrides")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> ( x Lude..@? "instanceIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
