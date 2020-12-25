{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupModifyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupModifyConfig
  ( InstanceGroupModifyConfig (..),

    -- * Smart constructor
    mkInstanceGroupModifyConfig,

    -- * Lenses
    igmcInstanceGroupId,
    igmcConfigurations,
    igmcEC2InstanceIdsToTerminate,
    igmcInstanceCount,
    igmcShrinkPolicy,
  )
where

import qualified Network.AWS.EMR.Types.Configuration as Types
import qualified Network.AWS.EMR.Types.InstanceId as Types
import qualified Network.AWS.EMR.Types.ShrinkPolicy as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Modify the size or configurations of an instance group.
--
-- /See:/ 'mkInstanceGroupModifyConfig' smart constructor.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig'
  { -- | Unique ID of the instance group to modify.
    instanceGroupId :: Types.XmlStringMaxLen256,
    -- | A list of new or modified configurations to apply for an instance group.
    configurations :: Core.Maybe [Types.Configuration],
    -- | The EC2 InstanceIds to terminate. After you terminate the instances, the instance group will not return to its original requested size.
    eC2InstanceIdsToTerminate :: Core.Maybe [Types.InstanceId],
    -- | Target size for the instance group.
    instanceCount :: Core.Maybe Core.Int,
    -- | Policy for customizing shrink operations.
    shrinkPolicy :: Core.Maybe Types.ShrinkPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceGroupModifyConfig' value with any optional fields omitted.
mkInstanceGroupModifyConfig ::
  -- | 'instanceGroupId'
  Types.XmlStringMaxLen256 ->
  InstanceGroupModifyConfig
mkInstanceGroupModifyConfig instanceGroupId =
  InstanceGroupModifyConfig'
    { instanceGroupId,
      configurations = Core.Nothing,
      eC2InstanceIdsToTerminate = Core.Nothing,
      instanceCount = Core.Nothing,
      shrinkPolicy = Core.Nothing
    }

-- | Unique ID of the instance group to modify.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcInstanceGroupId :: Lens.Lens' InstanceGroupModifyConfig Types.XmlStringMaxLen256
igmcInstanceGroupId = Lens.field @"instanceGroupId"
{-# DEPRECATED igmcInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | A list of new or modified configurations to apply for an instance group.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcConfigurations :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe [Types.Configuration])
igmcConfigurations = Lens.field @"configurations"
{-# DEPRECATED igmcConfigurations "Use generic-lens or generic-optics with 'configurations' instead." #-}

-- | The EC2 InstanceIds to terminate. After you terminate the instances, the instance group will not return to its original requested size.
--
-- /Note:/ Consider using 'eC2InstanceIdsToTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcEC2InstanceIdsToTerminate :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe [Types.InstanceId])
igmcEC2InstanceIdsToTerminate = Lens.field @"eC2InstanceIdsToTerminate"
{-# DEPRECATED igmcEC2InstanceIdsToTerminate "Use generic-lens or generic-optics with 'eC2InstanceIdsToTerminate' instead." #-}

-- | Target size for the instance group.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcInstanceCount :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe Core.Int)
igmcInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED igmcInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | Policy for customizing shrink operations.
--
-- /Note:/ Consider using 'shrinkPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igmcShrinkPolicy :: Lens.Lens' InstanceGroupModifyConfig (Core.Maybe Types.ShrinkPolicy)
igmcShrinkPolicy = Lens.field @"shrinkPolicy"
{-# DEPRECATED igmcShrinkPolicy "Use generic-lens or generic-optics with 'shrinkPolicy' instead." #-}

instance Core.FromJSON InstanceGroupModifyConfig where
  toJSON InstanceGroupModifyConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InstanceGroupId" Core..= instanceGroupId),
            ("Configurations" Core..=) Core.<$> configurations,
            ("EC2InstanceIdsToTerminate" Core..=)
              Core.<$> eC2InstanceIdsToTerminate,
            ("InstanceCount" Core..=) Core.<$> instanceCount,
            ("ShrinkPolicy" Core..=) Core.<$> shrinkPolicy
          ]
      )
