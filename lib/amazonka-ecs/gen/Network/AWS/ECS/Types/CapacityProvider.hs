{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProvider
  ( CapacityProvider (..),

    -- * Smart constructor
    mkCapacityProvider,

    -- * Lenses
    cpAutoScalingGroupProvider,
    cpCapacityProviderArn,
    cpName,
    cpStatus,
    cpTags,
    cpUpdateStatus,
    cpUpdateStatusReason,
  )
where

import qualified Network.AWS.ECS.Types.AutoScalingGroupProvider as Types
import qualified Network.AWS.ECS.Types.CapacityProviderStatus as Types
import qualified Network.AWS.ECS.Types.CapacityProviderUpdateStatus as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.ECS.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a capacity provider.
--
-- /See:/ 'mkCapacityProvider' smart constructor.
data CapacityProvider = CapacityProvider'
  { -- | The Auto Scaling group settings for the capacity provider.
    autoScalingGroupProvider :: Core.Maybe Types.AutoScalingGroupProvider,
    -- | The Amazon Resource Name (ARN) that identifies the capacity provider.
    capacityProviderArn :: Core.Maybe Types.String,
    -- | The name of the capacity provider.
    name :: Core.Maybe Types.String,
    -- | The current status of the capacity provider. Only capacity providers in an @ACTIVE@ state can be used in a cluster. When a capacity provider is successfully deleted, it will have an @INACTIVE@ status.
    status :: Core.Maybe Types.CapacityProviderStatus,
    -- | The metadata that you apply to the capacity provider to help you categorize and organize it. Each tag consists of a key and an optional value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    --     * Maximum number of tags per resource - 50
    --
    --
    --     * For each resource, each tag key must be unique, and each tag key can have only one value.
    --
    --
    --     * Maximum key length - 128 Unicode characters in UTF-8
    --
    --
    --     * Maximum value length - 256 Unicode characters in UTF-8
    --
    --
    --     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
    --
    --
    --     * Tag keys and values are case-sensitive.
    --
    --
    --     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
    tags :: Core.Maybe [Types.Tag],
    -- | The update status of the capacity provider. The following are the possible states that will be returned.
    --
    --
    --     * DELETE_IN_PROGRESS
    --
    --     * The capacity provider is in the process of being deleted.
    --
    --
    --     * DELETE_COMPLETE
    --
    --     * The capacity provider has been successfully deleted and will have an @INACTIVE@ status.
    --
    --
    --     * DELETE_FAILED
    --
    --     * The capacity provider was unable to be deleted. The update status reason will provide further details about why the delete failed.
    updateStatus :: Core.Maybe Types.CapacityProviderUpdateStatus,
    -- | The update status reason. This provides further details about the update status for the capacity provider.
    updateStatusReason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityProvider' value with any optional fields omitted.
mkCapacityProvider ::
  CapacityProvider
mkCapacityProvider =
  CapacityProvider'
    { autoScalingGroupProvider = Core.Nothing,
      capacityProviderArn = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      updateStatus = Core.Nothing,
      updateStatusReason = Core.Nothing
    }

-- | The Auto Scaling group settings for the capacity provider.
--
-- /Note:/ Consider using 'autoScalingGroupProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAutoScalingGroupProvider :: Lens.Lens' CapacityProvider (Core.Maybe Types.AutoScalingGroupProvider)
cpAutoScalingGroupProvider = Lens.field @"autoScalingGroupProvider"
{-# DEPRECATED cpAutoScalingGroupProvider "Use generic-lens or generic-optics with 'autoScalingGroupProvider' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the capacity provider.
--
-- /Note:/ Consider using 'capacityProviderArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCapacityProviderArn :: Lens.Lens' CapacityProvider (Core.Maybe Types.String)
cpCapacityProviderArn = Lens.field @"capacityProviderArn"
{-# DEPRECATED cpCapacityProviderArn "Use generic-lens or generic-optics with 'capacityProviderArn' instead." #-}

-- | The name of the capacity provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CapacityProvider (Core.Maybe Types.String)
cpName = Lens.field @"name"
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The current status of the capacity provider. Only capacity providers in an @ACTIVE@ state can be used in a cluster. When a capacity provider is successfully deleted, it will have an @INACTIVE@ status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpStatus :: Lens.Lens' CapacityProvider (Core.Maybe Types.CapacityProviderStatus)
cpStatus = Lens.field @"status"
{-# DEPRECATED cpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The metadata that you apply to the capacity provider to help you categorize and organize it. Each tag consists of a key and an optional value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per resource - 50
--
--
--     * For each resource, each tag key must be unique, and each tag key can have only one value.
--
--
--     * Maximum key length - 128 Unicode characters in UTF-8
--
--
--     * Maximum value length - 256 Unicode characters in UTF-8
--
--
--     * If your tagging schema is used across multiple services and resources, remember that other services may have restrictions on allowed characters. Generally allowed characters are: letters, numbers, and spaces representable in UTF-8, and the following characters: + - = . _ : / @.
--
--
--     * Tag keys and values are case-sensitive.
--
--
--     * Do not use @aws:@ , @AWS:@ , or any upper or lowercase combination of such as a prefix for either keys or values as it is reserved for AWS use. You cannot edit or delete tag keys or values with this prefix. Tags with this prefix do not count against your tags per resource limit.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CapacityProvider (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The update status of the capacity provider. The following are the possible states that will be returned.
--
--
--     * DELETE_IN_PROGRESS
--
--     * The capacity provider is in the process of being deleted.
--
--
--     * DELETE_COMPLETE
--
--     * The capacity provider has been successfully deleted and will have an @INACTIVE@ status.
--
--
--     * DELETE_FAILED
--
--     * The capacity provider was unable to be deleted. The update status reason will provide further details about why the delete failed.
--
--
--
-- /Note:/ Consider using 'updateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUpdateStatus :: Lens.Lens' CapacityProvider (Core.Maybe Types.CapacityProviderUpdateStatus)
cpUpdateStatus = Lens.field @"updateStatus"
{-# DEPRECATED cpUpdateStatus "Use generic-lens or generic-optics with 'updateStatus' instead." #-}

-- | The update status reason. This provides further details about the update status for the capacity provider.
--
-- /Note:/ Consider using 'updateStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpUpdateStatusReason :: Lens.Lens' CapacityProvider (Core.Maybe Types.String)
cpUpdateStatusReason = Lens.field @"updateStatusReason"
{-# DEPRECATED cpUpdateStatusReason "Use generic-lens or generic-optics with 'updateStatusReason' instead." #-}

instance Core.FromJSON CapacityProvider where
  parseJSON =
    Core.withObject "CapacityProvider" Core.$
      \x ->
        CapacityProvider'
          Core.<$> (x Core..:? "autoScalingGroupProvider")
          Core.<*> (x Core..:? "capacityProviderArn")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "updateStatus")
          Core.<*> (x Core..:? "updateStatusReason")
