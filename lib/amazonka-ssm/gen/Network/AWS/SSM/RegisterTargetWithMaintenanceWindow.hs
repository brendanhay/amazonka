{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a target with a maintenance window.
module Network.AWS.SSM.RegisterTargetWithMaintenanceWindow
  ( -- * Creating a request
    RegisterTargetWithMaintenanceWindow (..),
    mkRegisterTargetWithMaintenanceWindow,

    -- ** Request lenses
    rWindowId,
    rResourceType,
    rTargets,
    rClientToken,
    rDescription,
    rName,
    rOwnerInformation,

    -- * Destructuring the response
    RegisterTargetWithMaintenanceWindowResponse (..),
    mkRegisterTargetWithMaintenanceWindowResponse,

    -- ** Response lenses
    rrsWindowTargetId,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkRegisterTargetWithMaintenanceWindow' smart constructor.
data RegisterTargetWithMaintenanceWindow = RegisterTargetWithMaintenanceWindow'
  { -- | The ID of the maintenance window the target should be registered with.
    windowId :: Types.MaintenanceWindowId,
    -- | The type of target being registered with the maintenance window.
    resourceType :: Types.MaintenanceWindowResourceType,
    -- | The targets to register with the maintenance window. In other words, the instances to run commands on when the maintenance window runs.
    --
    -- You can specify targets using instance IDs, resource group names, or tags that have been applied to instances.
    -- __Example 1__ : Specify instance IDs
    -- @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
    -- __Example 2__ : Use tag key-pairs applied to instances
    -- @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
    -- __Example 3__ : Use tag-keys applied to instances
    -- @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
    -- __Example 4__ : Use resource group names
    -- @Key=resource-groups:Name,Values=/resource-group-name/ @
    -- __Example 5__ : Use filters for resource group types
    -- @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
    -- For more information about these examples formats, including the best use case for each one, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window> in the /AWS Systems Manager User Guide/ .
    targets :: [Types.Target],
    -- | User-provided idempotency token.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | An optional description for the target.
    description :: Core.Maybe Types.MaintenanceWindowDescription,
    -- | An optional name for the target.
    name :: Core.Maybe Types.MaintenanceWindowName,
    -- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
    ownerInformation :: Core.Maybe Types.OwnerInformation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTargetWithMaintenanceWindow' value with any optional fields omitted.
mkRegisterTargetWithMaintenanceWindow ::
  -- | 'windowId'
  Types.MaintenanceWindowId ->
  -- | 'resourceType'
  Types.MaintenanceWindowResourceType ->
  RegisterTargetWithMaintenanceWindow
mkRegisterTargetWithMaintenanceWindow windowId resourceType =
  RegisterTargetWithMaintenanceWindow'
    { windowId,
      resourceType,
      targets = Core.mempty,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing,
      ownerInformation = Core.Nothing
    }

-- | The ID of the maintenance window the target should be registered with.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWindowId :: Lens.Lens' RegisterTargetWithMaintenanceWindow Types.MaintenanceWindowId
rWindowId = Lens.field @"windowId"
{-# DEPRECATED rWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The type of target being registered with the maintenance window.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceType :: Lens.Lens' RegisterTargetWithMaintenanceWindow Types.MaintenanceWindowResourceType
rResourceType = Lens.field @"resourceType"
{-# DEPRECATED rResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The targets to register with the maintenance window. In other words, the instances to run commands on when the maintenance window runs.
--
-- You can specify targets using instance IDs, resource group names, or tags that have been applied to instances.
-- __Example 1__ : Specify instance IDs
-- @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
-- __Example 2__ : Use tag key-pairs applied to instances
-- @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
-- __Example 3__ : Use tag-keys applied to instances
-- @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
-- __Example 4__ : Use resource group names
-- @Key=resource-groups:Name,Values=/resource-group-name/ @
-- __Example 5__ : Use filters for resource group types
-- @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
-- For more information about these examples formats, including the best use case for each one, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/mw-cli-tutorial-targets-examples.html Examples: Register targets with a maintenance window> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTargets :: Lens.Lens' RegisterTargetWithMaintenanceWindow [Types.Target]
rTargets = Lens.field @"targets"
{-# DEPRECATED rTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClientToken :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Core.Maybe Types.ClientToken)
rClientToken = Lens.field @"clientToken"
{-# DEPRECATED rClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | An optional description for the target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Core.Maybe Types.MaintenanceWindowDescription)
rDescription = Lens.field @"description"
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | An optional name for the target.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Core.Maybe Types.MaintenanceWindowName)
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
--
-- /Note:/ Consider using 'ownerInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOwnerInformation :: Lens.Lens' RegisterTargetWithMaintenanceWindow (Core.Maybe Types.OwnerInformation)
rOwnerInformation = Lens.field @"ownerInformation"
{-# DEPRECATED rOwnerInformation "Use generic-lens or generic-optics with 'ownerInformation' instead." #-}

instance Core.FromJSON RegisterTargetWithMaintenanceWindow where
  toJSON RegisterTargetWithMaintenanceWindow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            Core.Just ("ResourceType" Core..= resourceType),
            Core.Just ("Targets" Core..= targets),
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("Description" Core..=) Core.<$> description,
            ("Name" Core..=) Core.<$> name,
            ("OwnerInformation" Core..=) Core.<$> ownerInformation
          ]
      )

instance Core.AWSRequest RegisterTargetWithMaintenanceWindow where
  type
    Rs RegisterTargetWithMaintenanceWindow =
      RegisterTargetWithMaintenanceWindowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonSSM.RegisterTargetWithMaintenanceWindow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTargetWithMaintenanceWindowResponse'
            Core.<$> (x Core..:? "WindowTargetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterTargetWithMaintenanceWindowResponse' smart constructor.
data RegisterTargetWithMaintenanceWindowResponse = RegisterTargetWithMaintenanceWindowResponse'
  { -- | The ID of the target definition in this maintenance window.
    windowTargetId :: Core.Maybe Types.MaintenanceWindowTargetId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTargetWithMaintenanceWindowResponse' value with any optional fields omitted.
mkRegisterTargetWithMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterTargetWithMaintenanceWindowResponse
mkRegisterTargetWithMaintenanceWindowResponse responseStatus =
  RegisterTargetWithMaintenanceWindowResponse'
    { windowTargetId =
        Core.Nothing,
      responseStatus
    }

-- | The ID of the target definition in this maintenance window.
--
-- /Note:/ Consider using 'windowTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsWindowTargetId :: Lens.Lens' RegisterTargetWithMaintenanceWindowResponse (Core.Maybe Types.MaintenanceWindowTargetId)
rrsWindowTargetId = Lens.field @"windowTargetId"
{-# DEPRECATED rrsWindowTargetId "Use generic-lens or generic-optics with 'windowTargetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RegisterTargetWithMaintenanceWindowResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
