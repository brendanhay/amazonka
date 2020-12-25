{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing protection group. A protection group is a grouping of protected resources so they can be handled as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
module Network.AWS.Shield.UpdateProtectionGroup
  ( -- * Creating a request
    UpdateProtectionGroup (..),
    mkUpdateProtectionGroup,

    -- ** Request lenses
    upgProtectionGroupId,
    upgAggregation,
    upgPattern,
    upgMembers,
    upgResourceType,

    -- * Destructuring the response
    UpdateProtectionGroupResponse (..),
    mkUpdateProtectionGroupResponse,

    -- ** Response lenses
    upgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkUpdateProtectionGroup' smart constructor.
data UpdateProtectionGroup = UpdateProtectionGroup'
  { -- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
    protectionGroupId :: Types.ProtectionGroupId,
    -- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
    --
    --
    --     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
    --
    --
    --     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
    --
    --
    --     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
    aggregation :: Types.ProtectionGroupAggregation,
    -- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
    pattern' :: Types.ProtectionGroupPattern,
    -- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: Core.Maybe [Types.ResourceArn],
    -- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
    resourceType :: Core.Maybe Types.ProtectedResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProtectionGroup' value with any optional fields omitted.
mkUpdateProtectionGroup ::
  -- | 'protectionGroupId'
  Types.ProtectionGroupId ->
  -- | 'aggregation'
  Types.ProtectionGroupAggregation ->
  -- | 'pattern\''
  Types.ProtectionGroupPattern ->
  UpdateProtectionGroup
mkUpdateProtectionGroup protectionGroupId aggregation pattern' =
  UpdateProtectionGroup'
    { protectionGroupId,
      aggregation,
      pattern',
      members = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgProtectionGroupId :: Lens.Lens' UpdateProtectionGroup Types.ProtectionGroupId
upgProtectionGroupId = Lens.field @"protectionGroupId"
{-# DEPRECATED upgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

-- | Defines how AWS Shield combines resource data for the group in order to detect, mitigate, and report events.
--
--
--     * Sum - Use the total traffic across the group. This is a good choice for most cases. Examples include Elastic IP addresses for EC2 instances that scale manually or automatically.
--
--
--     * Mean - Use the average of the traffic across the group. This is a good choice for resources that share traffic uniformly. Examples include accelerators and load balancers.
--
--
--     * Max - Use the highest traffic from each resource. This is useful for resources that don't share traffic and for resources that share that traffic in a non-uniform way. Examples include CloudFront distributions and origin resources for CloudFront distributions.
--
--
--
-- /Note:/ Consider using 'aggregation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgAggregation :: Lens.Lens' UpdateProtectionGroup Types.ProtectionGroupAggregation
upgAggregation = Lens.field @"aggregation"
{-# DEPRECATED upgAggregation "Use generic-lens or generic-optics with 'aggregation' instead." #-}

-- | The criteria to use to choose the protected resources for inclusion in the group. You can include all resources that have protections, provide a list of resource Amazon Resource Names (ARNs), or include all resources of a specified resource type.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgPattern :: Lens.Lens' UpdateProtectionGroup Types.ProtectionGroupPattern
upgPattern = Lens.field @"pattern'"
{-# DEPRECATED upgPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

-- | The Amazon Resource Names (ARNs) of the resources to include in the protection group. You must set this when you set @Pattern@ to @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgMembers :: Lens.Lens' UpdateProtectionGroup (Core.Maybe [Types.ResourceArn])
upgMembers = Lens.field @"members"
{-# DEPRECATED upgMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The resource type to include in the protection group. All protected resources of this type are included in the protection group. You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not set it for any other @Pattern@ setting.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgResourceType :: Lens.Lens' UpdateProtectionGroup (Core.Maybe Types.ProtectedResourceType)
upgResourceType = Lens.field @"resourceType"
{-# DEPRECATED upgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON UpdateProtectionGroup where
  toJSON UpdateProtectionGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProtectionGroupId" Core..= protectionGroupId),
            Core.Just ("Aggregation" Core..= aggregation),
            Core.Just ("Pattern" Core..= pattern'),
            ("Members" Core..=) Core.<$> members,
            ("ResourceType" Core..=) Core.<$> resourceType
          ]
      )

instance Core.AWSRequest UpdateProtectionGroup where
  type Rs UpdateProtectionGroup = UpdateProtectionGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.UpdateProtectionGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProtectionGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProtectionGroupResponse' smart constructor.
newtype UpdateProtectionGroupResponse = UpdateProtectionGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProtectionGroupResponse' value with any optional fields omitted.
mkUpdateProtectionGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProtectionGroupResponse
mkUpdateProtectionGroupResponse responseStatus =
  UpdateProtectionGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upgrrsResponseStatus :: Lens.Lens' UpdateProtectionGroupResponse Core.Int
upgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED upgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
