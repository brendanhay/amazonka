{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RemoveThingFromBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the given thing from the billing group.
module Network.AWS.IoT.RemoveThingFromBillingGroup
  ( -- * Creating a request
    RemoveThingFromBillingGroup (..),
    mkRemoveThingFromBillingGroup,

    -- ** Request lenses
    rtfbgBillingGroupArn,
    rtfbgBillingGroupName,
    rtfbgThingArn,
    rtfbgThingName,

    -- * Destructuring the response
    RemoveThingFromBillingGroupResponse (..),
    mkRemoveThingFromBillingGroupResponse,

    -- ** Response lenses
    rtfbgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveThingFromBillingGroup' smart constructor.
data RemoveThingFromBillingGroup = RemoveThingFromBillingGroup'
  { -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Types.BillingGroupArn,
    -- | The name of the billing group.
    billingGroupName :: Core.Maybe Types.BillingGroupName,
    -- | The ARN of the thing to be removed from the billing group.
    thingArn :: Core.Maybe Types.ThingArn,
    -- | The name of the thing to be removed from the billing group.
    thingName :: Core.Maybe Types.ThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveThingFromBillingGroup' value with any optional fields omitted.
mkRemoveThingFromBillingGroup ::
  RemoveThingFromBillingGroup
mkRemoveThingFromBillingGroup =
  RemoveThingFromBillingGroup'
    { billingGroupArn = Core.Nothing,
      billingGroupName = Core.Nothing,
      thingArn = Core.Nothing,
      thingName = Core.Nothing
    }

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgBillingGroupArn :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Types.BillingGroupArn)
rtfbgBillingGroupArn = Lens.field @"billingGroupArn"
{-# DEPRECATED rtfbgBillingGroupArn "Use generic-lens or generic-optics with 'billingGroupArn' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgBillingGroupName :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Types.BillingGroupName)
rtfbgBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED rtfbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The ARN of the thing to be removed from the billing group.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgThingArn :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Types.ThingArn)
rtfbgThingArn = Lens.field @"thingArn"
{-# DEPRECATED rtfbgThingArn "Use generic-lens or generic-optics with 'thingArn' instead." #-}

-- | The name of the thing to be removed from the billing group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgThingName :: Lens.Lens' RemoveThingFromBillingGroup (Core.Maybe Types.ThingName)
rtfbgThingName = Lens.field @"thingName"
{-# DEPRECATED rtfbgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.FromJSON RemoveThingFromBillingGroup where
  toJSON RemoveThingFromBillingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingGroupArn" Core..=) Core.<$> billingGroupArn,
            ("billingGroupName" Core..=) Core.<$> billingGroupName,
            ("thingArn" Core..=) Core.<$> thingArn,
            ("thingName" Core..=) Core.<$> thingName
          ]
      )

instance Core.AWSRequest RemoveThingFromBillingGroup where
  type
    Rs RemoveThingFromBillingGroup =
      RemoveThingFromBillingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/billing-groups/removeThingFromBillingGroup",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveThingFromBillingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveThingFromBillingGroupResponse' smart constructor.
newtype RemoveThingFromBillingGroupResponse = RemoveThingFromBillingGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveThingFromBillingGroupResponse' value with any optional fields omitted.
mkRemoveThingFromBillingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveThingFromBillingGroupResponse
mkRemoveThingFromBillingGroupResponse responseStatus =
  RemoveThingFromBillingGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfbgrrsResponseStatus :: Lens.Lens' RemoveThingFromBillingGroupResponse Core.Int
rtfbgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtfbgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
