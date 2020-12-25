{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AddThingToBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a billing group.
module Network.AWS.IoT.AddThingToBillingGroup
  ( -- * Creating a request
    AddThingToBillingGroup (..),
    mkAddThingToBillingGroup,

    -- ** Request lenses
    attbgBillingGroupArn,
    attbgBillingGroupName,
    attbgThingArn,
    attbgThingName,

    -- * Destructuring the response
    AddThingToBillingGroupResponse (..),
    mkAddThingToBillingGroupResponse,

    -- ** Response lenses
    attbgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddThingToBillingGroup' smart constructor.
data AddThingToBillingGroup = AddThingToBillingGroup'
  { -- | The ARN of the billing group.
    billingGroupArn :: Core.Maybe Types.BillingGroupArn,
    -- | The name of the billing group.
    billingGroupName :: Core.Maybe Types.BillingGroupName,
    -- | The ARN of the thing to be added to the billing group.
    thingArn :: Core.Maybe Types.ThingArn,
    -- | The name of the thing to be added to the billing group.
    thingName :: Core.Maybe Types.ThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddThingToBillingGroup' value with any optional fields omitted.
mkAddThingToBillingGroup ::
  AddThingToBillingGroup
mkAddThingToBillingGroup =
  AddThingToBillingGroup'
    { billingGroupArn = Core.Nothing,
      billingGroupName = Core.Nothing,
      thingArn = Core.Nothing,
      thingName = Core.Nothing
    }

-- | The ARN of the billing group.
--
-- /Note:/ Consider using 'billingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgBillingGroupArn :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Types.BillingGroupArn)
attbgBillingGroupArn = Lens.field @"billingGroupArn"
{-# DEPRECATED attbgBillingGroupArn "Use generic-lens or generic-optics with 'billingGroupArn' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgBillingGroupName :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Types.BillingGroupName)
attbgBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED attbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The ARN of the thing to be added to the billing group.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgThingArn :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Types.ThingArn)
attbgThingArn = Lens.field @"thingArn"
{-# DEPRECATED attbgThingArn "Use generic-lens or generic-optics with 'thingArn' instead." #-}

-- | The name of the thing to be added to the billing group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgThingName :: Lens.Lens' AddThingToBillingGroup (Core.Maybe Types.ThingName)
attbgThingName = Lens.field @"thingName"
{-# DEPRECATED attbgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.FromJSON AddThingToBillingGroup where
  toJSON AddThingToBillingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("billingGroupArn" Core..=) Core.<$> billingGroupArn,
            ("billingGroupName" Core..=) Core.<$> billingGroupName,
            ("thingArn" Core..=) Core.<$> thingArn,
            ("thingName" Core..=) Core.<$> thingName
          ]
      )

instance Core.AWSRequest AddThingToBillingGroup where
  type Rs AddThingToBillingGroup = AddThingToBillingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/billing-groups/addThingToBillingGroup",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToBillingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddThingToBillingGroupResponse' smart constructor.
newtype AddThingToBillingGroupResponse = AddThingToBillingGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddThingToBillingGroupResponse' value with any optional fields omitted.
mkAddThingToBillingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddThingToBillingGroupResponse
mkAddThingToBillingGroupResponse responseStatus =
  AddThingToBillingGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attbgrrsResponseStatus :: Lens.Lens' AddThingToBillingGroupResponse Core.Int
attbgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED attbgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
