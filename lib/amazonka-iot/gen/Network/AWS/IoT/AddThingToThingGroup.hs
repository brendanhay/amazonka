{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AddThingToThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a thing to a thing group.
module Network.AWS.IoT.AddThingToThingGroup
  ( -- * Creating a request
    AddThingToThingGroup (..),
    mkAddThingToThingGroup,

    -- ** Request lenses
    atttgOverrideDynamicGroups,
    atttgThingArn,
    atttgThingGroupArn,
    atttgThingGroupName,
    atttgThingName,

    -- * Destructuring the response
    AddThingToThingGroupResponse (..),
    mkAddThingToThingGroupResponse,

    -- ** Response lenses
    atttgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddThingToThingGroup' smart constructor.
data AddThingToThingGroup = AddThingToThingGroup'
  { -- | Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
    overrideDynamicGroups :: Core.Maybe Core.Bool,
    -- | The ARN of the thing to add to a group.
    thingArn :: Core.Maybe Types.ThingArn,
    -- | The ARN of the group to which you are adding a thing.
    thingGroupArn :: Core.Maybe Types.ThingGroupArn,
    -- | The name of the group to which you are adding a thing.
    thingGroupName :: Core.Maybe Types.ThingGroupName,
    -- | The name of the thing to add to a group.
    thingName :: Core.Maybe Types.ThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddThingToThingGroup' value with any optional fields omitted.
mkAddThingToThingGroup ::
  AddThingToThingGroup
mkAddThingToThingGroup =
  AddThingToThingGroup'
    { overrideDynamicGroups = Core.Nothing,
      thingArn = Core.Nothing,
      thingGroupArn = Core.Nothing,
      thingGroupName = Core.Nothing,
      thingName = Core.Nothing
    }

-- | Override dynamic thing groups with static thing groups when 10-group limit is reached. If a thing belongs to 10 thing groups, and one or more of those groups are dynamic thing groups, adding a thing to a static group removes the thing from the last dynamic group.
--
-- /Note:/ Consider using 'overrideDynamicGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgOverrideDynamicGroups :: Lens.Lens' AddThingToThingGroup (Core.Maybe Core.Bool)
atttgOverrideDynamicGroups = Lens.field @"overrideDynamicGroups"
{-# DEPRECATED atttgOverrideDynamicGroups "Use generic-lens or generic-optics with 'overrideDynamicGroups' instead." #-}

-- | The ARN of the thing to add to a group.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingArn :: Lens.Lens' AddThingToThingGroup (Core.Maybe Types.ThingArn)
atttgThingArn = Lens.field @"thingArn"
{-# DEPRECATED atttgThingArn "Use generic-lens or generic-optics with 'thingArn' instead." #-}

-- | The ARN of the group to which you are adding a thing.
--
-- /Note:/ Consider using 'thingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingGroupArn :: Lens.Lens' AddThingToThingGroup (Core.Maybe Types.ThingGroupArn)
atttgThingGroupArn = Lens.field @"thingGroupArn"
{-# DEPRECATED atttgThingGroupArn "Use generic-lens or generic-optics with 'thingGroupArn' instead." #-}

-- | The name of the group to which you are adding a thing.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingGroupName :: Lens.Lens' AddThingToThingGroup (Core.Maybe Types.ThingGroupName)
atttgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED atttgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The name of the thing to add to a group.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgThingName :: Lens.Lens' AddThingToThingGroup (Core.Maybe Types.ThingName)
atttgThingName = Lens.field @"thingName"
{-# DEPRECATED atttgThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.FromJSON AddThingToThingGroup where
  toJSON AddThingToThingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("overrideDynamicGroups" Core..=) Core.<$> overrideDynamicGroups,
            ("thingArn" Core..=) Core.<$> thingArn,
            ("thingGroupArn" Core..=) Core.<$> thingGroupArn,
            ("thingGroupName" Core..=) Core.<$> thingGroupName,
            ("thingName" Core..=) Core.<$> thingName
          ]
      )

instance Core.AWSRequest AddThingToThingGroup where
  type Rs AddThingToThingGroup = AddThingToThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath "/thing-groups/addThingToThingGroup",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddThingToThingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddThingToThingGroupResponse' smart constructor.
newtype AddThingToThingGroupResponse = AddThingToThingGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddThingToThingGroupResponse' value with any optional fields omitted.
mkAddThingToThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddThingToThingGroupResponse
mkAddThingToThingGroupResponse responseStatus =
  AddThingToThingGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atttgrrsResponseStatus :: Lens.Lens' AddThingToThingGroupResponse Core.Int
atttgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED atttgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
