{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a thing group.
module Network.AWS.IoT.CreateThingGroup
  ( -- * Creating a request
    CreateThingGroup (..),
    mkCreateThingGroup,

    -- ** Request lenses
    ctgThingGroupName,
    ctgParentGroupName,
    ctgTags,
    ctgThingGroupProperties,

    -- * Destructuring the response
    CreateThingGroupResponse (..),
    mkCreateThingGroupResponse,

    -- ** Response lenses
    ctgrrsThingGroupArn,
    ctgrrsThingGroupId,
    ctgrrsThingGroupName,
    ctgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateThingGroup' smart constructor.
data CreateThingGroup = CreateThingGroup'
  { -- | The thing group name to create.
    thingGroupName :: Types.ThingGroupName,
    -- | The name of the parent thing group.
    parentGroupName :: Core.Maybe Types.ParentGroupName,
    -- | Metadata which can be used to manage the thing group.
    tags :: Core.Maybe [Types.Tag],
    -- | The thing group properties.
    thingGroupProperties :: Core.Maybe Types.ThingGroupProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingGroup' value with any optional fields omitted.
mkCreateThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  CreateThingGroup
mkCreateThingGroup thingGroupName =
  CreateThingGroup'
    { thingGroupName,
      parentGroupName = Core.Nothing,
      tags = Core.Nothing,
      thingGroupProperties = Core.Nothing
    }

-- | The thing group name to create.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgThingGroupName :: Lens.Lens' CreateThingGroup Types.ThingGroupName
ctgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED ctgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The name of the parent thing group.
--
-- /Note:/ Consider using 'parentGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgParentGroupName :: Lens.Lens' CreateThingGroup (Core.Maybe Types.ParentGroupName)
ctgParentGroupName = Lens.field @"parentGroupName"
{-# DEPRECATED ctgParentGroupName "Use generic-lens or generic-optics with 'parentGroupName' instead." #-}

-- | Metadata which can be used to manage the thing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTags :: Lens.Lens' CreateThingGroup (Core.Maybe [Types.Tag])
ctgTags = Lens.field @"tags"
{-# DEPRECATED ctgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgThingGroupProperties :: Lens.Lens' CreateThingGroup (Core.Maybe Types.ThingGroupProperties)
ctgThingGroupProperties = Lens.field @"thingGroupProperties"
{-# DEPRECATED ctgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

instance Core.FromJSON CreateThingGroup where
  toJSON CreateThingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ ("parentGroupName" Core..=) Core.<$> parentGroupName,
            ("tags" Core..=) Core.<$> tags,
            ("thingGroupProperties" Core..=) Core.<$> thingGroupProperties
          ]
      )

instance Core.AWSRequest CreateThingGroup where
  type Rs CreateThingGroup = CreateThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/thing-groups/" Core.<> (Core.toText thingGroupName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThingGroupResponse'
            Core.<$> (x Core..:? "thingGroupArn")
            Core.<*> (x Core..:? "thingGroupId")
            Core.<*> (x Core..:? "thingGroupName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateThingGroupResponse' smart constructor.
data CreateThingGroupResponse = CreateThingGroupResponse'
  { -- | The thing group ARN.
    thingGroupArn :: Core.Maybe Types.ThingGroupArn,
    -- | The thing group ID.
    thingGroupId :: Core.Maybe Types.ThingGroupId,
    -- | The thing group name.
    thingGroupName :: Core.Maybe Types.ThingGroupName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingGroupResponse' value with any optional fields omitted.
mkCreateThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateThingGroupResponse
mkCreateThingGroupResponse responseStatus =
  CreateThingGroupResponse'
    { thingGroupArn = Core.Nothing,
      thingGroupId = Core.Nothing,
      thingGroupName = Core.Nothing,
      responseStatus
    }

-- | The thing group ARN.
--
-- /Note:/ Consider using 'thingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsThingGroupArn :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Types.ThingGroupArn)
ctgrrsThingGroupArn = Lens.field @"thingGroupArn"
{-# DEPRECATED ctgrrsThingGroupArn "Use generic-lens or generic-optics with 'thingGroupArn' instead." #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsThingGroupId :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Types.ThingGroupId)
ctgrrsThingGroupId = Lens.field @"thingGroupId"
{-# DEPRECATED ctgrrsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsThingGroupName :: Lens.Lens' CreateThingGroupResponse (Core.Maybe Types.ThingGroupName)
ctgrrsThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED ctgrrsThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsResponseStatus :: Lens.Lens' CreateThingGroupResponse Core.Int
ctgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
