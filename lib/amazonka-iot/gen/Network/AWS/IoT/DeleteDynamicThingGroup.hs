{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dynamic thing group.
module Network.AWS.IoT.DeleteDynamicThingGroup
  ( -- * Creating a request
    DeleteDynamicThingGroup (..),
    mkDeleteDynamicThingGroup,

    -- ** Request lenses
    ddtgThingGroupName,
    ddtgExpectedVersion,

    -- * Destructuring the response
    DeleteDynamicThingGroupResponse (..),
    mkDeleteDynamicThingGroupResponse,

    -- ** Response lenses
    ddtgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDynamicThingGroup' smart constructor.
data DeleteDynamicThingGroup = DeleteDynamicThingGroup'
  { -- | The name of the dynamic thing group to delete.
    thingGroupName :: Types.ThingGroupName,
    -- | The expected version of the dynamic thing group to delete.
    expectedVersion :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDynamicThingGroup' value with any optional fields omitted.
mkDeleteDynamicThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  DeleteDynamicThingGroup
mkDeleteDynamicThingGroup thingGroupName =
  DeleteDynamicThingGroup'
    { thingGroupName,
      expectedVersion = Core.Nothing
    }

-- | The name of the dynamic thing group to delete.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgThingGroupName :: Lens.Lens' DeleteDynamicThingGroup Types.ThingGroupName
ddtgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED ddtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The expected version of the dynamic thing group to delete.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgExpectedVersion :: Lens.Lens' DeleteDynamicThingGroup (Core.Maybe Core.Integer)
ddtgExpectedVersion = Lens.field @"expectedVersion"
{-# DEPRECATED ddtgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

instance Core.AWSRequest DeleteDynamicThingGroup where
  type Rs DeleteDynamicThingGroup = DeleteDynamicThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/dynamic-thing-groups/" Core.<> (Core.toText thingGroupName)),
        Core._rqQuery =
          Core.toQueryValue "expectedVersion" Core.<$> expectedVersion,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDynamicThingGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDynamicThingGroupResponse' smart constructor.
newtype DeleteDynamicThingGroupResponse = DeleteDynamicThingGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDynamicThingGroupResponse' value with any optional fields omitted.
mkDeleteDynamicThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDynamicThingGroupResponse
mkDeleteDynamicThingGroupResponse responseStatus =
  DeleteDynamicThingGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtgrrsResponseStatus :: Lens.Lens' DeleteDynamicThingGroupResponse Core.Int
ddtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
