{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a thing group.
module Network.AWS.IoT.DeleteThingGroup
  ( -- * Creating a request
    DeleteThingGroup (..),
    mkDeleteThingGroup,

    -- ** Request lenses
    dThingGroupName,
    dExpectedVersion,

    -- * Destructuring the response
    DeleteThingGroupResponse (..),
    mkDeleteThingGroupResponse,

    -- ** Response lenses
    dtgrfrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteThingGroup' smart constructor.
data DeleteThingGroup = DeleteThingGroup'
  { -- | The name of the thing group to delete.
    thingGroupName :: Types.ThingGroupName,
    -- | The expected version of the thing group to delete.
    expectedVersion :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingGroup' value with any optional fields omitted.
mkDeleteThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  DeleteThingGroup
mkDeleteThingGroup thingGroupName =
  DeleteThingGroup' {thingGroupName, expectedVersion = Core.Nothing}

-- | The name of the thing group to delete.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingGroupName :: Lens.Lens' DeleteThingGroup Types.ThingGroupName
dThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED dThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The expected version of the thing group to delete.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExpectedVersion :: Lens.Lens' DeleteThingGroup (Core.Maybe Core.Integer)
dExpectedVersion = Lens.field @"expectedVersion"
{-# DEPRECATED dExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

instance Core.AWSRequest DeleteThingGroup where
  type Rs DeleteThingGroup = DeleteThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/thing-groups/" Core.<> (Core.toText thingGroupName)),
        Core._rqQuery =
          Core.toQueryValue "expectedVersion" Core.<$> expectedVersion,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThingGroupResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteThingGroupResponse' smart constructor.
newtype DeleteThingGroupResponse = DeleteThingGroupResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingGroupResponse' value with any optional fields omitted.
mkDeleteThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteThingGroupResponse
mkDeleteThingGroupResponse responseStatus =
  DeleteThingGroupResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrfrsResponseStatus :: Lens.Lens' DeleteThingGroupResponse Core.Int
dtgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
