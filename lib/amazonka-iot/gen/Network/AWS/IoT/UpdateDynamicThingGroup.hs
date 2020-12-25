{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dynamic thing group.
module Network.AWS.IoT.UpdateDynamicThingGroup
  ( -- * Creating a request
    UpdateDynamicThingGroup (..),
    mkUpdateDynamicThingGroup,

    -- ** Request lenses
    udtgThingGroupName,
    udtgThingGroupProperties,
    udtgExpectedVersion,
    udtgIndexName,
    udtgQueryString,
    udtgQueryVersion,

    -- * Destructuring the response
    UpdateDynamicThingGroupResponse (..),
    mkUpdateDynamicThingGroupResponse,

    -- ** Response lenses
    udtgrrsVersion,
    udtgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDynamicThingGroup' smart constructor.
data UpdateDynamicThingGroup = UpdateDynamicThingGroup'
  { -- | The name of the dynamic thing group to update.
    thingGroupName :: Types.ThingGroupName,
    -- | The dynamic thing group properties to update.
    thingGroupProperties :: Types.ThingGroupProperties,
    -- | The expected version of the dynamic thing group to update.
    expectedVersion :: Core.Maybe Core.Integer,
    -- | The dynamic thing group index to update.
    indexName :: Core.Maybe Types.IndexName,
    -- | The dynamic thing group search query string to update.
    queryString :: Core.Maybe Types.QueryString,
    -- | The dynamic thing group query version to update.
    queryVersion :: Core.Maybe Types.QueryVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDynamicThingGroup' value with any optional fields omitted.
mkUpdateDynamicThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  -- | 'thingGroupProperties'
  Types.ThingGroupProperties ->
  UpdateDynamicThingGroup
mkUpdateDynamicThingGroup thingGroupName thingGroupProperties =
  UpdateDynamicThingGroup'
    { thingGroupName,
      thingGroupProperties,
      expectedVersion = Core.Nothing,
      indexName = Core.Nothing,
      queryString = Core.Nothing,
      queryVersion = Core.Nothing
    }

-- | The name of the dynamic thing group to update.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgThingGroupName :: Lens.Lens' UpdateDynamicThingGroup Types.ThingGroupName
udtgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED udtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The dynamic thing group properties to update.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgThingGroupProperties :: Lens.Lens' UpdateDynamicThingGroup Types.ThingGroupProperties
udtgThingGroupProperties = Lens.field @"thingGroupProperties"
{-# DEPRECATED udtgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

-- | The expected version of the dynamic thing group to update.
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgExpectedVersion :: Lens.Lens' UpdateDynamicThingGroup (Core.Maybe Core.Integer)
udtgExpectedVersion = Lens.field @"expectedVersion"
{-# DEPRECATED udtgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The dynamic thing group index to update.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgIndexName :: Lens.Lens' UpdateDynamicThingGroup (Core.Maybe Types.IndexName)
udtgIndexName = Lens.field @"indexName"
{-# DEPRECATED udtgIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The dynamic thing group search query string to update.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgQueryString :: Lens.Lens' UpdateDynamicThingGroup (Core.Maybe Types.QueryString)
udtgQueryString = Lens.field @"queryString"
{-# DEPRECATED udtgQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group query version to update.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgQueryVersion :: Lens.Lens' UpdateDynamicThingGroup (Core.Maybe Types.QueryVersion)
udtgQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED udtgQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

instance Core.FromJSON UpdateDynamicThingGroup where
  toJSON UpdateDynamicThingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("thingGroupProperties" Core..= thingGroupProperties),
            ("expectedVersion" Core..=) Core.<$> expectedVersion,
            ("indexName" Core..=) Core.<$> indexName,
            ("queryString" Core..=) Core.<$> queryString,
            ("queryVersion" Core..=) Core.<$> queryVersion
          ]
      )

instance Core.AWSRequest UpdateDynamicThingGroup where
  type Rs UpdateDynamicThingGroup = UpdateDynamicThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/dynamic-thing-groups/" Core.<> (Core.toText thingGroupName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDynamicThingGroupResponse'
            Core.<$> (x Core..:? "version") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDynamicThingGroupResponse' smart constructor.
data UpdateDynamicThingGroupResponse = UpdateDynamicThingGroupResponse'
  { -- | The dynamic thing group version.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDynamicThingGroupResponse' value with any optional fields omitted.
mkUpdateDynamicThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDynamicThingGroupResponse
mkUpdateDynamicThingGroupResponse responseStatus =
  UpdateDynamicThingGroupResponse'
    { version = Core.Nothing,
      responseStatus
    }

-- | The dynamic thing group version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgrrsVersion :: Lens.Lens' UpdateDynamicThingGroupResponse (Core.Maybe Core.Integer)
udtgrrsVersion = Lens.field @"version"
{-# DEPRECATED udtgrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udtgrrsResponseStatus :: Lens.Lens' UpdateDynamicThingGroupResponse Core.Int
udtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
