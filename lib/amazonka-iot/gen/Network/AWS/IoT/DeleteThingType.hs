{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing type. You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling 'DeprecateThingType' , then remove any associated things by calling 'UpdateThing' to change the thing type on any associated thing, and finally use 'DeleteThingType' to delete the thing type.
module Network.AWS.IoT.DeleteThingType
  ( -- * Creating a request
    DeleteThingType (..),
    mkDeleteThingType,

    -- ** Request lenses
    dttThingTypeName,

    -- * Destructuring the response
    DeleteThingTypeResponse (..),
    mkDeleteThingTypeResponse,

    -- ** Response lenses
    dttrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThingType operation.
--
-- /See:/ 'mkDeleteThingType' smart constructor.
newtype DeleteThingType = DeleteThingType'
  { -- | The name of the thing type.
    thingTypeName :: Types.ThingTypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingType' value with any optional fields omitted.
mkDeleteThingType ::
  -- | 'thingTypeName'
  Types.ThingTypeName ->
  DeleteThingType
mkDeleteThingType thingTypeName = DeleteThingType' {thingTypeName}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttThingTypeName :: Lens.Lens' DeleteThingType Types.ThingTypeName
dttThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED dttThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Core.AWSRequest DeleteThingType where
  type Rs DeleteThingType = DeleteThingTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/thing-types/" Core.<> (Core.toText thingTypeName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThingTypeResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The output for the DeleteThingType operation.
--
-- /See:/ 'mkDeleteThingTypeResponse' smart constructor.
newtype DeleteThingTypeResponse = DeleteThingTypeResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingTypeResponse' value with any optional fields omitted.
mkDeleteThingTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteThingTypeResponse
mkDeleteThingTypeResponse responseStatus =
  DeleteThingTypeResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrrsResponseStatus :: Lens.Lens' DeleteThingTypeResponse Core.Int
dttrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dttrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
