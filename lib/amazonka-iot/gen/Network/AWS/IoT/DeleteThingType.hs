{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteThingType (..)
    , mkDeleteThingType
    -- ** Request lenses
    , dttThingTypeName

    -- * Destructuring the response
    , DeleteThingTypeResponse (..)
    , mkDeleteThingTypeResponse
    -- ** Response lenses
    , dttrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThingType operation.
--
-- /See:/ 'mkDeleteThingType' smart constructor.
newtype DeleteThingType = DeleteThingType'
  { thingTypeName :: Types.ThingTypeName
    -- ^ The name of the thing type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingType' value with any optional fields omitted.
mkDeleteThingType
    :: Types.ThingTypeName -- ^ 'thingTypeName'
    -> DeleteThingType
mkDeleteThingType thingTypeName = DeleteThingType'{thingTypeName}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttThingTypeName :: Lens.Lens' DeleteThingType Types.ThingTypeName
dttThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE dttThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

instance Core.ToQuery DeleteThingType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteThingType where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteThingType where
        type Rs DeleteThingType = DeleteThingTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/thing-types/" Core.<> Core.toText thingTypeName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteThingTypeResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output for the DeleteThingType operation.
--
-- /See:/ 'mkDeleteThingTypeResponse' smart constructor.
newtype DeleteThingTypeResponse = DeleteThingTypeResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingTypeResponse' value with any optional fields omitted.
mkDeleteThingTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteThingTypeResponse
mkDeleteThingTypeResponse responseStatus
  = DeleteThingTypeResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrrsResponseStatus :: Lens.Lens' DeleteThingTypeResponse Core.Int
dttrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dttrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
