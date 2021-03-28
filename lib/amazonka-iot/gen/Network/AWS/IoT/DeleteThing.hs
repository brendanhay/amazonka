{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing. Returns successfully with no error if the deletion is successful or you specify a thing that doesn't exist.
module Network.AWS.IoT.DeleteThing
    (
    -- * Creating a request
      DeleteThing (..)
    , mkDeleteThing
    -- ** Request lenses
    , dtThingName
    , dtExpectedVersion

    -- * Destructuring the response
    , DeleteThingResponse (..)
    , mkDeleteThingResponse
    -- ** Response lenses
    , dtrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThing operation.
--
-- /See:/ 'mkDeleteThing' smart constructor.
data DeleteThing = DeleteThing'
  { thingName :: Types.ThingName
    -- ^ The name of the thing to delete.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @DeleteThing@ request is rejected with a @VersionConflictException@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThing' value with any optional fields omitted.
mkDeleteThing
    :: Types.ThingName -- ^ 'thingName'
    -> DeleteThing
mkDeleteThing thingName
  = DeleteThing'{thingName, expectedVersion = Core.Nothing}

-- | The name of the thing to delete.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtThingName :: Lens.Lens' DeleteThing Types.ThingName
dtThingName = Lens.field @"thingName"
{-# INLINEABLE dtThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @DeleteThing@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtExpectedVersion :: Lens.Lens' DeleteThing (Core.Maybe Core.Integer)
dtExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE dtExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

instance Core.ToQuery DeleteThing where
        toQuery DeleteThing{..}
          = Core.maybe Core.mempty (Core.toQueryPair "expectedVersion")
              expectedVersion

instance Core.ToHeaders DeleteThing where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteThing where
        type Rs DeleteThing = DeleteThingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/things/" Core.<> Core.toText thingName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteThingResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output of the DeleteThing operation.
--
-- /See:/ 'mkDeleteThingResponse' smart constructor.
newtype DeleteThingResponse = DeleteThingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteThingResponse' value with any optional fields omitted.
mkDeleteThingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteThingResponse
mkDeleteThingResponse responseStatus
  = DeleteThingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrfrsResponseStatus :: Lens.Lens' DeleteThingResponse Core.Int
dtrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
