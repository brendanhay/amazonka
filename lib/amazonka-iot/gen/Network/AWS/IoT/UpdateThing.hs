{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a thing.
module Network.AWS.IoT.UpdateThing
    (
    -- * Creating a request
      UpdateThing (..)
    , mkUpdateThing
    -- ** Request lenses
    , utThingName
    , utAttributePayload
    , utExpectedVersion
    , utRemoveThingType
    , utThingTypeName

    -- * Destructuring the response
    , UpdateThingResponse (..)
    , mkUpdateThingResponse
    -- ** Response lenses
    , utrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the UpdateThing operation.
--
-- /See:/ 'mkUpdateThing' smart constructor.
data UpdateThing = UpdateThing'
  { thingName :: Types.ThingName
    -- ^ The name of the thing to update.
--
-- You can't change a thing's name. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
  , attributePayload :: Core.Maybe Types.AttributePayload
    -- ^ A list of thing attributes, a JSON string containing name-value pairs. For example:
--
-- @{\"attributes\":{\"name1\":\"value2\"}}@ 
-- This data is used to add new attributes or update existing attributes.
  , expectedVersion :: Core.Maybe Core.Integer
    -- ^ The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @UpdateThing@ request is rejected with a @VersionConflictException@ .
  , removeThingType :: Core.Maybe Core.Bool
    -- ^ Remove a thing type association. If __true__ , the association is removed.
  , thingTypeName :: Core.Maybe Types.ThingTypeName
    -- ^ The name of the thing type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThing' value with any optional fields omitted.
mkUpdateThing
    :: Types.ThingName -- ^ 'thingName'
    -> UpdateThing
mkUpdateThing thingName
  = UpdateThing'{thingName, attributePayload = Core.Nothing,
                 expectedVersion = Core.Nothing, removeThingType = Core.Nothing,
                 thingTypeName = Core.Nothing}

-- | The name of the thing to update.
--
-- You can't change a thing's name. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utThingName :: Lens.Lens' UpdateThing Types.ThingName
utThingName = Lens.field @"thingName"
{-# INLINEABLE utThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | A list of thing attributes, a JSON string containing name-value pairs. For example:
--
-- @{\"attributes\":{\"name1\":\"value2\"}}@ 
-- This data is used to add new attributes or update existing attributes.
--
-- /Note:/ Consider using 'attributePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAttributePayload :: Lens.Lens' UpdateThing (Core.Maybe Types.AttributePayload)
utAttributePayload = Lens.field @"attributePayload"
{-# INLINEABLE utAttributePayload #-}
{-# DEPRECATED attributePayload "Use generic-lens or generic-optics with 'attributePayload' instead"  #-}

-- | The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @UpdateThing@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utExpectedVersion :: Lens.Lens' UpdateThing (Core.Maybe Core.Integer)
utExpectedVersion = Lens.field @"expectedVersion"
{-# INLINEABLE utExpectedVersion #-}
{-# DEPRECATED expectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead"  #-}

-- | Remove a thing type association. If __true__ , the association is removed.
--
-- /Note:/ Consider using 'removeThingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utRemoveThingType :: Lens.Lens' UpdateThing (Core.Maybe Core.Bool)
utRemoveThingType = Lens.field @"removeThingType"
{-# INLINEABLE utRemoveThingType #-}
{-# DEPRECATED removeThingType "Use generic-lens or generic-optics with 'removeThingType' instead"  #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utThingTypeName :: Lens.Lens' UpdateThing (Core.Maybe Types.ThingTypeName)
utThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE utThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

instance Core.ToQuery UpdateThing where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateThing where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateThing where
        toJSON UpdateThing{..}
          = Core.object
              (Core.catMaybes
                 [("attributePayload" Core..=) Core.<$> attributePayload,
                  ("expectedVersion" Core..=) Core.<$> expectedVersion,
                  ("removeThingType" Core..=) Core.<$> removeThingType,
                  ("thingTypeName" Core..=) Core.<$> thingTypeName])

instance Core.AWSRequest UpdateThing where
        type Rs UpdateThing = UpdateThingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/things/" Core.<> Core.toText thingName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateThingResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output from the UpdateThing operation.
--
-- /See:/ 'mkUpdateThingResponse' smart constructor.
newtype UpdateThingResponse = UpdateThingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateThingResponse' value with any optional fields omitted.
mkUpdateThingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateThingResponse
mkUpdateThingResponse responseStatus
  = UpdateThingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateThingResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
