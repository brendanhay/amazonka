{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeprecateThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates a thing type. You can not associate new things with deprecated thing type.
module Network.AWS.IoT.DeprecateThingType
    (
    -- * Creating a request
      DeprecateThingType (..)
    , mkDeprecateThingType
    -- ** Request lenses
    , dttfThingTypeName
    , dttfUndoDeprecate

    -- * Destructuring the response
    , DeprecateThingTypeResponse (..)
    , mkDeprecateThingTypeResponse
    -- ** Response lenses
    , dttrgrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeprecateThingType operation.
--
-- /See:/ 'mkDeprecateThingType' smart constructor.
data DeprecateThingType = DeprecateThingType'
  { thingTypeName :: Types.ThingTypeName
    -- ^ The name of the thing type to deprecate.
  , undoDeprecate :: Core.Maybe Core.Bool
    -- ^ Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeprecateThingType' value with any optional fields omitted.
mkDeprecateThingType
    :: Types.ThingTypeName -- ^ 'thingTypeName'
    -> DeprecateThingType
mkDeprecateThingType thingTypeName
  = DeprecateThingType'{thingTypeName, undoDeprecate = Core.Nothing}

-- | The name of the thing type to deprecate.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfThingTypeName :: Lens.Lens' DeprecateThingType Types.ThingTypeName
dttfThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE dttfThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

-- | Whether to undeprecate a deprecated thing type. If __true__ , the thing type will not be deprecated anymore and you can associate it with things.
--
-- /Note:/ Consider using 'undoDeprecate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfUndoDeprecate :: Lens.Lens' DeprecateThingType (Core.Maybe Core.Bool)
dttfUndoDeprecate = Lens.field @"undoDeprecate"
{-# INLINEABLE dttfUndoDeprecate #-}
{-# DEPRECATED undoDeprecate "Use generic-lens or generic-optics with 'undoDeprecate' instead"  #-}

instance Core.ToQuery DeprecateThingType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeprecateThingType where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON DeprecateThingType where
        toJSON DeprecateThingType{..}
          = Core.object
              (Core.catMaybes [("undoDeprecate" Core..=) Core.<$> undoDeprecate])

instance Core.AWSRequest DeprecateThingType where
        type Rs DeprecateThingType = DeprecateThingTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/thing-types/" Core.<> Core.toText thingTypeName Core.<>
                             "/deprecate",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeprecateThingTypeResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output for the DeprecateThingType operation.
--
-- /See:/ 'mkDeprecateThingTypeResponse' smart constructor.
newtype DeprecateThingTypeResponse = DeprecateThingTypeResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeprecateThingTypeResponse' value with any optional fields omitted.
mkDeprecateThingTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeprecateThingTypeResponse
mkDeprecateThingTypeResponse responseStatus
  = DeprecateThingTypeResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrgrsResponseStatus :: Lens.Lens' DeprecateThingTypeResponse Core.Int
dttrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dttrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
