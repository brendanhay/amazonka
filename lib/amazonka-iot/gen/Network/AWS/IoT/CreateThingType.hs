{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new thing type.
module Network.AWS.IoT.CreateThingType
    (
    -- * Creating a request
      CreateThingType (..)
    , mkCreateThingType
    -- ** Request lenses
    , cttThingTypeName
    , cttTags
    , cttThingTypeProperties

    -- * Destructuring the response
    , CreateThingTypeResponse (..)
    , mkCreateThingTypeResponse
    -- ** Response lenses
    , cttrrsThingTypeArn
    , cttrrsThingTypeId
    , cttrrsThingTypeName
    , cttrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateThingType operation.
--
-- /See:/ 'mkCreateThingType' smart constructor.
data CreateThingType = CreateThingType'
  { thingTypeName :: Types.ThingTypeName
    -- ^ The name of the thing type.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage the thing type.
  , thingTypeProperties :: Core.Maybe Types.ThingTypeProperties
    -- ^ The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingType' value with any optional fields omitted.
mkCreateThingType
    :: Types.ThingTypeName -- ^ 'thingTypeName'
    -> CreateThingType
mkCreateThingType thingTypeName
  = CreateThingType'{thingTypeName, tags = Core.Nothing,
                     thingTypeProperties = Core.Nothing}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttThingTypeName :: Lens.Lens' CreateThingType Types.ThingTypeName
cttThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE cttThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

-- | Metadata which can be used to manage the thing type.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttTags :: Lens.Lens' CreateThingType (Core.Maybe [Types.Tag])
cttTags = Lens.field @"tags"
{-# INLINEABLE cttTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttThingTypeProperties :: Lens.Lens' CreateThingType (Core.Maybe Types.ThingTypeProperties)
cttThingTypeProperties = Lens.field @"thingTypeProperties"
{-# INLINEABLE cttThingTypeProperties #-}
{-# DEPRECATED thingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead"  #-}

instance Core.ToQuery CreateThingType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateThingType where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateThingType where
        toJSON CreateThingType{..}
          = Core.object
              (Core.catMaybes
                 [("tags" Core..=) Core.<$> tags,
                  ("thingTypeProperties" Core..=) Core.<$> thingTypeProperties])

instance Core.AWSRequest CreateThingType where
        type Rs CreateThingType = CreateThingTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/thing-types/" Core.<> Core.toText thingTypeName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateThingTypeResponse' Core.<$>
                   (x Core..:? "thingTypeArn") Core.<*> x Core..:? "thingTypeId"
                     Core.<*> x Core..:? "thingTypeName"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output of the CreateThingType operation.
--
-- /See:/ 'mkCreateThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { thingTypeArn :: Core.Maybe Types.ThingTypeArn
    -- ^ The Amazon Resource Name (ARN) of the thing type.
  , thingTypeId :: Core.Maybe Types.ThingTypeId
    -- ^ The thing type ID.
  , thingTypeName :: Core.Maybe Types.ThingTypeName
    -- ^ The name of the thing type.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateThingTypeResponse' value with any optional fields omitted.
mkCreateThingTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateThingTypeResponse
mkCreateThingTypeResponse responseStatus
  = CreateThingTypeResponse'{thingTypeArn = Core.Nothing,
                             thingTypeId = Core.Nothing, thingTypeName = Core.Nothing,
                             responseStatus}

-- | The Amazon Resource Name (ARN) of the thing type.
--
-- /Note:/ Consider using 'thingTypeArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsThingTypeArn :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Types.ThingTypeArn)
cttrrsThingTypeArn = Lens.field @"thingTypeArn"
{-# INLINEABLE cttrrsThingTypeArn #-}
{-# DEPRECATED thingTypeArn "Use generic-lens or generic-optics with 'thingTypeArn' instead"  #-}

-- | The thing type ID.
--
-- /Note:/ Consider using 'thingTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsThingTypeId :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Types.ThingTypeId)
cttrrsThingTypeId = Lens.field @"thingTypeId"
{-# INLINEABLE cttrrsThingTypeId #-}
{-# DEPRECATED thingTypeId "Use generic-lens or generic-optics with 'thingTypeId' instead"  #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsThingTypeName :: Lens.Lens' CreateThingTypeResponse (Core.Maybe Types.ThingTypeName)
cttrrsThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE cttrrsThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrrsResponseStatus :: Lens.Lens' CreateThingTypeResponse Core.Int
cttrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cttrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
