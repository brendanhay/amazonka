{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing type.
module Network.AWS.IoT.DescribeThingType
    (
    -- * Creating a request
      DescribeThingType (..)
    , mkDescribeThingType
    -- ** Request lenses
    , dThingTypeName

    -- * Destructuring the response
    , DescribeThingTypeResponse (..)
    , mkDescribeThingTypeResponse
    -- ** Response lenses
    , dttrfrsThingTypeArn
    , dttrfrsThingTypeId
    , dttrfrsThingTypeMetadata
    , dttrfrsThingTypeName
    , dttrfrsThingTypeProperties
    , dttrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeThingType operation.
--
-- /See:/ 'mkDescribeThingType' smart constructor.
newtype DescribeThingType = DescribeThingType'
  { thingTypeName :: Types.ThingTypeName
    -- ^ The name of the thing type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeThingType' value with any optional fields omitted.
mkDescribeThingType
    :: Types.ThingTypeName -- ^ 'thingTypeName'
    -> DescribeThingType
mkDescribeThingType thingTypeName
  = DescribeThingType'{thingTypeName}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingTypeName :: Lens.Lens' DescribeThingType Types.ThingTypeName
dThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE dThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

instance Core.ToQuery DescribeThingType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeThingType where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeThingType where
        type Rs DescribeThingType = DescribeThingTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/thing-types/" Core.<> Core.toText thingTypeName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeThingTypeResponse' Core.<$>
                   (x Core..:? "thingTypeArn") Core.<*> x Core..:? "thingTypeId"
                     Core.<*> x Core..:? "thingTypeMetadata"
                     Core.<*> x Core..:? "thingTypeName"
                     Core.<*> x Core..:? "thingTypeProperties"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the DescribeThingType operation.
--
-- /See:/ 'mkDescribeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'
  { thingTypeArn :: Core.Maybe Types.ThingTypeArn
    -- ^ The thing type ARN.
  , thingTypeId :: Core.Maybe Types.ThingTypeId
    -- ^ The thing type ID.
  , thingTypeMetadata :: Core.Maybe Types.ThingTypeMetadata
    -- ^ The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
  , thingTypeName :: Core.Maybe Types.ThingTypeName
    -- ^ The name of the thing type.
  , thingTypeProperties :: Core.Maybe Types.ThingTypeProperties
    -- ^ The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeThingTypeResponse' value with any optional fields omitted.
mkDescribeThingTypeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeThingTypeResponse
mkDescribeThingTypeResponse responseStatus
  = DescribeThingTypeResponse'{thingTypeArn = Core.Nothing,
                               thingTypeId = Core.Nothing, thingTypeMetadata = Core.Nothing,
                               thingTypeName = Core.Nothing, thingTypeProperties = Core.Nothing,
                               responseStatus}

-- | The thing type ARN.
--
-- /Note:/ Consider using 'thingTypeArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrfrsThingTypeArn :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Types.ThingTypeArn)
dttrfrsThingTypeArn = Lens.field @"thingTypeArn"
{-# INLINEABLE dttrfrsThingTypeArn #-}
{-# DEPRECATED thingTypeArn "Use generic-lens or generic-optics with 'thingTypeArn' instead"  #-}

-- | The thing type ID.
--
-- /Note:/ Consider using 'thingTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrfrsThingTypeId :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Types.ThingTypeId)
dttrfrsThingTypeId = Lens.field @"thingTypeId"
{-# INLINEABLE dttrfrsThingTypeId #-}
{-# DEPRECATED thingTypeId "Use generic-lens or generic-optics with 'thingTypeId' instead"  #-}

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- /Note:/ Consider using 'thingTypeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrfrsThingTypeMetadata :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Types.ThingTypeMetadata)
dttrfrsThingTypeMetadata = Lens.field @"thingTypeMetadata"
{-# INLINEABLE dttrfrsThingTypeMetadata #-}
{-# DEPRECATED thingTypeMetadata "Use generic-lens or generic-optics with 'thingTypeMetadata' instead"  #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrfrsThingTypeName :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Types.ThingTypeName)
dttrfrsThingTypeName = Lens.field @"thingTypeName"
{-# INLINEABLE dttrfrsThingTypeName #-}
{-# DEPRECATED thingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead"  #-}

-- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrfrsThingTypeProperties :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Types.ThingTypeProperties)
dttrfrsThingTypeProperties = Lens.field @"thingTypeProperties"
{-# INLINEABLE dttrfrsThingTypeProperties #-}
{-# DEPRECATED thingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrfrsResponseStatus :: Lens.Lens' DescribeThingTypeResponse Core.Int
dttrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dttrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
