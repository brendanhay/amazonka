{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for a dimension. You cannot change the type of a dimension after it is created (you can delete it and re-create it).
module Network.AWS.IoT.UpdateDimension
    (
    -- * Creating a request
      UpdateDimension (..)
    , mkUpdateDimension
    -- ** Request lenses
    , udName
    , udStringValues

    -- * Destructuring the response
    , UpdateDimensionResponse (..)
    , mkUpdateDimensionResponse
    -- ** Response lenses
    , udrrsArn
    , udrrsCreationDate
    , udrrsLastModifiedDate
    , udrrsName
    , udrrsStringValues
    , udrrsType
    , udrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDimension' smart constructor.
data UpdateDimension = UpdateDimension'
  { name :: Types.Name
    -- ^ A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
  , stringValues :: Core.NonEmpty Types.DimensionStringValue
    -- ^ Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDimension' value with any optional fields omitted.
mkUpdateDimension
    :: Types.Name -- ^ 'name'
    -> Core.NonEmpty Types.DimensionStringValue -- ^ 'stringValues'
    -> UpdateDimension
mkUpdateDimension name stringValues
  = UpdateDimension'{name, stringValues}

-- | A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDimension Types.Name
udName = Lens.field @"name"
{-# INLINEABLE udName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udStringValues :: Lens.Lens' UpdateDimension (Core.NonEmpty Types.DimensionStringValue)
udStringValues = Lens.field @"stringValues"
{-# INLINEABLE udStringValues #-}
{-# DEPRECATED stringValues "Use generic-lens or generic-optics with 'stringValues' instead"  #-}

instance Core.ToQuery UpdateDimension where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDimension where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateDimension where
        toJSON UpdateDimension{..}
          = Core.object
              (Core.catMaybes [Core.Just ("stringValues" Core..= stringValues)])

instance Core.AWSRequest UpdateDimension where
        type Rs UpdateDimension = UpdateDimensionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/dimensions/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDimensionResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "creationDate" Core.<*>
                     x Core..:? "lastModifiedDate"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "stringValues"
                     Core.<*> x Core..:? "type"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDimensionResponse' smart constructor.
data UpdateDimensionResponse = UpdateDimensionResponse'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN (Amazon resource name) of the created dimension.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time, in milliseconds since epoch, when the dimension was initially created.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time, in milliseconds since epoch, when the dimension was most recently updated.
  , name :: Core.Maybe Types.Name
    -- ^ A unique identifier for the dimension.
  , stringValues :: Core.Maybe (Core.NonEmpty Types.DimensionStringValue)
    -- ^ The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
  , type' :: Core.Maybe Types.DimensionType
    -- ^ The type of the dimension.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateDimensionResponse' value with any optional fields omitted.
mkUpdateDimensionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDimensionResponse
mkUpdateDimensionResponse responseStatus
  = UpdateDimensionResponse'{arn = Core.Nothing,
                             creationDate = Core.Nothing, lastModifiedDate = Core.Nothing,
                             name = Core.Nothing, stringValues = Core.Nothing,
                             type' = Core.Nothing, responseStatus}

-- | The ARN (Amazon resource name) of the created dimension.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsArn :: Lens.Lens' UpdateDimensionResponse (Core.Maybe Types.Arn)
udrrsArn = Lens.field @"arn"
{-# INLINEABLE udrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time, in milliseconds since epoch, when the dimension was initially created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsCreationDate :: Lens.Lens' UpdateDimensionResponse (Core.Maybe Core.NominalDiffTime)
udrrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE udrrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date and time, in milliseconds since epoch, when the dimension was most recently updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsLastModifiedDate :: Lens.Lens' UpdateDimensionResponse (Core.Maybe Core.NominalDiffTime)
udrrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE udrrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | A unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsName :: Lens.Lens' UpdateDimensionResponse (Core.Maybe Types.Name)
udrrsName = Lens.field @"name"
{-# INLINEABLE udrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsStringValues :: Lens.Lens' UpdateDimensionResponse (Core.Maybe (Core.NonEmpty Types.DimensionStringValue))
udrrsStringValues = Lens.field @"stringValues"
{-# INLINEABLE udrrsStringValues #-}
{-# DEPRECATED stringValues "Use generic-lens or generic-optics with 'stringValues' instead"  #-}

-- | The type of the dimension.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsType :: Lens.Lens' UpdateDimensionResponse (Core.Maybe Types.DimensionType)
udrrsType = Lens.field @"type'"
{-# INLINEABLE udrrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDimensionResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE udrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
