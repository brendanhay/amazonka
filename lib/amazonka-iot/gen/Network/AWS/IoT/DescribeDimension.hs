{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a dimension that is defined in your AWS account.
module Network.AWS.IoT.DescribeDimension
    (
    -- * Creating a request
      DescribeDimension (..)
    , mkDescribeDimension
    -- ** Request lenses
    , ddName

    -- * Destructuring the response
    , DescribeDimensionResponse (..)
    , mkDescribeDimensionResponse
    -- ** Response lenses
    , ddrfrsArn
    , ddrfrsCreationDate
    , ddrfrsLastModifiedDate
    , ddrfrsName
    , ddrfrsStringValues
    , ddrfrsType
    , ddrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDimension' smart constructor.
newtype DescribeDimension = DescribeDimension'
  { name :: Types.Name
    -- ^ The unique identifier for the dimension.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDimension' value with any optional fields omitted.
mkDescribeDimension
    :: Types.Name -- ^ 'name'
    -> DescribeDimension
mkDescribeDimension name = DescribeDimension'{name}

-- | The unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DescribeDimension Types.Name
ddName = Lens.field @"name"
{-# INLINEABLE ddName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DescribeDimension where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDimension where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeDimension where
        type Rs DescribeDimension = DescribeDimensionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/dimensions/" Core.<> Core.toText name,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDimensionResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "creationDate" Core.<*>
                     x Core..:? "lastModifiedDate"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "stringValues"
                     Core.<*> x Core..:? "type"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeDimensionResponse' smart constructor.
data DescribeDimensionResponse = DescribeDimensionResponse'
  { arn :: Core.Maybe Types.DimensionArn
    -- ^ The ARN (Amazon resource name) for the dimension.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the dimension was created.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the dimension was last modified.
  , name :: Core.Maybe Types.DimensionName
    -- ^ The unique identifier for the dimension.
  , stringValues :: Core.Maybe (Core.NonEmpty Types.DimensionStringValue)
    -- ^ The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
  , type' :: Core.Maybe Types.DimensionType
    -- ^ The type of the dimension.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeDimensionResponse' value with any optional fields omitted.
mkDescribeDimensionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDimensionResponse
mkDescribeDimensionResponse responseStatus
  = DescribeDimensionResponse'{arn = Core.Nothing,
                               creationDate = Core.Nothing, lastModifiedDate = Core.Nothing,
                               name = Core.Nothing, stringValues = Core.Nothing,
                               type' = Core.Nothing, responseStatus}

-- | The ARN (Amazon resource name) for the dimension.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsArn :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Types.DimensionArn)
ddrfrsArn = Lens.field @"arn"
{-# INLINEABLE ddrfrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date the dimension was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsCreationDate :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Core.NominalDiffTime)
ddrfrsCreationDate = Lens.field @"creationDate"
{-# INLINEABLE ddrfrsCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The date the dimension was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsLastModifiedDate :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Core.NominalDiffTime)
ddrfrsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE ddrfrsLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsName :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Types.DimensionName)
ddrfrsName = Lens.field @"name"
{-# INLINEABLE ddrfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsStringValues :: Lens.Lens' DescribeDimensionResponse (Core.Maybe (Core.NonEmpty Types.DimensionStringValue))
ddrfrsStringValues = Lens.field @"stringValues"
{-# INLINEABLE ddrfrsStringValues #-}
{-# DEPRECATED stringValues "Use generic-lens or generic-optics with 'stringValues' instead"  #-}

-- | The type of the dimension.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsType :: Lens.Lens' DescribeDimensionResponse (Core.Maybe Types.DimensionType)
ddrfrsType = Lens.field @"type'"
{-# INLINEABLE ddrfrsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DescribeDimensionResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
