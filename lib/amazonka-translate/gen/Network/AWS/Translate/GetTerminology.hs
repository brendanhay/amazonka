{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.GetTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom terminology.
module Network.AWS.Translate.GetTerminology
    (
    -- * Creating a request
      GetTerminology (..)
    , mkGetTerminology
    -- ** Request lenses
    , gtName
    , gtTerminologyDataFormat

    -- * Destructuring the response
    , GetTerminologyResponse (..)
    , mkGetTerminologyResponse
    -- ** Response lenses
    , gtrrsTerminologyDataLocation
    , gtrrsTerminologyProperties
    , gtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkGetTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { name :: Types.Name
    -- ^ The name of the custom terminology being retrieved.
  , terminologyDataFormat :: Types.TerminologyDataFormat
    -- ^ The data format of the custom terminology being retrieved, either CSV or TMX.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTerminology' value with any optional fields omitted.
mkGetTerminology
    :: Types.Name -- ^ 'name'
    -> Types.TerminologyDataFormat -- ^ 'terminologyDataFormat'
    -> GetTerminology
mkGetTerminology name terminologyDataFormat
  = GetTerminology'{name, terminologyDataFormat}

-- | The name of the custom terminology being retrieved.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTerminology Types.Name
gtName = Lens.field @"name"
{-# INLINEABLE gtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The data format of the custom terminology being retrieved, either CSV or TMX.
--
-- /Note:/ Consider using 'terminologyDataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTerminologyDataFormat :: Lens.Lens' GetTerminology Types.TerminologyDataFormat
gtTerminologyDataFormat = Lens.field @"terminologyDataFormat"
{-# INLINEABLE gtTerminologyDataFormat #-}
{-# DEPRECATED terminologyDataFormat "Use generic-lens or generic-optics with 'terminologyDataFormat' instead"  #-}

instance Core.ToQuery GetTerminology where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTerminology where
        toHeaders GetTerminology{..}
          = Core.pure
              ("X-Amz-Target", "AWSShineFrontendService_20170701.GetTerminology")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTerminology where
        toJSON GetTerminology{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("TerminologyDataFormat" Core..= terminologyDataFormat)])

instance Core.AWSRequest GetTerminology where
        type Rs GetTerminology = GetTerminologyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTerminologyResponse' Core.<$>
                   (x Core..:? "TerminologyDataLocation") Core.<*>
                     x Core..:? "TerminologyProperties"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { terminologyDataLocation :: Core.Maybe Types.TerminologyDataLocation
    -- ^ The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
  , terminologyProperties :: Core.Maybe Types.TerminologyProperties
    -- ^ The properties of the custom terminology being retrieved.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTerminologyResponse' value with any optional fields omitted.
mkGetTerminologyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTerminologyResponse
mkGetTerminologyResponse responseStatus
  = GetTerminologyResponse'{terminologyDataLocation = Core.Nothing,
                            terminologyProperties = Core.Nothing, responseStatus}

-- | The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
--
-- /Note:/ Consider using 'terminologyDataLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTerminologyDataLocation :: Lens.Lens' GetTerminologyResponse (Core.Maybe Types.TerminologyDataLocation)
gtrrsTerminologyDataLocation = Lens.field @"terminologyDataLocation"
{-# INLINEABLE gtrrsTerminologyDataLocation #-}
{-# DEPRECATED terminologyDataLocation "Use generic-lens or generic-optics with 'terminologyDataLocation' instead"  #-}

-- | The properties of the custom terminology being retrieved.
--
-- /Note:/ Consider using 'terminologyProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTerminologyProperties :: Lens.Lens' GetTerminologyResponse (Core.Maybe Types.TerminologyProperties)
gtrrsTerminologyProperties = Lens.field @"terminologyProperties"
{-# INLINEABLE gtrrsTerminologyProperties #-}
{-# DEPRECATED terminologyProperties "Use generic-lens or generic-optics with 'terminologyProperties' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTerminologyResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
