{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StopStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running stream processor that was created by 'CreateStreamProcessor' .
module Network.AWS.Rekognition.StopStreamProcessor
    (
    -- * Creating a request
      StopStreamProcessor (..)
    , mkStopStreamProcessor
    -- ** Request lenses
    , sspName

    -- * Destructuring the response
    , StopStreamProcessorResponse (..)
    , mkStopStreamProcessorResponse
    -- ** Response lenses
    , ssprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopStreamProcessor' smart constructor.
newtype StopStreamProcessor = StopStreamProcessor'
  { name :: Types.StreamProcessorName
    -- ^ The name of a stream processor created by 'CreateStreamProcessor' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopStreamProcessor' value with any optional fields omitted.
mkStopStreamProcessor
    :: Types.StreamProcessorName -- ^ 'name'
    -> StopStreamProcessor
mkStopStreamProcessor name = StopStreamProcessor'{name}

-- | The name of a stream processor created by 'CreateStreamProcessor' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspName :: Lens.Lens' StopStreamProcessor Types.StreamProcessorName
sspName = Lens.field @"name"
{-# INLINEABLE sspName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StopStreamProcessor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopStreamProcessor where
        toHeaders StopStreamProcessor{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.StopStreamProcessor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopStreamProcessor where
        toJSON StopStreamProcessor{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopStreamProcessor where
        type Rs StopStreamProcessor = StopStreamProcessorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopStreamProcessorResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopStreamProcessorResponse' smart constructor.
newtype StopStreamProcessorResponse = StopStreamProcessorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopStreamProcessorResponse' value with any optional fields omitted.
mkStopStreamProcessorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopStreamProcessorResponse
mkStopStreamProcessorResponse responseStatus
  = StopStreamProcessorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssprrsResponseStatus :: Lens.Lens' StopStreamProcessorResponse Core.Int
ssprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
