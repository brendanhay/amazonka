{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts processing a stream processor. You create a stream processor by calling 'CreateStreamProcessor' . To tell @StartStreamProcessor@ which stream processor to start, use the value of the @Name@ field specified in the call to @CreateStreamProcessor@ .
module Network.AWS.Rekognition.StartStreamProcessor
    (
    -- * Creating a request
      StartStreamProcessor (..)
    , mkStartStreamProcessor
    -- ** Request lenses
    , sName

    -- * Destructuring the response
    , StartStreamProcessorResponse (..)
    , mkStartStreamProcessorResponse
    -- ** Response lenses
    , ssprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartStreamProcessor' smart constructor.
newtype StartStreamProcessor = StartStreamProcessor'
  { name :: Types.StreamProcessorName
    -- ^ The name of the stream processor to start processing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartStreamProcessor' value with any optional fields omitted.
mkStartStreamProcessor
    :: Types.StreamProcessorName -- ^ 'name'
    -> StartStreamProcessor
mkStartStreamProcessor name = StartStreamProcessor'{name}

-- | The name of the stream processor to start processing.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StartStreamProcessor Types.StreamProcessorName
sName = Lens.field @"name"
{-# INLINEABLE sName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StartStreamProcessor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartStreamProcessor where
        toHeaders StartStreamProcessor{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.StartStreamProcessor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartStreamProcessor where
        toJSON StartStreamProcessor{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartStreamProcessor where
        type Rs StartStreamProcessor = StartStreamProcessorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartStreamProcessorResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartStreamProcessorResponse' smart constructor.
newtype StartStreamProcessorResponse = StartStreamProcessorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartStreamProcessorResponse' value with any optional fields omitted.
mkStartStreamProcessorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartStreamProcessorResponse
mkStartStreamProcessorResponse responseStatus
  = StartStreamProcessorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssprfrsResponseStatus :: Lens.Lens' StartStreamProcessorResponse Core.Int
ssprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ssprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
