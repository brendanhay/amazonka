{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream processor identified by @Name@ . You assign the value for @Name@ when you create the stream processor with 'CreateStreamProcessor' . You might not be able to use the same name for a stream processor for a few seconds after calling @DeleteStreamProcessor@ .
module Network.AWS.Rekognition.DeleteStreamProcessor
    (
    -- * Creating a request
      DeleteStreamProcessor (..)
    , mkDeleteStreamProcessor
    -- ** Request lenses
    , dName

    -- * Destructuring the response
    , DeleteStreamProcessorResponse (..)
    , mkDeleteStreamProcessorResponse
    -- ** Response lenses
    , dsprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStreamProcessor' smart constructor.
newtype DeleteStreamProcessor = DeleteStreamProcessor'
  { name :: Types.StreamProcessorName
    -- ^ The name of the stream processor you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamProcessor' value with any optional fields omitted.
mkDeleteStreamProcessor
    :: Types.StreamProcessorName -- ^ 'name'
    -> DeleteStreamProcessor
mkDeleteStreamProcessor name = DeleteStreamProcessor'{name}

-- | The name of the stream processor you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteStreamProcessor Types.StreamProcessorName
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteStreamProcessor where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteStreamProcessor where
        toHeaders DeleteStreamProcessor{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.DeleteStreamProcessor")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteStreamProcessor where
        toJSON DeleteStreamProcessor{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteStreamProcessor where
        type Rs DeleteStreamProcessor = DeleteStreamProcessorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteStreamProcessorResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteStreamProcessorResponse' smart constructor.
newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamProcessorResponse' value with any optional fields omitted.
mkDeleteStreamProcessorResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteStreamProcessorResponse
mkDeleteStreamProcessorResponse responseStatus
  = DeleteStreamProcessorResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsResponseStatus :: Lens.Lens' DeleteStreamProcessorResponse Core.Int
dsprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
