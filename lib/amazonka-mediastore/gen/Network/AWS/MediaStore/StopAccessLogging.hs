{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.StopAccessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops access logging on the specified container. When you stop access logging on a container, MediaStore stops sending access logs to Amazon CloudWatch Logs. These access logs are not saved and are not retrievable.
module Network.AWS.MediaStore.StopAccessLogging
    (
    -- * Creating a request
      StopAccessLogging (..)
    , mkStopAccessLogging
    -- ** Request lenses
    , salContainerName

    -- * Destructuring the response
    , StopAccessLoggingResponse (..)
    , mkStopAccessLoggingResponse
    -- ** Response lenses
    , salrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopAccessLogging' smart constructor.
newtype StopAccessLogging = StopAccessLogging'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that you want to stop access logging on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAccessLogging' value with any optional fields omitted.
mkStopAccessLogging
    :: Types.ContainerName -- ^ 'containerName'
    -> StopAccessLogging
mkStopAccessLogging containerName
  = StopAccessLogging'{containerName}

-- | The name of the container that you want to stop access logging on.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salContainerName :: Lens.Lens' StopAccessLogging Types.ContainerName
salContainerName = Lens.field @"containerName"
{-# INLINEABLE salContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery StopAccessLogging where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopAccessLogging where
        toHeaders StopAccessLogging{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.StopAccessLogging")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopAccessLogging where
        toJSON StopAccessLogging{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest StopAccessLogging where
        type Rs StopAccessLogging = StopAccessLoggingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopAccessLoggingResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopAccessLoggingResponse' smart constructor.
newtype StopAccessLoggingResponse = StopAccessLoggingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAccessLoggingResponse' value with any optional fields omitted.
mkStopAccessLoggingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopAccessLoggingResponse
mkStopAccessLoggingResponse responseStatus
  = StopAccessLoggingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salrrsResponseStatus :: Lens.Lens' StopAccessLoggingResponse Core.Int
salrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE salrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
