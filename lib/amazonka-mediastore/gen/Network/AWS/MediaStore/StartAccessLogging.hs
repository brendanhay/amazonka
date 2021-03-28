{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.StartAccessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts access logging on the specified container. When you enable access logging on a container, MediaStore delivers access logs for objects stored in that container to Amazon CloudWatch Logs.
module Network.AWS.MediaStore.StartAccessLogging
    (
    -- * Creating a request
      StartAccessLogging (..)
    , mkStartAccessLogging
    -- ** Request lenses
    , sContainerName

    -- * Destructuring the response
    , StartAccessLoggingResponse (..)
    , mkStartAccessLoggingResponse
    -- ** Response lenses
    , srsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartAccessLogging' smart constructor.
newtype StartAccessLogging = StartAccessLogging'
  { containerName :: Types.ContainerName
    -- ^ The name of the container that you want to start access logging on.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAccessLogging' value with any optional fields omitted.
mkStartAccessLogging
    :: Types.ContainerName -- ^ 'containerName'
    -> StartAccessLogging
mkStartAccessLogging containerName
  = StartAccessLogging'{containerName}

-- | The name of the container that you want to start access logging on.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sContainerName :: Lens.Lens' StartAccessLogging Types.ContainerName
sContainerName = Lens.field @"containerName"
{-# INLINEABLE sContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery StartAccessLogging where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartAccessLogging where
        toHeaders StartAccessLogging{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.StartAccessLogging")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartAccessLogging where
        toJSON StartAccessLogging{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ContainerName" Core..= containerName)])

instance Core.AWSRequest StartAccessLogging where
        type Rs StartAccessLogging = StartAccessLoggingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartAccessLoggingResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartAccessLoggingResponse' smart constructor.
newtype StartAccessLoggingResponse = StartAccessLoggingResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartAccessLoggingResponse' value with any optional fields omitted.
mkStartAccessLoggingResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartAccessLoggingResponse
mkStartAccessLoggingResponse responseStatus
  = StartAccessLoggingResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartAccessLoggingResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
