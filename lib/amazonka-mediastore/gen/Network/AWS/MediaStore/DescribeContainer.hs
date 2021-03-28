{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.DescribeContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties of the requested container. This request is commonly used to retrieve the endpoint of a container. An endpoint is a value assigned by the service when a new container is created. A container's endpoint does not change after it has been assigned. The @DescribeContainer@ request returns a single @Container@ object based on @ContainerName@ . To return all @Container@ objects that are associated with a specified AWS account, use 'ListContainers' .
module Network.AWS.MediaStore.DescribeContainer
    (
    -- * Creating a request
      DescribeContainer (..)
    , mkDescribeContainer
    -- ** Request lenses
    , dContainerName

    -- * Destructuring the response
    , DescribeContainerResponse (..)
    , mkDescribeContainerResponse
    -- ** Response lenses
    , drsContainer
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContainer' smart constructor.
newtype DescribeContainer = DescribeContainer'
  { containerName :: Core.Maybe Types.ContainerName
    -- ^ The name of the container to query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContainer' value with any optional fields omitted.
mkDescribeContainer
    :: DescribeContainer
mkDescribeContainer
  = DescribeContainer'{containerName = Core.Nothing}

-- | The name of the container to query.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContainerName :: Lens.Lens' DescribeContainer (Core.Maybe Types.ContainerName)
dContainerName = Lens.field @"containerName"
{-# INLINEABLE dContainerName #-}
{-# DEPRECATED containerName "Use generic-lens or generic-optics with 'containerName' instead"  #-}

instance Core.ToQuery DescribeContainer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeContainer where
        toHeaders DescribeContainer{..}
          = Core.pure
              ("X-Amz-Target", "MediaStore_20170901.DescribeContainer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeContainer where
        toJSON DescribeContainer{..}
          = Core.object
              (Core.catMaybes [("ContainerName" Core..=) Core.<$> containerName])

instance Core.AWSRequest DescribeContainer where
        type Rs DescribeContainer = DescribeContainerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeContainerResponse' Core.<$>
                   (x Core..:? "Container") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeContainerResponse' smart constructor.
data DescribeContainerResponse = DescribeContainerResponse'
  { container :: Core.Maybe Types.Container
    -- ^ The name of the queried container.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeContainerResponse' value with any optional fields omitted.
mkDescribeContainerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeContainerResponse
mkDescribeContainerResponse responseStatus
  = DescribeContainerResponse'{container = Core.Nothing,
                               responseStatus}

-- | The name of the queried container.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContainer :: Lens.Lens' DescribeContainerResponse (Core.Maybe Types.Container)
drsContainer = Lens.field @"container"
{-# INLINEABLE drsContainer #-}
{-# DEPRECATED container "Use generic-lens or generic-optics with 'container' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeContainerResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
