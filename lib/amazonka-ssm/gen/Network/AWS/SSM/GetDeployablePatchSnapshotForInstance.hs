{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current snapshot for the patch baseline the instance uses. This API is primarily used by the AWS-RunPatchBaseline Systems Manager document. 
module Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
    (
    -- * Creating a request
      GetDeployablePatchSnapshotForInstance (..)
    , mkGetDeployablePatchSnapshotForInstance
    -- ** Request lenses
    , gdpsfiInstanceId
    , gdpsfiSnapshotId

    -- * Destructuring the response
    , GetDeployablePatchSnapshotForInstanceResponse (..)
    , mkGetDeployablePatchSnapshotForInstanceResponse
    -- ** Response lenses
    , gdpsfirrsInstanceId
    , gdpsfirrsProduct
    , gdpsfirrsSnapshotDownloadUrl
    , gdpsfirrsSnapshotId
    , gdpsfirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetDeployablePatchSnapshotForInstance' smart constructor.
data GetDeployablePatchSnapshotForInstance = GetDeployablePatchSnapshotForInstance'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance for which the appropriate patch snapshot should be retrieved.
  , snapshotId :: Types.SnapshotId
    -- ^ The user-defined snapshot ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeployablePatchSnapshotForInstance' value with any optional fields omitted.
mkGetDeployablePatchSnapshotForInstance
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.SnapshotId -- ^ 'snapshotId'
    -> GetDeployablePatchSnapshotForInstance
mkGetDeployablePatchSnapshotForInstance instanceId snapshotId
  = GetDeployablePatchSnapshotForInstance'{instanceId, snapshotId}

-- | The ID of the instance for which the appropriate patch snapshot should be retrieved.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfiInstanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Types.InstanceId
gdpsfiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gdpsfiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The user-defined snapshot ID.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfiSnapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Types.SnapshotId
gdpsfiSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE gdpsfiSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

instance Core.ToQuery GetDeployablePatchSnapshotForInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDeployablePatchSnapshotForInstance where
        toHeaders GetDeployablePatchSnapshotForInstance{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.GetDeployablePatchSnapshotForInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDeployablePatchSnapshotForInstance where
        toJSON GetDeployablePatchSnapshotForInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  Core.Just ("SnapshotId" Core..= snapshotId)])

instance Core.AWSRequest GetDeployablePatchSnapshotForInstance
         where
        type Rs GetDeployablePatchSnapshotForInstance =
             GetDeployablePatchSnapshotForInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeployablePatchSnapshotForInstanceResponse' Core.<$>
                   (x Core..:? "InstanceId") Core.<*> x Core..:? "Product" Core.<*>
                     x Core..:? "SnapshotDownloadUrl"
                     Core.<*> x Core..:? "SnapshotId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDeployablePatchSnapshotForInstanceResponse' smart constructor.
data GetDeployablePatchSnapshotForInstanceResponse = GetDeployablePatchSnapshotForInstanceResponse'
  { instanceId :: Core.Maybe Types.InstanceId
    -- ^ The ID of the instance.
  , product :: Core.Maybe Types.Product
    -- ^ Returns the specific operating system (for example Windows Server 2012 or Amazon Linux 2015.09) on the instance for the specified patch snapshot.
  , snapshotDownloadUrl :: Core.Maybe Types.SnapshotDownloadUrl
    -- ^ A pre-signed Amazon S3 URL that can be used to download the patch snapshot.
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The user-defined snapshot ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeployablePatchSnapshotForInstanceResponse' value with any optional fields omitted.
mkGetDeployablePatchSnapshotForInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeployablePatchSnapshotForInstanceResponse
mkGetDeployablePatchSnapshotForInstanceResponse responseStatus
  = GetDeployablePatchSnapshotForInstanceResponse'{instanceId =
                                                     Core.Nothing,
                                                   product = Core.Nothing,
                                                   snapshotDownloadUrl = Core.Nothing,
                                                   snapshotId = Core.Nothing, responseStatus}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirrsInstanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Types.InstanceId)
gdpsfirrsInstanceId = Lens.field @"instanceId"
{-# INLINEABLE gdpsfirrsInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Returns the specific operating system (for example Windows Server 2012 or Amazon Linux 2015.09) on the instance for the specified patch snapshot.
--
-- /Note:/ Consider using 'product' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirrsProduct :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Types.Product)
gdpsfirrsProduct = Lens.field @"product"
{-# INLINEABLE gdpsfirrsProduct #-}
{-# DEPRECATED product "Use generic-lens or generic-optics with 'product' instead"  #-}

-- | A pre-signed Amazon S3 URL that can be used to download the patch snapshot.
--
-- /Note:/ Consider using 'snapshotDownloadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirrsSnapshotDownloadUrl :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Types.SnapshotDownloadUrl)
gdpsfirrsSnapshotDownloadUrl = Lens.field @"snapshotDownloadUrl"
{-# INLINEABLE gdpsfirrsSnapshotDownloadUrl #-}
{-# DEPRECATED snapshotDownloadUrl "Use generic-lens or generic-optics with 'snapshotDownloadUrl' instead"  #-}

-- | The user-defined snapshot ID.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirrsSnapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Types.SnapshotId)
gdpsfirrsSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE gdpsfirrsSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirrsResponseStatus :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse Core.Int
gdpsfirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdpsfirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
