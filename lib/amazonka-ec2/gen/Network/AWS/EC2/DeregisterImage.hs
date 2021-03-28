{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified AMI. After you deregister an AMI, it can't be used to launch new instances; however, it doesn't affect any instances that you've already launched from the AMI. You'll continue to incur usage costs for those instances until you terminate them.
--
-- When you deregister an Amazon EBS-backed AMI, it doesn't affect the snapshot that was created for the root volume of the instance during the AMI creation process. When you deregister an instance store-backed AMI, it doesn't affect the files that you uploaded to Amazon S3 when you created the AMI.
module Network.AWS.EC2.DeregisterImage
    (
    -- * Creating a request
      DeregisterImage (..)
    , mkDeregisterImage
    -- ** Request lenses
    , diImageId
    , diDryRun

    -- * Destructuring the response
    , DeregisterImageResponse (..)
    , mkDeregisterImageResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeregisterImage.
--
-- /See:/ 'mkDeregisterImage' smart constructor.
data DeregisterImage = DeregisterImage'
  { imageId :: Types.ImageId
    -- ^ The ID of the AMI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterImage' value with any optional fields omitted.
mkDeregisterImage
    :: Types.ImageId -- ^ 'imageId'
    -> DeregisterImage
mkDeregisterImage imageId
  = DeregisterImage'{imageId, dryRun = Core.Nothing}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageId :: Lens.Lens' DeregisterImage Types.ImageId
diImageId = Lens.field @"imageId"
{-# INLINEABLE diImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDryRun :: Lens.Lens' DeregisterImage (Core.Maybe Core.Bool)
diDryRun = Lens.field @"dryRun"
{-# INLINEABLE diDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeregisterImage where
        toQuery DeregisterImage{..}
          = Core.toQueryPair "Action" ("DeregisterImage" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ImageId" imageId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeregisterImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeregisterImage where
        type Rs DeregisterImage = DeregisterImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeregisterImageResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterImageResponse' smart constructor.
data DeregisterImageResponse = DeregisterImageResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterImageResponse' value with any optional fields omitted.
mkDeregisterImageResponse
    :: DeregisterImageResponse
mkDeregisterImageResponse = DeregisterImageResponse'
