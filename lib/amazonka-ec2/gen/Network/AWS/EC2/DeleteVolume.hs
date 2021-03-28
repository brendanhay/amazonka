{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EBS volume. The volume must be in the @available@ state (not attached to an instance).
--
-- The volume can remain in the @deleting@ state for several minutes.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS volume> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DeleteVolume
    (
    -- * Creating a request
      DeleteVolume (..)
    , mkDeleteVolume
    -- ** Request lenses
    , dvlVolumeId
    , dvlDryRun

    -- * Destructuring the response
    , DeleteVolumeResponse (..)
    , mkDeleteVolumeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { volumeId :: Types.VolumeId
    -- ^ The ID of the volume.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVolume' value with any optional fields omitted.
mkDeleteVolume
    :: Types.VolumeId -- ^ 'volumeId'
    -> DeleteVolume
mkDeleteVolume volumeId
  = DeleteVolume'{volumeId, dryRun = Core.Nothing}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlVolumeId :: Lens.Lens' DeleteVolume Types.VolumeId
dvlVolumeId = Lens.field @"volumeId"
{-# INLINEABLE dvlVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvlDryRun :: Lens.Lens' DeleteVolume (Core.Maybe Core.Bool)
dvlDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvlDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteVolume where
        toQuery DeleteVolume{..}
          = Core.toQueryPair "Action" ("DeleteVolume" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VolumeId" volumeId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteVolume where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteVolume where
        type Rs DeleteVolume = DeleteVolumeResponse
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
        parseResponse = Response.receiveNull DeleteVolumeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVolumeResponse' value with any optional fields omitted.
mkDeleteVolumeResponse
    :: DeleteVolumeResponse
mkDeleteVolumeResponse = DeleteVolumeResponse'
