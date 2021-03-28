{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables I/O operations for a volume that had I/O operations disabled because the data on the volume was potentially inconsistent.
module Network.AWS.EC2.EnableVolumeIO
    (
    -- * Creating a request
      EnableVolumeIO (..)
    , mkEnableVolumeIO
    -- ** Request lenses
    , evioVolumeId
    , evioDryRun

    -- * Destructuring the response
    , EnableVolumeIOResponse (..)
    , mkEnableVolumeIOResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableVolumeIO' smart constructor.
data EnableVolumeIO = EnableVolumeIO'
  { volumeId :: Types.VolumeId
    -- ^ The ID of the volume.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVolumeIO' value with any optional fields omitted.
mkEnableVolumeIO
    :: Types.VolumeId -- ^ 'volumeId'
    -> EnableVolumeIO
mkEnableVolumeIO volumeId
  = EnableVolumeIO'{volumeId, dryRun = Core.Nothing}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evioVolumeId :: Lens.Lens' EnableVolumeIO Types.VolumeId
evioVolumeId = Lens.field @"volumeId"
{-# INLINEABLE evioVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evioDryRun :: Lens.Lens' EnableVolumeIO (Core.Maybe Core.Bool)
evioDryRun = Lens.field @"dryRun"
{-# INLINEABLE evioDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery EnableVolumeIO where
        toQuery EnableVolumeIO{..}
          = Core.toQueryPair "Action" ("EnableVolumeIO" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VolumeId" volumeId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders EnableVolumeIO where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableVolumeIO where
        type Rs EnableVolumeIO = EnableVolumeIOResponse
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
        parseResponse = Response.receiveNull EnableVolumeIOResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableVolumeIOResponse' smart constructor.
data EnableVolumeIOResponse = EnableVolumeIOResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableVolumeIOResponse' value with any optional fields omitted.
mkEnableVolumeIOResponse
    :: EnableVolumeIOResponse
mkEnableVolumeIOResponse = EnableVolumeIOResponse'
