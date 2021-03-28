{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVolumeAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a volume attribute.
--
-- By default, all I/O operations for the volume are suspended when the data on the volume is determined to be potentially inconsistent, to prevent undetectable, latent data corruption. The I/O access to the volume can be resumed by first enabling I/O access and then checking the data consistency on your volume.
-- You can change the default behavior to resume I/O operations. We recommend that you change this only for boot volumes or for volumes that are stateless or disposable.
module Network.AWS.EC2.ModifyVolumeAttribute
    (
    -- * Creating a request
      ModifyVolumeAttribute (..)
    , mkModifyVolumeAttribute
    -- ** Request lenses
    , mvaVolumeId
    , mvaAutoEnableIO
    , mvaDryRun

    -- * Destructuring the response
    , ModifyVolumeAttributeResponse (..)
    , mkModifyVolumeAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyVolumeAttribute' smart constructor.
data ModifyVolumeAttribute = ModifyVolumeAttribute'
  { volumeId :: Types.VolumeId
    -- ^ The ID of the volume.
  , autoEnableIO :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether the volume should be auto-enabled for I/O operations.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVolumeAttribute' value with any optional fields omitted.
mkModifyVolumeAttribute
    :: Types.VolumeId -- ^ 'volumeId'
    -> ModifyVolumeAttribute
mkModifyVolumeAttribute volumeId
  = ModifyVolumeAttribute'{volumeId, autoEnableIO = Core.Nothing,
                           dryRun = Core.Nothing}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaVolumeId :: Lens.Lens' ModifyVolumeAttribute Types.VolumeId
mvaVolumeId = Lens.field @"volumeId"
{-# INLINEABLE mvaVolumeId #-}
{-# DEPRECATED volumeId "Use generic-lens or generic-optics with 'volumeId' instead"  #-}

-- | Indicates whether the volume should be auto-enabled for I/O operations.
--
-- /Note:/ Consider using 'autoEnableIO' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaAutoEnableIO :: Lens.Lens' ModifyVolumeAttribute (Core.Maybe Types.AttributeBooleanValue)
mvaAutoEnableIO = Lens.field @"autoEnableIO"
{-# INLINEABLE mvaAutoEnableIO #-}
{-# DEPRECATED autoEnableIO "Use generic-lens or generic-optics with 'autoEnableIO' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvaDryRun :: Lens.Lens' ModifyVolumeAttribute (Core.Maybe Core.Bool)
mvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE mvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ModifyVolumeAttribute where
        toQuery ModifyVolumeAttribute{..}
          = Core.toQueryPair "Action" ("ModifyVolumeAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VolumeId" volumeId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoEnableIO")
                autoEnableIO
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ModifyVolumeAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyVolumeAttribute where
        type Rs ModifyVolumeAttribute = ModifyVolumeAttributeResponse
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
        parseResponse = Response.receiveNull ModifyVolumeAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyVolumeAttributeResponse' smart constructor.
data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyVolumeAttributeResponse' value with any optional fields omitted.
mkModifyVolumeAttributeResponse
    :: ModifyVolumeAttributeResponse
mkModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse'
