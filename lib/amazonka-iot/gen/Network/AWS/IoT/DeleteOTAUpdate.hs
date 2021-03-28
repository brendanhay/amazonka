{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteOTAUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an OTA update.
module Network.AWS.IoT.DeleteOTAUpdate
    (
    -- * Creating a request
      DeleteOTAUpdate (..)
    , mkDeleteOTAUpdate
    -- ** Request lenses
    , dotauOtaUpdateId
    , dotauDeleteStream
    , dotauForceDeleteAWSJob

    -- * Destructuring the response
    , DeleteOTAUpdateResponse (..)
    , mkDeleteOTAUpdateResponse
    -- ** Response lenses
    , dotaurrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOTAUpdate' smart constructor.
data DeleteOTAUpdate = DeleteOTAUpdate'
  { otaUpdateId :: Types.OtaUpdateId
    -- ^ The ID of the OTA update to delete.
  , deleteStream :: Core.Maybe Core.Bool
    -- ^ Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
  , forceDeleteAWSJob :: Core.Maybe Core.Bool
    -- ^ Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOTAUpdate' value with any optional fields omitted.
mkDeleteOTAUpdate
    :: Types.OtaUpdateId -- ^ 'otaUpdateId'
    -> DeleteOTAUpdate
mkDeleteOTAUpdate otaUpdateId
  = DeleteOTAUpdate'{otaUpdateId, deleteStream = Core.Nothing,
                     forceDeleteAWSJob = Core.Nothing}

-- | The ID of the OTA update to delete.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauOtaUpdateId :: Lens.Lens' DeleteOTAUpdate Types.OtaUpdateId
dotauOtaUpdateId = Lens.field @"otaUpdateId"
{-# INLINEABLE dotauOtaUpdateId #-}
{-# DEPRECATED otaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead"  #-}

-- | Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
--
-- /Note:/ Consider using 'deleteStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauDeleteStream :: Lens.Lens' DeleteOTAUpdate (Core.Maybe Core.Bool)
dotauDeleteStream = Lens.field @"deleteStream"
{-# INLINEABLE dotauDeleteStream #-}
{-# DEPRECATED deleteStream "Use generic-lens or generic-optics with 'deleteStream' instead"  #-}

-- | Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
--
-- /Note:/ Consider using 'forceDeleteAWSJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauForceDeleteAWSJob :: Lens.Lens' DeleteOTAUpdate (Core.Maybe Core.Bool)
dotauForceDeleteAWSJob = Lens.field @"forceDeleteAWSJob"
{-# INLINEABLE dotauForceDeleteAWSJob #-}
{-# DEPRECATED forceDeleteAWSJob "Use generic-lens or generic-optics with 'forceDeleteAWSJob' instead"  #-}

instance Core.ToQuery DeleteOTAUpdate where
        toQuery DeleteOTAUpdate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "deleteStream")
              deleteStream
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "forceDeleteAWSJob")
                forceDeleteAWSJob

instance Core.ToHeaders DeleteOTAUpdate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteOTAUpdate where
        type Rs DeleteOTAUpdate = DeleteOTAUpdateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/otaUpdates/" Core.<> Core.toText otaUpdateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteOTAUpdateResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteOTAUpdateResponse' smart constructor.
newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOTAUpdateResponse' value with any optional fields omitted.
mkDeleteOTAUpdateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteOTAUpdateResponse
mkDeleteOTAUpdateResponse responseStatus
  = DeleteOTAUpdateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotaurrsResponseStatus :: Lens.Lens' DeleteOTAUpdateResponse Core.Int
dotaurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dotaurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
