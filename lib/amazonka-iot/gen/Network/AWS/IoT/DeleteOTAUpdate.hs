{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteOTAUpdate (..),
    mkDeleteOTAUpdate,

    -- ** Request lenses
    dotauOtaUpdateId,
    dotauDeleteStream,
    dotauForceDeleteAWSJob,

    -- * Destructuring the response
    DeleteOTAUpdateResponse (..),
    mkDeleteOTAUpdateResponse,

    -- ** Response lenses
    dotaurrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteOTAUpdate' smart constructor.
data DeleteOTAUpdate = DeleteOTAUpdate'
  { -- | The ID of the OTA update to delete.
    otaUpdateId :: Types.OtaUpdateId,
    -- | Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
    deleteStream :: Core.Maybe Core.Bool,
    -- | Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
    forceDeleteAWSJob :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOTAUpdate' value with any optional fields omitted.
mkDeleteOTAUpdate ::
  -- | 'otaUpdateId'
  Types.OtaUpdateId ->
  DeleteOTAUpdate
mkDeleteOTAUpdate otaUpdateId =
  DeleteOTAUpdate'
    { otaUpdateId,
      deleteStream = Core.Nothing,
      forceDeleteAWSJob = Core.Nothing
    }

-- | The ID of the OTA update to delete.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauOtaUpdateId :: Lens.Lens' DeleteOTAUpdate Types.OtaUpdateId
dotauOtaUpdateId = Lens.field @"otaUpdateId"
{-# DEPRECATED dotauOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

-- | Specifies if the stream associated with an OTA update should be deleted when the OTA update is deleted.
--
-- /Note:/ Consider using 'deleteStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauDeleteStream :: Lens.Lens' DeleteOTAUpdate (Core.Maybe Core.Bool)
dotauDeleteStream = Lens.field @"deleteStream"
{-# DEPRECATED dotauDeleteStream "Use generic-lens or generic-optics with 'deleteStream' instead." #-}

-- | Specifies if the AWS Job associated with the OTA update should be deleted when the OTA update is deleted.
--
-- /Note:/ Consider using 'forceDeleteAWSJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotauForceDeleteAWSJob :: Lens.Lens' DeleteOTAUpdate (Core.Maybe Core.Bool)
dotauForceDeleteAWSJob = Lens.field @"forceDeleteAWSJob"
{-# DEPRECATED dotauForceDeleteAWSJob "Use generic-lens or generic-optics with 'forceDeleteAWSJob' instead." #-}

instance Core.AWSRequest DeleteOTAUpdate where
  type Rs DeleteOTAUpdate = DeleteOTAUpdateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/otaUpdates/" Core.<> (Core.toText otaUpdateId)),
        Core._rqQuery =
          Core.toQueryValue "deleteStream" Core.<$> deleteStream
            Core.<> (Core.toQueryValue "forceDeleteAWSJob" Core.<$> forceDeleteAWSJob),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOTAUpdateResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteOTAUpdateResponse' smart constructor.
newtype DeleteOTAUpdateResponse = DeleteOTAUpdateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteOTAUpdateResponse' value with any optional fields omitted.
mkDeleteOTAUpdateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteOTAUpdateResponse
mkDeleteOTAUpdateResponse responseStatus =
  DeleteOTAUpdateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dotaurrsResponseStatus :: Lens.Lens' DeleteOTAUpdateResponse Core.Int
dotaurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dotaurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
