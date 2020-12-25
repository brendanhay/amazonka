{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.NotifyWhenUploaded
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends you notification through CloudWatch Events when all files written to your file share have been uploaded to Amazon S3.
--
-- AWS Storage Gateway can send a notification through Amazon CloudWatch Events when all files written to your file share up to that point in time have been uploaded to Amazon S3. These files include files written to the file share up to the time that you make a request for notification. When the upload is done, Storage Gateway sends you notification through an Amazon CloudWatch Event. You can configure CloudWatch Events to send the notification through event targets such as Amazon SNS or AWS Lambda function. This operation is only supported for file gateways.
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-upload-notification Getting file upload notification> in the /AWS Storage Gateway User Guide/ .
module Network.AWS.StorageGateway.NotifyWhenUploaded
  ( -- * Creating a request
    NotifyWhenUploaded (..),
    mkNotifyWhenUploaded,

    -- ** Request lenses
    nwuFileShareARN,

    -- * Destructuring the response
    NotifyWhenUploadedResponse (..),
    mkNotifyWhenUploadedResponse,

    -- ** Response lenses
    nwurrsFileShareARN,
    nwurrsNotificationId,
    nwurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkNotifyWhenUploaded' smart constructor.
newtype NotifyWhenUploaded = NotifyWhenUploaded'
  { fileShareARN :: Types.FileShareARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyWhenUploaded' value with any optional fields omitted.
mkNotifyWhenUploaded ::
  -- | 'fileShareARN'
  Types.FileShareARN ->
  NotifyWhenUploaded
mkNotifyWhenUploaded fileShareARN =
  NotifyWhenUploaded' {fileShareARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwuFileShareARN :: Lens.Lens' NotifyWhenUploaded Types.FileShareARN
nwuFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED nwuFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

instance Core.FromJSON NotifyWhenUploaded where
  toJSON NotifyWhenUploaded {..} =
    Core.object
      (Core.catMaybes [Core.Just ("FileShareARN" Core..= fileShareARN)])

instance Core.AWSRequest NotifyWhenUploaded where
  type Rs NotifyWhenUploaded = NotifyWhenUploadedResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.NotifyWhenUploaded")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          NotifyWhenUploadedResponse'
            Core.<$> (x Core..:? "FileShareARN")
            Core.<*> (x Core..:? "NotificationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkNotifyWhenUploadedResponse' smart constructor.
data NotifyWhenUploadedResponse = NotifyWhenUploadedResponse'
  { fileShareARN :: Core.Maybe Types.FileShareARN,
    notificationId :: Core.Maybe Types.NotificationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyWhenUploadedResponse' value with any optional fields omitted.
mkNotifyWhenUploadedResponse ::
  -- | 'responseStatus'
  Core.Int ->
  NotifyWhenUploadedResponse
mkNotifyWhenUploadedResponse responseStatus =
  NotifyWhenUploadedResponse'
    { fileShareARN = Core.Nothing,
      notificationId = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwurrsFileShareARN :: Lens.Lens' NotifyWhenUploadedResponse (Core.Maybe Types.FileShareARN)
nwurrsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED nwurrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwurrsNotificationId :: Lens.Lens' NotifyWhenUploadedResponse (Core.Maybe Types.NotificationId)
nwurrsNotificationId = Lens.field @"notificationId"
{-# DEPRECATED nwurrsNotificationId "Use generic-lens or generic-optics with 'notificationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwurrsResponseStatus :: Lens.Lens' NotifyWhenUploadedResponse Core.Int
nwurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED nwurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
