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
    nwursFileShareARN,
    nwursNotificationId,
    nwursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkNotifyWhenUploaded' smart constructor.
newtype NotifyWhenUploaded = NotifyWhenUploaded'
  { fileShareARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyWhenUploaded' with the minimum fields required to make a request.
--
-- * 'fileShareARN' -
mkNotifyWhenUploaded ::
  -- | 'fileShareARN'
  Lude.Text ->
  NotifyWhenUploaded
mkNotifyWhenUploaded pFileShareARN_ =
  NotifyWhenUploaded' {fileShareARN = pFileShareARN_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwuFileShareARN :: Lens.Lens' NotifyWhenUploaded Lude.Text
nwuFileShareARN = Lens.lens (fileShareARN :: NotifyWhenUploaded -> Lude.Text) (\s a -> s {fileShareARN = a} :: NotifyWhenUploaded)
{-# DEPRECATED nwuFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

instance Lude.AWSRequest NotifyWhenUploaded where
  type Rs NotifyWhenUploaded = NotifyWhenUploadedResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          NotifyWhenUploadedResponse'
            Lude.<$> (x Lude..?> "FileShareARN")
            Lude.<*> (x Lude..?> "NotificationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders NotifyWhenUploaded where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.NotifyWhenUploaded" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON NotifyWhenUploaded where
  toJSON NotifyWhenUploaded' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("FileShareARN" Lude..= fileShareARN)])

instance Lude.ToPath NotifyWhenUploaded where
  toPath = Lude.const "/"

instance Lude.ToQuery NotifyWhenUploaded where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkNotifyWhenUploadedResponse' smart constructor.
data NotifyWhenUploadedResponse = NotifyWhenUploadedResponse'
  { fileShareARN :: Lude.Maybe Lude.Text,
    notificationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyWhenUploadedResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' -
-- * 'notificationId' -
-- * 'responseStatus' - The response status code.
mkNotifyWhenUploadedResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  NotifyWhenUploadedResponse
mkNotifyWhenUploadedResponse pResponseStatus_ =
  NotifyWhenUploadedResponse'
    { fileShareARN = Lude.Nothing,
      notificationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwursFileShareARN :: Lens.Lens' NotifyWhenUploadedResponse (Lude.Maybe Lude.Text)
nwursFileShareARN = Lens.lens (fileShareARN :: NotifyWhenUploadedResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: NotifyWhenUploadedResponse)
{-# DEPRECATED nwursFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwursNotificationId :: Lens.Lens' NotifyWhenUploadedResponse (Lude.Maybe Lude.Text)
nwursNotificationId = Lens.lens (notificationId :: NotifyWhenUploadedResponse -> Lude.Maybe Lude.Text) (\s a -> s {notificationId = a} :: NotifyWhenUploadedResponse)
{-# DEPRECATED nwursNotificationId "Use generic-lens or generic-optics with 'notificationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwursResponseStatus :: Lens.Lens' NotifyWhenUploadedResponse Lude.Int
nwursResponseStatus = Lens.lens (responseStatus :: NotifyWhenUploadedResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: NotifyWhenUploadedResponse)
{-# DEPRECATED nwursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
