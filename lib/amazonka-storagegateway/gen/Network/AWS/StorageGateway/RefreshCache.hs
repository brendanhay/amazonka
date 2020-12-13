{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RefreshCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes the cache for the specified file share. This operation finds objects in the Amazon S3 bucket that were added, removed, or replaced since the gateway last listed the bucket's contents and cached the results. This operation is only supported in the file gateway type. You can subscribe to be notified through an Amazon CloudWatch event when your RefreshCache operation completes. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-notification Getting notified about file operations> in the /AWS Storage Gateway User Guide/ .
--
-- When this API is called, it only initiates the refresh operation. When the API call completes and returns a success code, it doesn't necessarily mean that the file refresh has completed. You should use the refresh-complete notification to determine that the operation has completed before you check for new files on the gateway file share. You can subscribe to be notified through an CloudWatch event when your @RefreshCache@ operation completes.
-- Throttle limit: This API is asynchronous so the gateway will accept no more than two refreshes at any time. We recommend using the refresh-complete CloudWatch event notification before issuing additional requests. For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-notification Getting notified about file operations> in the /AWS Storage Gateway User Guide/ .
-- If you invoke the RefreshCache API when two requests are already being processed, any new request will cause an @InvalidGatewayRequestException@ error because too many requests were sent to the server.
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-notification Getting notified about file operations> in the /AWS Storage Gateway User Guide/ .
module Network.AWS.StorageGateway.RefreshCache
  ( -- * Creating a request
    RefreshCache (..),
    mkRefreshCache,

    -- ** Request lenses
    rcFileShareARN,
    rcFolderList,
    rcRecursive,

    -- * Destructuring the response
    RefreshCacheResponse (..),
    mkRefreshCacheResponse,

    -- ** Response lenses
    rcrsFileShareARN,
    rcrsNotificationId,
    rcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | RefreshCacheInput
--
-- /See:/ 'mkRefreshCache' smart constructor.
data RefreshCache = RefreshCache'
  { -- | The Amazon Resource Name (ARN) of the file share you want to refresh.
    fileShareARN :: Lude.Text,
    -- | A comma-separated list of the paths of folders to refresh in the cache. The default is [@"/"@ ]. The default refreshes objects and folders at the root of the Amazon S3 bucket. If @Recursive@ is set to @true@ , the entire S3 bucket that the file share has access to is refreshed.
    folderList :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A value that specifies whether to recursively refresh folders in the cache. The refresh includes folders that were in the cache the last time the gateway listed the folder's contents. If this value set to @true@ , each folder that is listed in @FolderList@ is recursively updated. Otherwise, subfolders listed in @FolderList@ are not refreshed. Only objects that are in folders listed directly under @FolderList@ are found and used for the update. The default is @true@ .
    --
    -- Valid Values: @true@ | @false@
    recursive :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshCache' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the file share you want to refresh.
-- * 'folderList' - A comma-separated list of the paths of folders to refresh in the cache. The default is [@"/"@ ]. The default refreshes objects and folders at the root of the Amazon S3 bucket. If @Recursive@ is set to @true@ , the entire S3 bucket that the file share has access to is refreshed.
-- * 'recursive' - A value that specifies whether to recursively refresh folders in the cache. The refresh includes folders that were in the cache the last time the gateway listed the folder's contents. If this value set to @true@ , each folder that is listed in @FolderList@ is recursively updated. Otherwise, subfolders listed in @FolderList@ are not refreshed. Only objects that are in folders listed directly under @FolderList@ are found and used for the update. The default is @true@ .
--
-- Valid Values: @true@ | @false@
mkRefreshCache ::
  -- | 'fileShareARN'
  Lude.Text ->
  RefreshCache
mkRefreshCache pFileShareARN_ =
  RefreshCache'
    { fileShareARN = pFileShareARN_,
      folderList = Lude.Nothing,
      recursive = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the file share you want to refresh.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFileShareARN :: Lens.Lens' RefreshCache Lude.Text
rcFileShareARN = Lens.lens (fileShareARN :: RefreshCache -> Lude.Text) (\s a -> s {fileShareARN = a} :: RefreshCache)
{-# DEPRECATED rcFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | A comma-separated list of the paths of folders to refresh in the cache. The default is [@"/"@ ]. The default refreshes objects and folders at the root of the Amazon S3 bucket. If @Recursive@ is set to @true@ , the entire S3 bucket that the file share has access to is refreshed.
--
-- /Note:/ Consider using 'folderList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFolderList :: Lens.Lens' RefreshCache (Lude.Maybe (Lude.NonEmpty Lude.Text))
rcFolderList = Lens.lens (folderList :: RefreshCache -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {folderList = a} :: RefreshCache)
{-# DEPRECATED rcFolderList "Use generic-lens or generic-optics with 'folderList' instead." #-}

-- | A value that specifies whether to recursively refresh folders in the cache. The refresh includes folders that were in the cache the last time the gateway listed the folder's contents. If this value set to @true@ , each folder that is listed in @FolderList@ is recursively updated. Otherwise, subfolders listed in @FolderList@ are not refreshed. Only objects that are in folders listed directly under @FolderList@ are found and used for the update. The default is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecursive :: Lens.Lens' RefreshCache (Lude.Maybe Lude.Bool)
rcRecursive = Lens.lens (recursive :: RefreshCache -> Lude.Maybe Lude.Bool) (\s a -> s {recursive = a} :: RefreshCache)
{-# DEPRECATED rcRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

instance Lude.AWSRequest RefreshCache where
  type Rs RefreshCache = RefreshCacheResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          RefreshCacheResponse'
            Lude.<$> (x Lude..?> "FileShareARN")
            Lude.<*> (x Lude..?> "NotificationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RefreshCache where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.RefreshCache" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RefreshCache where
  toJSON RefreshCache' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FileShareARN" Lude..= fileShareARN),
            ("FolderList" Lude..=) Lude.<$> folderList,
            ("Recursive" Lude..=) Lude.<$> recursive
          ]
      )

instance Lude.ToPath RefreshCache where
  toPath = Lude.const "/"

instance Lude.ToQuery RefreshCache where
  toQuery = Lude.const Lude.mempty

-- | RefreshCacheOutput
--
-- /See:/ 'mkRefreshCacheResponse' smart constructor.
data RefreshCacheResponse = RefreshCacheResponse'
  { fileShareARN :: Lude.Maybe Lude.Text,
    notificationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshCacheResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' -
-- * 'notificationId' -
-- * 'responseStatus' - The response status code.
mkRefreshCacheResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RefreshCacheResponse
mkRefreshCacheResponse pResponseStatus_ =
  RefreshCacheResponse'
    { fileShareARN = Lude.Nothing,
      notificationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsFileShareARN :: Lens.Lens' RefreshCacheResponse (Lude.Maybe Lude.Text)
rcrsFileShareARN = Lens.lens (fileShareARN :: RefreshCacheResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: RefreshCacheResponse)
{-# DEPRECATED rcrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsNotificationId :: Lens.Lens' RefreshCacheResponse (Lude.Maybe Lude.Text)
rcrsNotificationId = Lens.lens (notificationId :: RefreshCacheResponse -> Lude.Maybe Lude.Text) (\s a -> s {notificationId = a} :: RefreshCacheResponse)
{-# DEPRECATED rcrsNotificationId "Use generic-lens or generic-optics with 'notificationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrsResponseStatus :: Lens.Lens' RefreshCacheResponse Lude.Int
rcrsResponseStatus = Lens.lens (responseStatus :: RefreshCacheResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RefreshCacheResponse)
{-# DEPRECATED rcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
