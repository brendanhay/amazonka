{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RefreshCache (..)
    , mkRefreshCache
    -- ** Request lenses
    , rcFileShareARN
    , rcFolderList
    , rcRecursive

    -- * Destructuring the response
    , RefreshCacheResponse (..)
    , mkRefreshCacheResponse
    -- ** Response lenses
    , rcrrsFileShareARN
    , rcrrsNotificationId
    , rcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | RefreshCacheInput
--
-- /See:/ 'mkRefreshCache' smart constructor.
data RefreshCache = RefreshCache'
  { fileShareARN :: Types.FileShareARN
    -- ^ The Amazon Resource Name (ARN) of the file share you want to refresh.
  , folderList :: Core.Maybe (Core.NonEmpty Types.Folder)
    -- ^ A comma-separated list of the paths of folders to refresh in the cache. The default is [@"/"@ ]. The default refreshes objects and folders at the root of the Amazon S3 bucket. If @Recursive@ is set to @true@ , the entire S3 bucket that the file share has access to is refreshed.
  , recursive :: Core.Maybe Core.Bool
    -- ^ A value that specifies whether to recursively refresh folders in the cache. The refresh includes folders that were in the cache the last time the gateway listed the folder's contents. If this value set to @true@ , each folder that is listed in @FolderList@ is recursively updated. Otherwise, subfolders listed in @FolderList@ are not refreshed. Only objects that are in folders listed directly under @FolderList@ are found and used for the update. The default is @true@ .
--
-- Valid Values: @true@ | @false@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RefreshCache' value with any optional fields omitted.
mkRefreshCache
    :: Types.FileShareARN -- ^ 'fileShareARN'
    -> RefreshCache
mkRefreshCache fileShareARN
  = RefreshCache'{fileShareARN, folderList = Core.Nothing,
                  recursive = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the file share you want to refresh.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFileShareARN :: Lens.Lens' RefreshCache Types.FileShareARN
rcFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE rcFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | A comma-separated list of the paths of folders to refresh in the cache. The default is [@"/"@ ]. The default refreshes objects and folders at the root of the Amazon S3 bucket. If @Recursive@ is set to @true@ , the entire S3 bucket that the file share has access to is refreshed.
--
-- /Note:/ Consider using 'folderList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcFolderList :: Lens.Lens' RefreshCache (Core.Maybe (Core.NonEmpty Types.Folder))
rcFolderList = Lens.field @"folderList"
{-# INLINEABLE rcFolderList #-}
{-# DEPRECATED folderList "Use generic-lens or generic-optics with 'folderList' instead"  #-}

-- | A value that specifies whether to recursively refresh folders in the cache. The refresh includes folders that were in the cache the last time the gateway listed the folder's contents. If this value set to @true@ , each folder that is listed in @FolderList@ is recursively updated. Otherwise, subfolders listed in @FolderList@ are not refreshed. Only objects that are in folders listed directly under @FolderList@ are found and used for the update. The default is @true@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecursive :: Lens.Lens' RefreshCache (Core.Maybe Core.Bool)
rcRecursive = Lens.field @"recursive"
{-# INLINEABLE rcRecursive #-}
{-# DEPRECATED recursive "Use generic-lens or generic-optics with 'recursive' instead"  #-}

instance Core.ToQuery RefreshCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RefreshCache where
        toHeaders RefreshCache{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.RefreshCache")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RefreshCache where
        toJSON RefreshCache{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FileShareARN" Core..= fileShareARN),
                  ("FolderList" Core..=) Core.<$> folderList,
                  ("Recursive" Core..=) Core.<$> recursive])

instance Core.AWSRequest RefreshCache where
        type Rs RefreshCache = RefreshCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RefreshCacheResponse' Core.<$>
                   (x Core..:? "FileShareARN") Core.<*> x Core..:? "NotificationId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | RefreshCacheOutput
--
-- /See:/ 'mkRefreshCacheResponse' smart constructor.
data RefreshCacheResponse = RefreshCacheResponse'
  { fileShareARN :: Core.Maybe Types.FileShareARN
  , notificationId :: Core.Maybe Types.NotificationId
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RefreshCacheResponse' value with any optional fields omitted.
mkRefreshCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RefreshCacheResponse
mkRefreshCacheResponse responseStatus
  = RefreshCacheResponse'{fileShareARN = Core.Nothing,
                          notificationId = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsFileShareARN :: Lens.Lens' RefreshCacheResponse (Core.Maybe Types.FileShareARN)
rcrrsFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE rcrrsFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'notificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsNotificationId :: Lens.Lens' RefreshCacheResponse (Core.Maybe Types.NotificationId)
rcrrsNotificationId = Lens.field @"notificationId"
{-# INLINEABLE rcrrsNotificationId #-}
{-# DEPRECATED notificationId "Use generic-lens or generic-optics with 'notificationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' RefreshCacheResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
