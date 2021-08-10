{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.RefreshCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes the cache for the specified file share. This operation finds
-- objects in the Amazon S3 bucket that were added, removed, or replaced
-- since the gateway last listed the bucket\'s contents and cached the
-- results. This operation is only supported in the file gateway type. You
-- can subscribe to be notified through an Amazon CloudWatch event when
-- your RefreshCache operation completes. For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-notification Getting notified about file operations>
-- in the /AWS Storage Gateway User Guide/.
--
-- When this API is called, it only initiates the refresh operation. When
-- the API call completes and returns a success code, it doesn\'t
-- necessarily mean that the file refresh has completed. You should use the
-- refresh-complete notification to determine that the operation has
-- completed before you check for new files on the gateway file share. You
-- can subscribe to be notified through an CloudWatch event when your
-- @RefreshCache@ operation completes.
--
-- Throttle limit: This API is asynchronous so the gateway will accept no
-- more than two refreshes at any time. We recommend using the
-- refresh-complete CloudWatch event notification before issuing additional
-- requests. For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-notification Getting notified about file operations>
-- in the /AWS Storage Gateway User Guide/.
--
-- If you invoke the RefreshCache API when two requests are already being
-- processed, any new request will cause an
-- @InvalidGatewayRequestException@ error because too many requests were
-- sent to the server.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/monitoring-file-gateway.html#get-notification Getting notified about file operations>
-- in the /AWS Storage Gateway User Guide/.
module Network.AWS.StorageGateway.RefreshCache
  ( -- * Creating a Request
    RefreshCache (..),
    newRefreshCache,

    -- * Request Lenses
    refreshCache_recursive,
    refreshCache_folderList,
    refreshCache_fileShareARN,

    -- * Destructuring the Response
    RefreshCacheResponse (..),
    newRefreshCacheResponse,

    -- * Response Lenses
    refreshCacheResponse_fileShareARN,
    refreshCacheResponse_notificationId,
    refreshCacheResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | RefreshCacheInput
--
-- /See:/ 'newRefreshCache' smart constructor.
data RefreshCache = RefreshCache'
  { -- | A value that specifies whether to recursively refresh folders in the
    -- cache. The refresh includes folders that were in the cache the last time
    -- the gateway listed the folder\'s contents. If this value set to @true@,
    -- each folder that is listed in @FolderList@ is recursively updated.
    -- Otherwise, subfolders listed in @FolderList@ are not refreshed. Only
    -- objects that are in folders listed directly under @FolderList@ are found
    -- and used for the update. The default is @true@.
    --
    -- Valid Values: @true@ | @false@
    recursive :: Prelude.Maybe Prelude.Bool,
    -- | A comma-separated list of the paths of folders to refresh in the cache.
    -- The default is [@\"\/\"@]. The default refreshes objects and folders at
    -- the root of the Amazon S3 bucket. If @Recursive@ is set to @true@, the
    -- entire S3 bucket that the file share has access to is refreshed.
    folderList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the file share you want to refresh.
    fileShareARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recursive', 'refreshCache_recursive' - A value that specifies whether to recursively refresh folders in the
-- cache. The refresh includes folders that were in the cache the last time
-- the gateway listed the folder\'s contents. If this value set to @true@,
-- each folder that is listed in @FolderList@ is recursively updated.
-- Otherwise, subfolders listed in @FolderList@ are not refreshed. Only
-- objects that are in folders listed directly under @FolderList@ are found
-- and used for the update. The default is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'folderList', 'refreshCache_folderList' - A comma-separated list of the paths of folders to refresh in the cache.
-- The default is [@\"\/\"@]. The default refreshes objects and folders at
-- the root of the Amazon S3 bucket. If @Recursive@ is set to @true@, the
-- entire S3 bucket that the file share has access to is refreshed.
--
-- 'fileShareARN', 'refreshCache_fileShareARN' - The Amazon Resource Name (ARN) of the file share you want to refresh.
newRefreshCache ::
  -- | 'fileShareARN'
  Prelude.Text ->
  RefreshCache
newRefreshCache pFileShareARN_ =
  RefreshCache'
    { recursive = Prelude.Nothing,
      folderList = Prelude.Nothing,
      fileShareARN = pFileShareARN_
    }

-- | A value that specifies whether to recursively refresh folders in the
-- cache. The refresh includes folders that were in the cache the last time
-- the gateway listed the folder\'s contents. If this value set to @true@,
-- each folder that is listed in @FolderList@ is recursively updated.
-- Otherwise, subfolders listed in @FolderList@ are not refreshed. Only
-- objects that are in folders listed directly under @FolderList@ are found
-- and used for the update. The default is @true@.
--
-- Valid Values: @true@ | @false@
refreshCache_recursive :: Lens.Lens' RefreshCache (Prelude.Maybe Prelude.Bool)
refreshCache_recursive = Lens.lens (\RefreshCache' {recursive} -> recursive) (\s@RefreshCache' {} a -> s {recursive = a} :: RefreshCache)

-- | A comma-separated list of the paths of folders to refresh in the cache.
-- The default is [@\"\/\"@]. The default refreshes objects and folders at
-- the root of the Amazon S3 bucket. If @Recursive@ is set to @true@, the
-- entire S3 bucket that the file share has access to is refreshed.
refreshCache_folderList :: Lens.Lens' RefreshCache (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
refreshCache_folderList = Lens.lens (\RefreshCache' {folderList} -> folderList) (\s@RefreshCache' {} a -> s {folderList = a} :: RefreshCache) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the file share you want to refresh.
refreshCache_fileShareARN :: Lens.Lens' RefreshCache Prelude.Text
refreshCache_fileShareARN = Lens.lens (\RefreshCache' {fileShareARN} -> fileShareARN) (\s@RefreshCache' {} a -> s {fileShareARN = a} :: RefreshCache)

instance Core.AWSRequest RefreshCache where
  type AWSResponse RefreshCache = RefreshCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RefreshCacheResponse'
            Prelude.<$> (x Core..?> "FileShareARN")
            Prelude.<*> (x Core..?> "NotificationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RefreshCache

instance Prelude.NFData RefreshCache

instance Core.ToHeaders RefreshCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.RefreshCache" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RefreshCache where
  toJSON RefreshCache' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Recursive" Core..=) Prelude.<$> recursive,
            ("FolderList" Core..=) Prelude.<$> folderList,
            Prelude.Just ("FileShareARN" Core..= fileShareARN)
          ]
      )

instance Core.ToPath RefreshCache where
  toPath = Prelude.const "/"

instance Core.ToQuery RefreshCache where
  toQuery = Prelude.const Prelude.mempty

-- | RefreshCacheOutput
--
-- /See:/ 'newRefreshCacheResponse' smart constructor.
data RefreshCacheResponse = RefreshCacheResponse'
  { fileShareARN :: Prelude.Maybe Prelude.Text,
    notificationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'refreshCacheResponse_fileShareARN' - Undocumented member.
--
-- 'notificationId', 'refreshCacheResponse_notificationId' - Undocumented member.
--
-- 'httpStatus', 'refreshCacheResponse_httpStatus' - The response's http status code.
newRefreshCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RefreshCacheResponse
newRefreshCacheResponse pHttpStatus_ =
  RefreshCacheResponse'
    { fileShareARN =
        Prelude.Nothing,
      notificationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
refreshCacheResponse_fileShareARN :: Lens.Lens' RefreshCacheResponse (Prelude.Maybe Prelude.Text)
refreshCacheResponse_fileShareARN = Lens.lens (\RefreshCacheResponse' {fileShareARN} -> fileShareARN) (\s@RefreshCacheResponse' {} a -> s {fileShareARN = a} :: RefreshCacheResponse)

-- | Undocumented member.
refreshCacheResponse_notificationId :: Lens.Lens' RefreshCacheResponse (Prelude.Maybe Prelude.Text)
refreshCacheResponse_notificationId = Lens.lens (\RefreshCacheResponse' {notificationId} -> notificationId) (\s@RefreshCacheResponse' {} a -> s {notificationId = a} :: RefreshCacheResponse)

-- | The response's http status code.
refreshCacheResponse_httpStatus :: Lens.Lens' RefreshCacheResponse Prelude.Int
refreshCacheResponse_httpStatus = Lens.lens (\RefreshCacheResponse' {httpStatus} -> httpStatus) (\s@RefreshCacheResponse' {} a -> s {httpStatus = a} :: RefreshCacheResponse)

instance Prelude.NFData RefreshCacheResponse
