{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateNFSFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Network File System (NFS) file share. This operation is only supported in the file gateway type.
--
-- Updates the following file share settings:
--
--     * Default storage class for your S3 bucket
--
--
--     * Metadata defaults for your S3 bucket
--
--
--     * Allowed NFS clients for your file share
--
--
--     * Squash settings
--
--
--     * Write status of your file share
module Network.AWS.StorageGateway.UpdateNFSFileShare
  ( -- * Creating a request
    UpdateNFSFileShare (..),
    mkUpdateNFSFileShare,

    -- ** Request lenses
    unfsfsFileShareARN,
    unfsfsCacheAttributes,
    unfsfsClientList,
    unfsfsDefaultStorageClass,
    unfsfsFileShareName,
    unfsfsGuessMIMETypeEnabled,
    unfsfsKMSEncrypted,
    unfsfsKMSKey,
    unfsfsNFSFileShareDefaults,
    unfsfsNotificationPolicy,
    unfsfsObjectACL,
    unfsfsReadOnly,
    unfsfsRequesterPays,
    unfsfsSquash,

    -- * Destructuring the response
    UpdateNFSFileShareResponse (..),
    mkUpdateNFSFileShareResponse,

    -- ** Response lenses
    unfsfsrrsFileShareARN,
    unfsfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | UpdateNFSFileShareInput
--
-- /See:/ 'mkUpdateNFSFileShare' smart constructor.
data UpdateNFSFileShare = UpdateNFSFileShare'
  { -- | The Amazon Resource Name (ARN) of the file share to be updated.
    fileShareARN :: Types.FileShareARN,
    -- | Refresh cache information.
    cacheAttributes :: Core.Maybe Types.CacheAttributes,
    -- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
    clientList :: Core.Maybe (Core.NonEmpty Types.IPV4AddressCIDR),
    -- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Core.Maybe Types.DefaultStorageClass,
    -- | The name of the file share. Optional.
    fileShareName :: Core.Maybe Types.FileShareName,
    -- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Core.Maybe Core.Bool,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kMSEncrypted :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
    kMSKey :: Core.Maybe Types.KMSKey,
    -- | The default values for the file share. Optional.
    nFSFileShareDefaults :: Core.Maybe Types.NFSFileShareDefaults,
    -- | The notification policy of the file share.
    notificationPolicy :: Core.Maybe Types.NotificationPolicy,
    -- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
    objectACL :: Core.Maybe Types.ObjectACL,
    -- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Core.Maybe Core.Bool,
    -- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
    --
    -- Valid Values: @true@ | @false@
    requesterPays :: Core.Maybe Core.Bool,
    -- | The user mapped to anonymous user.
    --
    -- Valid values are the following:
    --
    --     * @RootSquash@ : Only root is mapped to anonymous user.
    --
    --
    --     * @NoSquash@ : No one is mapped to anonymous user.
    --
    --
    --     * @AllSquash@ : Everyone is mapped to anonymous user.
    squash :: Core.Maybe Types.Squash
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNFSFileShare' value with any optional fields omitted.
mkUpdateNFSFileShare ::
  -- | 'fileShareARN'
  Types.FileShareARN ->
  UpdateNFSFileShare
mkUpdateNFSFileShare fileShareARN =
  UpdateNFSFileShare'
    { fileShareARN,
      cacheAttributes = Core.Nothing,
      clientList = Core.Nothing,
      defaultStorageClass = Core.Nothing,
      fileShareName = Core.Nothing,
      guessMIMETypeEnabled = Core.Nothing,
      kMSEncrypted = Core.Nothing,
      kMSKey = Core.Nothing,
      nFSFileShareDefaults = Core.Nothing,
      notificationPolicy = Core.Nothing,
      objectACL = Core.Nothing,
      readOnly = Core.Nothing,
      requesterPays = Core.Nothing,
      squash = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the file share to be updated.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsFileShareARN :: Lens.Lens' UpdateNFSFileShare Types.FileShareARN
unfsfsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED unfsfsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsCacheAttributes :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.CacheAttributes)
unfsfsCacheAttributes = Lens.field @"cacheAttributes"
{-# DEPRECATED unfsfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsClientList :: Lens.Lens' UpdateNFSFileShare (Core.Maybe (Core.NonEmpty Types.IPV4AddressCIDR))
unfsfsClientList = Lens.field @"clientList"
{-# DEPRECATED unfsfsClientList "Use generic-lens or generic-optics with 'clientList' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsDefaultStorageClass :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.DefaultStorageClass)
unfsfsDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# DEPRECATED unfsfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsFileShareName :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.FileShareName)
unfsfsFileShareName = Lens.field @"fileShareName"
{-# DEPRECATED unfsfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsGuessMIMETypeEnabled :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Core.Bool)
unfsfsGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# DEPRECATED unfsfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsKMSEncrypted :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Core.Bool)
unfsfsKMSEncrypted = Lens.field @"kMSEncrypted"
{-# DEPRECATED unfsfsKMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsKMSKey :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.KMSKey)
unfsfsKMSKey = Lens.field @"kMSKey"
{-# DEPRECATED unfsfsKMSKey "Use generic-lens or generic-optics with 'kMSKey' instead." #-}

-- | The default values for the file share. Optional.
--
-- /Note:/ Consider using 'nFSFileShareDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsNFSFileShareDefaults :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.NFSFileShareDefaults)
unfsfsNFSFileShareDefaults = Lens.field @"nFSFileShareDefaults"
{-# DEPRECATED unfsfsNFSFileShareDefaults "Use generic-lens or generic-optics with 'nFSFileShareDefaults' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsNotificationPolicy :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.NotificationPolicy)
unfsfsNotificationPolicy = Lens.field @"notificationPolicy"
{-# DEPRECATED unfsfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsObjectACL :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.ObjectACL)
unfsfsObjectACL = Lens.field @"objectACL"
{-# DEPRECATED unfsfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsReadOnly :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Core.Bool)
unfsfsReadOnly = Lens.field @"readOnly"
{-# DEPRECATED unfsfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsRequesterPays :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Core.Bool)
unfsfsRequesterPays = Lens.field @"requesterPays"
{-# DEPRECATED unfsfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | The user mapped to anonymous user.
--
-- Valid values are the following:
--
--     * @RootSquash@ : Only root is mapped to anonymous user.
--
--
--     * @NoSquash@ : No one is mapped to anonymous user.
--
--
--     * @AllSquash@ : Everyone is mapped to anonymous user.
--
--
--
-- /Note:/ Consider using 'squash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsSquash :: Lens.Lens' UpdateNFSFileShare (Core.Maybe Types.Squash)
unfsfsSquash = Lens.field @"squash"
{-# DEPRECATED unfsfsSquash "Use generic-lens or generic-optics with 'squash' instead." #-}

instance Core.FromJSON UpdateNFSFileShare where
  toJSON UpdateNFSFileShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FileShareARN" Core..= fileShareARN),
            ("CacheAttributes" Core..=) Core.<$> cacheAttributes,
            ("ClientList" Core..=) Core.<$> clientList,
            ("DefaultStorageClass" Core..=) Core.<$> defaultStorageClass,
            ("FileShareName" Core..=) Core.<$> fileShareName,
            ("GuessMIMETypeEnabled" Core..=) Core.<$> guessMIMETypeEnabled,
            ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
            ("KMSKey" Core..=) Core.<$> kMSKey,
            ("NFSFileShareDefaults" Core..=) Core.<$> nFSFileShareDefaults,
            ("NotificationPolicy" Core..=) Core.<$> notificationPolicy,
            ("ObjectACL" Core..=) Core.<$> objectACL,
            ("ReadOnly" Core..=) Core.<$> readOnly,
            ("RequesterPays" Core..=) Core.<$> requesterPays,
            ("Squash" Core..=) Core.<$> squash
          ]
      )

instance Core.AWSRequest UpdateNFSFileShare where
  type Rs UpdateNFSFileShare = UpdateNFSFileShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.UpdateNFSFileShare")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNFSFileShareResponse'
            Core.<$> (x Core..:? "FileShareARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | UpdateNFSFileShareOutput
--
-- /See:/ 'mkUpdateNFSFileShareResponse' smart constructor.
data UpdateNFSFileShareResponse = UpdateNFSFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the updated file share.
    fileShareARN :: Core.Maybe Types.FileShareARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateNFSFileShareResponse' value with any optional fields omitted.
mkUpdateNFSFileShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateNFSFileShareResponse
mkUpdateNFSFileShareResponse responseStatus =
  UpdateNFSFileShareResponse'
    { fileShareARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the updated file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsrrsFileShareARN :: Lens.Lens' UpdateNFSFileShareResponse (Core.Maybe Types.FileShareARN)
unfsfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED unfsfsrrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsrrsResponseStatus :: Lens.Lens' UpdateNFSFileShareResponse Core.Int
unfsfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED unfsfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
