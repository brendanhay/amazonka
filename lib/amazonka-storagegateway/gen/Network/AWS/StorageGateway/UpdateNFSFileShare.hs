{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    unfsfsKMSKey,
    unfsfsCacheAttributes,
    unfsfsObjectACL,
    unfsfsKMSEncrypted,
    unfsfsDefaultStorageClass,
    unfsfsFileShareName,
    unfsfsNotificationPolicy,
    unfsfsSquash,
    unfsfsRequesterPays,
    unfsfsNFSFileShareDefaults,
    unfsfsClientList,
    unfsfsGuessMIMETypeEnabled,
    unfsfsReadOnly,
    unfsfsFileShareARN,

    -- * Destructuring the response
    UpdateNFSFileShareResponse (..),
    mkUpdateNFSFileShareResponse,

    -- ** Response lenses
    unfsfsrsFileShareARN,
    unfsfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | UpdateNFSFileShareInput
--
-- /See:/ 'mkUpdateNFSFileShare' smart constructor.
data UpdateNFSFileShare = UpdateNFSFileShare'
  { kmsKey ::
      Lude.Maybe Lude.Text,
    cacheAttributes :: Lude.Maybe CacheAttributes,
    objectACL :: Lude.Maybe ObjectACL,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    defaultStorageClass :: Lude.Maybe Lude.Text,
    fileShareName :: Lude.Maybe Lude.Text,
    notificationPolicy :: Lude.Maybe Lude.Text,
    squash :: Lude.Maybe Lude.Text,
    requesterPays :: Lude.Maybe Lude.Bool,
    nFSFileShareDefaults ::
      Lude.Maybe NFSFileShareDefaults,
    clientList :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    guessMIMETypeEnabled :: Lude.Maybe Lude.Bool,
    readOnly :: Lude.Maybe Lude.Bool,
    fileShareARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNFSFileShare' with the minimum fields required to make a request.
--
-- * 'cacheAttributes' - Refresh cache information.
-- * 'clientList' - The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
-- * 'defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the file share to be updated.
-- * 'fileShareName' - The name of the file share. Optional.
-- * 'guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'nFSFileShareDefaults' - The default values for the file share. Optional.
-- * 'notificationPolicy' - The notification policy of the file share.
-- * 'objectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
-- * 'readOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'requesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
-- * 'squash' - The user mapped to anonymous user.
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
mkUpdateNFSFileShare ::
  -- | 'fileShareARN'
  Lude.Text ->
  UpdateNFSFileShare
mkUpdateNFSFileShare pFileShareARN_ =
  UpdateNFSFileShare'
    { kmsKey = Lude.Nothing,
      cacheAttributes = Lude.Nothing,
      objectACL = Lude.Nothing,
      kmsEncrypted = Lude.Nothing,
      defaultStorageClass = Lude.Nothing,
      fileShareName = Lude.Nothing,
      notificationPolicy = Lude.Nothing,
      squash = Lude.Nothing,
      requesterPays = Lude.Nothing,
      nFSFileShareDefaults = Lude.Nothing,
      clientList = Lude.Nothing,
      guessMIMETypeEnabled = Lude.Nothing,
      readOnly = Lude.Nothing,
      fileShareARN = pFileShareARN_
    }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsKMSKey :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Text)
unfsfsKMSKey = Lens.lens (kmsKey :: UpdateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsCacheAttributes :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe CacheAttributes)
unfsfsCacheAttributes = Lens.lens (cacheAttributes :: UpdateNFSFileShare -> Lude.Maybe CacheAttributes) (\s a -> s {cacheAttributes = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsObjectACL :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe ObjectACL)
unfsfsObjectACL = Lens.lens (objectACL :: UpdateNFSFileShare -> Lude.Maybe ObjectACL) (\s a -> s {objectACL = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsKMSEncrypted :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Bool)
unfsfsKMSEncrypted = Lens.lens (kmsEncrypted :: UpdateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsDefaultStorageClass :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Text)
unfsfsDefaultStorageClass = Lens.lens (defaultStorageClass :: UpdateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {defaultStorageClass = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsFileShareName :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Text)
unfsfsFileShareName = Lens.lens (fileShareName :: UpdateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {fileShareName = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsNotificationPolicy :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Text)
unfsfsNotificationPolicy = Lens.lens (notificationPolicy :: UpdateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {notificationPolicy = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

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
unfsfsSquash :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Text)
unfsfsSquash = Lens.lens (squash :: UpdateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {squash = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsSquash "Use generic-lens or generic-optics with 'squash' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsRequesterPays :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Bool)
unfsfsRequesterPays = Lens.lens (requesterPays :: UpdateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPays = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | The default values for the file share. Optional.
--
-- /Note:/ Consider using 'nFSFileShareDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsNFSFileShareDefaults :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe NFSFileShareDefaults)
unfsfsNFSFileShareDefaults = Lens.lens (nFSFileShareDefaults :: UpdateNFSFileShare -> Lude.Maybe NFSFileShareDefaults) (\s a -> s {nFSFileShareDefaults = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsNFSFileShareDefaults "Use generic-lens or generic-optics with 'nFSFileShareDefaults' instead." #-}

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsClientList :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe (Lude.NonEmpty Lude.Text))
unfsfsClientList = Lens.lens (clientList :: UpdateNFSFileShare -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {clientList = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsClientList "Use generic-lens or generic-optics with 'clientList' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsGuessMIMETypeEnabled :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Bool)
unfsfsGuessMIMETypeEnabled = Lens.lens (guessMIMETypeEnabled :: UpdateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {guessMIMETypeEnabled = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsReadOnly :: Lens.Lens' UpdateNFSFileShare (Lude.Maybe Lude.Bool)
unfsfsReadOnly = Lens.lens (readOnly :: UpdateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | The Amazon Resource Name (ARN) of the file share to be updated.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsFileShareARN :: Lens.Lens' UpdateNFSFileShare Lude.Text
unfsfsFileShareARN = Lens.lens (fileShareARN :: UpdateNFSFileShare -> Lude.Text) (\s a -> s {fileShareARN = a} :: UpdateNFSFileShare)
{-# DEPRECATED unfsfsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

instance Lude.AWSRequest UpdateNFSFileShare where
  type Rs UpdateNFSFileShare = UpdateNFSFileShareResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateNFSFileShareResponse'
            Lude.<$> (x Lude..?> "FileShareARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateNFSFileShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.UpdateNFSFileShare" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateNFSFileShare where
  toJSON UpdateNFSFileShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("CacheAttributes" Lude..=) Lude.<$> cacheAttributes,
            ("ObjectACL" Lude..=) Lude.<$> objectACL,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            ("DefaultStorageClass" Lude..=) Lude.<$> defaultStorageClass,
            ("FileShareName" Lude..=) Lude.<$> fileShareName,
            ("NotificationPolicy" Lude..=) Lude.<$> notificationPolicy,
            ("Squash" Lude..=) Lude.<$> squash,
            ("RequesterPays" Lude..=) Lude.<$> requesterPays,
            ("NFSFileShareDefaults" Lude..=) Lude.<$> nFSFileShareDefaults,
            ("ClientList" Lude..=) Lude.<$> clientList,
            ("GuessMIMETypeEnabled" Lude..=) Lude.<$> guessMIMETypeEnabled,
            ("ReadOnly" Lude..=) Lude.<$> readOnly,
            Lude.Just ("FileShareARN" Lude..= fileShareARN)
          ]
      )

instance Lude.ToPath UpdateNFSFileShare where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateNFSFileShare where
  toQuery = Lude.const Lude.mempty

-- | UpdateNFSFileShareOutput
--
-- /See:/ 'mkUpdateNFSFileShareResponse' smart constructor.
data UpdateNFSFileShareResponse = UpdateNFSFileShareResponse'
  { fileShareARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateNFSFileShareResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the updated file share.
-- * 'responseStatus' - The response status code.
mkUpdateNFSFileShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateNFSFileShareResponse
mkUpdateNFSFileShareResponse pResponseStatus_ =
  UpdateNFSFileShareResponse'
    { fileShareARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsrsFileShareARN :: Lens.Lens' UpdateNFSFileShareResponse (Lude.Maybe Lude.Text)
unfsfsrsFileShareARN = Lens.lens (fileShareARN :: UpdateNFSFileShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: UpdateNFSFileShareResponse)
{-# DEPRECATED unfsfsrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unfsfsrsResponseStatus :: Lens.Lens' UpdateNFSFileShareResponse Lude.Int
unfsfsrsResponseStatus = Lens.lens (responseStatus :: UpdateNFSFileShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateNFSFileShareResponse)
{-# DEPRECATED unfsfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
