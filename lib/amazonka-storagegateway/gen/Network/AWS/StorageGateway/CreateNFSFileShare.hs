{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateNFSFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Network File System (NFS) file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using an NFS interface. This operation is only supported for file gateways.
--
-- /Important:/ File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in the AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- File gateway does not support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.CreateNFSFileShare
  ( -- * Creating a request
    CreateNFSFileShare (..),
    mkCreateNFSFileShare,

    -- ** Request lenses
    cnfsfsKMSKey,
    cnfsfsCacheAttributes,
    cnfsfsObjectACL,
    cnfsfsKMSEncrypted,
    cnfsfsDefaultStorageClass,
    cnfsfsFileShareName,
    cnfsfsNotificationPolicy,
    cnfsfsSquash,
    cnfsfsRequesterPays,
    cnfsfsNFSFileShareDefaults,
    cnfsfsClientList,
    cnfsfsGuessMIMETypeEnabled,
    cnfsfsReadOnly,
    cnfsfsTags,
    cnfsfsClientToken,
    cnfsfsGatewayARN,
    cnfsfsRole,
    cnfsfsLocationARN,

    -- * Destructuring the response
    CreateNFSFileShareResponse (..),
    mkCreateNFSFileShareResponse,

    -- ** Response lenses
    cnfsfsrsFileShareARN,
    cnfsfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | CreateNFSFileShareInput
--
-- /See:/ 'mkCreateNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
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
    tags :: Lude.Maybe [Tag],
    clientToken :: Lude.Text,
    gatewayARN :: Lude.Text,
    role' :: Lude.Text,
    locationARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNFSFileShare' with the minimum fields required to make a request.
--
-- * 'cacheAttributes' - Refresh cache information.
-- * 'clientList' - The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
-- * 'clientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
-- * 'defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
-- * 'fileShareName' - The name of the file share. Optional.
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
-- * 'guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'locationARN' - The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
-- * 'nFSFileShareDefaults' - File share default values. Optional.
-- * 'notificationPolicy' - The notification policy of the file share.
-- * 'objectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
-- * 'readOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'requesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
-- * 'role'' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
-- * 'squash' - A value that maps a user to anonymous user.
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
-- * 'tags' - A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
mkCreateNFSFileShare ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'role''
  Lude.Text ->
  -- | 'locationARN'
  Lude.Text ->
  CreateNFSFileShare
mkCreateNFSFileShare
  pClientToken_
  pGatewayARN_
  pRole_
  pLocationARN_ =
    CreateNFSFileShare'
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
        tags = Lude.Nothing,
        clientToken = pClientToken_,
        gatewayARN = pGatewayARN_,
        role' = pRole_,
        locationARN = pLocationARN_
      }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsKMSKey :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Text)
cnfsfsKMSKey = Lens.lens (kmsKey :: CreateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsCacheAttributes :: Lens.Lens' CreateNFSFileShare (Lude.Maybe CacheAttributes)
cnfsfsCacheAttributes = Lens.lens (cacheAttributes :: CreateNFSFileShare -> Lude.Maybe CacheAttributes) (\s a -> s {cacheAttributes = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsObjectACL :: Lens.Lens' CreateNFSFileShare (Lude.Maybe ObjectACL)
cnfsfsObjectACL = Lens.lens (objectACL :: CreateNFSFileShare -> Lude.Maybe ObjectACL) (\s a -> s {objectACL = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsKMSEncrypted :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Bool)
cnfsfsKMSEncrypted = Lens.lens (kmsEncrypted :: CreateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsDefaultStorageClass :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Text)
cnfsfsDefaultStorageClass = Lens.lens (defaultStorageClass :: CreateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {defaultStorageClass = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsFileShareName :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Text)
cnfsfsFileShareName = Lens.lens (fileShareName :: CreateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {fileShareName = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsNotificationPolicy :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Text)
cnfsfsNotificationPolicy = Lens.lens (notificationPolicy :: CreateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {notificationPolicy = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that maps a user to anonymous user.
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
cnfsfsSquash :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Text)
cnfsfsSquash = Lens.lens (squash :: CreateNFSFileShare -> Lude.Maybe Lude.Text) (\s a -> s {squash = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsSquash "Use generic-lens or generic-optics with 'squash' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsRequesterPays :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Bool)
cnfsfsRequesterPays = Lens.lens (requesterPays :: CreateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPays = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | File share default values. Optional.
--
-- /Note:/ Consider using 'nFSFileShareDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsNFSFileShareDefaults :: Lens.Lens' CreateNFSFileShare (Lude.Maybe NFSFileShareDefaults)
cnfsfsNFSFileShareDefaults = Lens.lens (nFSFileShareDefaults :: CreateNFSFileShare -> Lude.Maybe NFSFileShareDefaults) (\s a -> s {nFSFileShareDefaults = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsNFSFileShareDefaults "Use generic-lens or generic-optics with 'nFSFileShareDefaults' instead." #-}

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsClientList :: Lens.Lens' CreateNFSFileShare (Lude.Maybe (Lude.NonEmpty Lude.Text))
cnfsfsClientList = Lens.lens (clientList :: CreateNFSFileShare -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {clientList = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsClientList "Use generic-lens or generic-optics with 'clientList' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsGuessMIMETypeEnabled :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Bool)
cnfsfsGuessMIMETypeEnabled = Lens.lens (guessMIMETypeEnabled :: CreateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {guessMIMETypeEnabled = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsReadOnly :: Lens.Lens' CreateNFSFileShare (Lude.Maybe Lude.Bool)
cnfsfsReadOnly = Lens.lens (readOnly :: CreateNFSFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsTags :: Lens.Lens' CreateNFSFileShare (Lude.Maybe [Tag])
cnfsfsTags = Lens.lens (tags :: CreateNFSFileShare -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsClientToken :: Lens.Lens' CreateNFSFileShare Lude.Text
cnfsfsClientToken = Lens.lens (clientToken :: CreateNFSFileShare -> Lude.Text) (\s a -> s {clientToken = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsGatewayARN :: Lens.Lens' CreateNFSFileShare Lude.Text
cnfsfsGatewayARN = Lens.lens (gatewayARN :: CreateNFSFileShare -> Lude.Text) (\s a -> s {gatewayARN = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsRole :: Lens.Lens' CreateNFSFileShare Lude.Text
cnfsfsRole = Lens.lens (role' :: CreateNFSFileShare -> Lude.Text) (\s a -> s {role' = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsLocationARN :: Lens.Lens' CreateNFSFileShare Lude.Text
cnfsfsLocationARN = Lens.lens (locationARN :: CreateNFSFileShare -> Lude.Text) (\s a -> s {locationARN = a} :: CreateNFSFileShare)
{-# DEPRECATED cnfsfsLocationARN "Use generic-lens or generic-optics with 'locationARN' instead." #-}

instance Lude.AWSRequest CreateNFSFileShare where
  type Rs CreateNFSFileShare = CreateNFSFileShareResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNFSFileShareResponse'
            Lude.<$> (x Lude..?> "FileShareARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNFSFileShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CreateNFSFileShare" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNFSFileShare where
  toJSON CreateNFSFileShare' {..} =
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
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("Role" Lude..= role'),
            Lude.Just ("LocationARN" Lude..= locationARN)
          ]
      )

instance Lude.ToPath CreateNFSFileShare where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNFSFileShare where
  toQuery = Lude.const Lude.mempty

-- | CreateNFSFileShareOutput
--
-- /See:/ 'mkCreateNFSFileShareResponse' smart constructor.
data CreateNFSFileShareResponse = CreateNFSFileShareResponse'
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

-- | Creates a value of 'CreateNFSFileShareResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
-- * 'responseStatus' - The response status code.
mkCreateNFSFileShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNFSFileShareResponse
mkCreateNFSFileShareResponse pResponseStatus_ =
  CreateNFSFileShareResponse'
    { fileShareARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsrsFileShareARN :: Lens.Lens' CreateNFSFileShareResponse (Lude.Maybe Lude.Text)
cnfsfsrsFileShareARN = Lens.lens (fileShareARN :: CreateNFSFileShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: CreateNFSFileShareResponse)
{-# DEPRECATED cnfsfsrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsrsResponseStatus :: Lens.Lens' CreateNFSFileShareResponse Lude.Int
cnfsfsrsResponseStatus = Lens.lens (responseStatus :: CreateNFSFileShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNFSFileShareResponse)
{-# DEPRECATED cnfsfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
