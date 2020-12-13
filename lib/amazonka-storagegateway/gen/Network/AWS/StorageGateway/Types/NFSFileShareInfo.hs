{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NFSFileShareInfo
  ( NFSFileShareInfo (..),

    -- * Smart constructor
    mkNFSFileShareInfo,

    -- * Lenses
    nfsfsiFileShareStatus,
    nfsfsiKMSKey,
    nfsfsiGatewayARN,
    nfsfsiPath,
    nfsfsiCacheAttributes,
    nfsfsiObjectACL,
    nfsfsiKMSEncrypted,
    nfsfsiFileShareId,
    nfsfsiFileShareARN,
    nfsfsiDefaultStorageClass,
    nfsfsiFileShareName,
    nfsfsiRole,
    nfsfsiNotificationPolicy,
    nfsfsiSquash,
    nfsfsiRequesterPays,
    nfsfsiNFSFileShareDefaults,
    nfsfsiLocationARN,
    nfsfsiClientList,
    nfsfsiGuessMIMETypeEnabled,
    nfsfsiReadOnly,
    nfsfsiTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.Tag

-- | The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.
--
-- /See:/ 'mkNFSFileShareInfo' smart constructor.
data NFSFileShareInfo = NFSFileShareInfo'
  { fileShareStatus :: Lude.Maybe Lude.Text,
    kmsKey :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    -- | Refresh cache information.
    cacheAttributes :: Lude.Maybe CacheAttributes,
    objectACL :: Lude.Maybe ObjectACL,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    fileShareId :: Lude.Maybe Lude.Text,
    fileShareARN :: Lude.Maybe Lude.Text,
    -- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Lude.Maybe Lude.Text,
    -- | The name of the file share. Optional.
    fileShareName :: Lude.Maybe Lude.Text,
    role' :: Lude.Maybe Lude.Text,
    -- | The notification policy of the file share.
    notificationPolicy :: Lude.Maybe Lude.Text,
    squash :: Lude.Maybe Lude.Text,
    -- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
    --
    -- Valid Values: @true@ | @false@
    requesterPays :: Lude.Maybe Lude.Bool,
    nFSFileShareDefaults :: Lude.Maybe NFSFileShareDefaults,
    locationARN :: Lude.Maybe Lude.Text,
    clientList :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Lude.Maybe Lude.Bool,
    -- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Lude.Maybe Lude.Bool,
    -- | A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NFSFileShareInfo' with the minimum fields required to make a request.
--
-- * 'fileShareStatus' -
-- * 'kmsKey' -
-- * 'gatewayARN' -
-- * 'path' -
-- * 'cacheAttributes' - Refresh cache information.
-- * 'objectACL' -
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'fileShareId' -
-- * 'fileShareARN' -
-- * 'defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
-- * 'fileShareName' - The name of the file share. Optional.
-- * 'role'' -
-- * 'notificationPolicy' - The notification policy of the file share.
-- * 'squash' -
-- * 'requesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
-- * 'nFSFileShareDefaults' -
-- * 'locationARN' -
-- * 'clientList' -
-- * 'guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
-- * 'readOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'tags' - A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
mkNFSFileShareInfo ::
  NFSFileShareInfo
mkNFSFileShareInfo =
  NFSFileShareInfo'
    { fileShareStatus = Lude.Nothing,
      kmsKey = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      path = Lude.Nothing,
      cacheAttributes = Lude.Nothing,
      objectACL = Lude.Nothing,
      kmsEncrypted = Lude.Nothing,
      fileShareId = Lude.Nothing,
      fileShareARN = Lude.Nothing,
      defaultStorageClass = Lude.Nothing,
      fileShareName = Lude.Nothing,
      role' = Lude.Nothing,
      notificationPolicy = Lude.Nothing,
      squash = Lude.Nothing,
      requesterPays = Lude.Nothing,
      nFSFileShareDefaults = Lude.Nothing,
      locationARN = Lude.Nothing,
      clientList = Lude.Nothing,
      guessMIMETypeEnabled = Lude.Nothing,
      readOnly = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareStatus :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiFileShareStatus = Lens.lens (fileShareStatus :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareStatus = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiFileShareStatus "Use generic-lens or generic-optics with 'fileShareStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiKMSKey :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiKMSKey = Lens.lens (kmsKey :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiGatewayARN :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiGatewayARN = Lens.lens (gatewayARN :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiPath :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiPath = Lens.lens (path :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiCacheAttributes :: Lens.Lens' NFSFileShareInfo (Lude.Maybe CacheAttributes)
nfsfsiCacheAttributes = Lens.lens (cacheAttributes :: NFSFileShareInfo -> Lude.Maybe CacheAttributes) (\s a -> s {cacheAttributes = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiObjectACL :: Lens.Lens' NFSFileShareInfo (Lude.Maybe ObjectACL)
nfsfsiObjectACL = Lens.lens (objectACL :: NFSFileShareInfo -> Lude.Maybe ObjectACL) (\s a -> s {objectACL = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiKMSEncrypted :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Bool)
nfsfsiKMSEncrypted = Lens.lens (kmsEncrypted :: NFSFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareId :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiFileShareId = Lens.lens (fileShareId :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareId = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiFileShareId "Use generic-lens or generic-optics with 'fileShareId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareARN :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiFileShareARN = Lens.lens (fileShareARN :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiDefaultStorageClass :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiDefaultStorageClass = Lens.lens (defaultStorageClass :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {defaultStorageClass = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareName :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiFileShareName = Lens.lens (fileShareName :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareName = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiRole :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiRole = Lens.lens (role' :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiNotificationPolicy :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiNotificationPolicy = Lens.lens (notificationPolicy :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {notificationPolicy = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'squash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiSquash :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiSquash = Lens.lens (squash :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {squash = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiSquash "Use generic-lens or generic-optics with 'squash' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiRequesterPays :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Bool)
nfsfsiRequesterPays = Lens.lens (requesterPays :: NFSFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPays = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nFSFileShareDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiNFSFileShareDefaults :: Lens.Lens' NFSFileShareInfo (Lude.Maybe NFSFileShareDefaults)
nfsfsiNFSFileShareDefaults = Lens.lens (nFSFileShareDefaults :: NFSFileShareInfo -> Lude.Maybe NFSFileShareDefaults) (\s a -> s {nFSFileShareDefaults = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiNFSFileShareDefaults "Use generic-lens or generic-optics with 'nFSFileShareDefaults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiLocationARN :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Text)
nfsfsiLocationARN = Lens.lens (locationARN :: NFSFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {locationARN = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiLocationARN "Use generic-lens or generic-optics with 'locationARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiClientList :: Lens.Lens' NFSFileShareInfo (Lude.Maybe (Lude.NonEmpty Lude.Text))
nfsfsiClientList = Lens.lens (clientList :: NFSFileShareInfo -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {clientList = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiClientList "Use generic-lens or generic-optics with 'clientList' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiGuessMIMETypeEnabled :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Bool)
nfsfsiGuessMIMETypeEnabled = Lens.lens (guessMIMETypeEnabled :: NFSFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {guessMIMETypeEnabled = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiReadOnly :: Lens.Lens' NFSFileShareInfo (Lude.Maybe Lude.Bool)
nfsfsiReadOnly = Lens.lens (readOnly :: NFSFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiTags :: Lens.Lens' NFSFileShareInfo (Lude.Maybe [Tag])
nfsfsiTags = Lens.lens (tags :: NFSFileShareInfo -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: NFSFileShareInfo)
{-# DEPRECATED nfsfsiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON NFSFileShareInfo where
  parseJSON =
    Lude.withObject
      "NFSFileShareInfo"
      ( \x ->
          NFSFileShareInfo'
            Lude.<$> (x Lude..:? "FileShareStatus")
            Lude.<*> (x Lude..:? "KMSKey")
            Lude.<*> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "CacheAttributes")
            Lude.<*> (x Lude..:? "ObjectACL")
            Lude.<*> (x Lude..:? "KMSEncrypted")
            Lude.<*> (x Lude..:? "FileShareId")
            Lude.<*> (x Lude..:? "FileShareARN")
            Lude.<*> (x Lude..:? "DefaultStorageClass")
            Lude.<*> (x Lude..:? "FileShareName")
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "NotificationPolicy")
            Lude.<*> (x Lude..:? "Squash")
            Lude.<*> (x Lude..:? "RequesterPays")
            Lude.<*> (x Lude..:? "NFSFileShareDefaults")
            Lude.<*> (x Lude..:? "LocationARN")
            Lude.<*> (x Lude..:? "ClientList")
            Lude.<*> (x Lude..:? "GuessMIMETypeEnabled")
            Lude.<*> (x Lude..:? "ReadOnly")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
