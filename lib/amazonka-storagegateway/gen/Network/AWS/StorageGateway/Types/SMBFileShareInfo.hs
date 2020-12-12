{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.SMBFileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.SMBFileShareInfo
  ( SMBFileShareInfo (..),

    -- * Smart constructor
    mkSMBFileShareInfo,

    -- * Lenses
    smbfsiAccessBasedEnumeration,
    smbfsiAdminUserList,
    smbfsiAuditDestinationARN,
    smbfsiFileShareStatus,
    smbfsiInvalidUserList,
    smbfsiKMSKey,
    smbfsiValidUserList,
    smbfsiGatewayARN,
    smbfsiPath,
    smbfsiAuthentication,
    smbfsiCacheAttributes,
    smbfsiObjectACL,
    smbfsiKMSEncrypted,
    smbfsiFileShareId,
    smbfsiFileShareARN,
    smbfsiDefaultStorageClass,
    smbfsiFileShareName,
    smbfsiRole,
    smbfsiSMBACLEnabled,
    smbfsiNotificationPolicy,
    smbfsiRequesterPays,
    smbfsiLocationARN,
    smbfsiGuessMIMETypeEnabled,
    smbfsiReadOnly,
    smbfsiCaseSensitivity,
    smbfsiTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.CaseSensitivity
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.Tag

-- | The Windows file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported for file gateways.
--
-- /See:/ 'mkSMBFileShareInfo' smart constructor.
data SMBFileShareInfo = SMBFileShareInfo'
  { accessBasedEnumeration ::
      Lude.Maybe Lude.Bool,
    adminUserList :: Lude.Maybe [Lude.Text],
    auditDestinationARN :: Lude.Maybe Lude.Text,
    fileShareStatus :: Lude.Maybe Lude.Text,
    invalidUserList :: Lude.Maybe [Lude.Text],
    kmsKey :: Lude.Maybe Lude.Text,
    validUserList :: Lude.Maybe [Lude.Text],
    gatewayARN :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    authentication :: Lude.Maybe Lude.Text,
    cacheAttributes :: Lude.Maybe CacheAttributes,
    objectACL :: Lude.Maybe ObjectACL,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    fileShareId :: Lude.Maybe Lude.Text,
    fileShareARN :: Lude.Maybe Lude.Text,
    defaultStorageClass :: Lude.Maybe Lude.Text,
    fileShareName :: Lude.Maybe Lude.Text,
    role' :: Lude.Maybe Lude.Text,
    sMBACLEnabled :: Lude.Maybe Lude.Bool,
    notificationPolicy :: Lude.Maybe Lude.Text,
    requesterPays :: Lude.Maybe Lude.Bool,
    locationARN :: Lude.Maybe Lude.Text,
    guessMIMETypeEnabled :: Lude.Maybe Lude.Bool,
    readOnly :: Lude.Maybe Lude.Bool,
    caseSensitivity :: Lude.Maybe CaseSensitivity,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SMBFileShareInfo' with the minimum fields required to make a request.
--
-- * 'accessBasedEnumeration' - Indicates whether @AccessBasedEnumeration@ is enabled.
-- * 'adminUserList' - A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
-- * 'auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
-- * 'authentication' - Undocumented field.
-- * 'cacheAttributes' - Refresh cache information.
-- * 'caseSensitivity' - The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
-- * 'defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
-- * 'fileShareARN' - Undocumented field.
-- * 'fileShareId' - Undocumented field.
-- * 'fileShareName' - The name of the file share. Optional.
-- * 'fileShareStatus' - Undocumented field.
-- * 'gatewayARN' - Undocumented field.
-- * 'guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
-- * 'invalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - Undocumented field.
-- * 'locationARN' - Undocumented field.
-- * 'notificationPolicy' - The notification policy of the file share.
-- * 'objectACL' - Undocumented field.
-- * 'path' - The file share path used by the SMB client to identify the mount point.
-- * 'readOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'requesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
-- * 'role'' - Undocumented field.
-- * 'sMBACLEnabled' - If this value is set to @true@ , it indicates that access control list (ACL) is enabled on the SMB file share. If it is set to @false@ , it indicates that file and directory permissions are mapped to the POSIX permission.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- * 'tags' - A list of up to 50 tags assigned to the SMB file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
-- * 'validUserList' - A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
mkSMBFileShareInfo ::
  SMBFileShareInfo
mkSMBFileShareInfo =
  SMBFileShareInfo'
    { accessBasedEnumeration = Lude.Nothing,
      adminUserList = Lude.Nothing,
      auditDestinationARN = Lude.Nothing,
      fileShareStatus = Lude.Nothing,
      invalidUserList = Lude.Nothing,
      kmsKey = Lude.Nothing,
      validUserList = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      path = Lude.Nothing,
      authentication = Lude.Nothing,
      cacheAttributes = Lude.Nothing,
      objectACL = Lude.Nothing,
      kmsEncrypted = Lude.Nothing,
      fileShareId = Lude.Nothing,
      fileShareARN = Lude.Nothing,
      defaultStorageClass = Lude.Nothing,
      fileShareName = Lude.Nothing,
      role' = Lude.Nothing,
      sMBACLEnabled = Lude.Nothing,
      notificationPolicy = Lude.Nothing,
      requesterPays = Lude.Nothing,
      locationARN = Lude.Nothing,
      guessMIMETypeEnabled = Lude.Nothing,
      readOnly = Lude.Nothing,
      caseSensitivity = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Indicates whether @AccessBasedEnumeration@ is enabled.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAccessBasedEnumeration :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Bool)
smbfsiAccessBasedEnumeration = Lens.lens (accessBasedEnumeration :: SMBFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {accessBasedEnumeration = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiAccessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead." #-}

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAdminUserList :: Lens.Lens' SMBFileShareInfo (Lude.Maybe [Lude.Text])
smbfsiAdminUserList = Lens.lens (adminUserList :: SMBFileShareInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {adminUserList = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiAdminUserList "Use generic-lens or generic-optics with 'adminUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAuditDestinationARN :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiAuditDestinationARN = Lens.lens (auditDestinationARN :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {auditDestinationARN = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiAuditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareStatus :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiFileShareStatus = Lens.lens (fileShareStatus :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareStatus = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiFileShareStatus "Use generic-lens or generic-optics with 'fileShareStatus' instead." #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiInvalidUserList :: Lens.Lens' SMBFileShareInfo (Lude.Maybe [Lude.Text])
smbfsiInvalidUserList = Lens.lens (invalidUserList :: SMBFileShareInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {invalidUserList = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiInvalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiKMSKey :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiKMSKey = Lens.lens (kmsKey :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiValidUserList :: Lens.Lens' SMBFileShareInfo (Lude.Maybe [Lude.Text])
smbfsiValidUserList = Lens.lens (validUserList :: SMBFileShareInfo -> Lude.Maybe [Lude.Text]) (\s a -> s {validUserList = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiValidUserList "Use generic-lens or generic-optics with 'validUserList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiGatewayARN :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiGatewayARN = Lens.lens (gatewayARN :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The file share path used by the SMB client to identify the mount point.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiPath :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiPath = Lens.lens (path :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAuthentication :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiAuthentication = Lens.lens (authentication :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {authentication = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiCacheAttributes :: Lens.Lens' SMBFileShareInfo (Lude.Maybe CacheAttributes)
smbfsiCacheAttributes = Lens.lens (cacheAttributes :: SMBFileShareInfo -> Lude.Maybe CacheAttributes) (\s a -> s {cacheAttributes = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiObjectACL :: Lens.Lens' SMBFileShareInfo (Lude.Maybe ObjectACL)
smbfsiObjectACL = Lens.lens (objectACL :: SMBFileShareInfo -> Lude.Maybe ObjectACL) (\s a -> s {objectACL = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiKMSEncrypted :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Bool)
smbfsiKMSEncrypted = Lens.lens (kmsEncrypted :: SMBFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareId :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiFileShareId = Lens.lens (fileShareId :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareId = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiFileShareId "Use generic-lens or generic-optics with 'fileShareId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareARN :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiFileShareARN = Lens.lens (fileShareARN :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiDefaultStorageClass :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiDefaultStorageClass = Lens.lens (defaultStorageClass :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {defaultStorageClass = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareName :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiFileShareName = Lens.lens (fileShareName :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {fileShareName = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiRole :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiRole = Lens.lens (role' :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | If this value is set to @true@ , it indicates that access control list (ACL) is enabled on the SMB file share. If it is set to @false@ , it indicates that file and directory permissions are mapped to the POSIX permission.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiSMBACLEnabled :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Bool)
smbfsiSMBACLEnabled = Lens.lens (sMBACLEnabled :: SMBFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {sMBACLEnabled = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiSMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiNotificationPolicy :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiNotificationPolicy = Lens.lens (notificationPolicy :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {notificationPolicy = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiRequesterPays :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Bool)
smbfsiRequesterPays = Lens.lens (requesterPays :: SMBFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPays = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiLocationARN :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Text)
smbfsiLocationARN = Lens.lens (locationARN :: SMBFileShareInfo -> Lude.Maybe Lude.Text) (\s a -> s {locationARN = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiLocationARN "Use generic-lens or generic-optics with 'locationARN' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiGuessMIMETypeEnabled :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Bool)
smbfsiGuessMIMETypeEnabled = Lens.lens (guessMIMETypeEnabled :: SMBFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {guessMIMETypeEnabled = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiReadOnly :: Lens.Lens' SMBFileShareInfo (Lude.Maybe Lude.Bool)
smbfsiReadOnly = Lens.lens (readOnly :: SMBFileShareInfo -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiCaseSensitivity :: Lens.Lens' SMBFileShareInfo (Lude.Maybe CaseSensitivity)
smbfsiCaseSensitivity = Lens.lens (caseSensitivity :: SMBFileShareInfo -> Lude.Maybe CaseSensitivity) (\s a -> s {caseSensitivity = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiCaseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead." #-}

-- | A list of up to 50 tags assigned to the SMB file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiTags :: Lens.Lens' SMBFileShareInfo (Lude.Maybe [Tag])
smbfsiTags = Lens.lens (tags :: SMBFileShareInfo -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SMBFileShareInfo)
{-# DEPRECATED smbfsiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON SMBFileShareInfo where
  parseJSON =
    Lude.withObject
      "SMBFileShareInfo"
      ( \x ->
          SMBFileShareInfo'
            Lude.<$> (x Lude..:? "AccessBasedEnumeration")
            Lude.<*> (x Lude..:? "AdminUserList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AuditDestinationARN")
            Lude.<*> (x Lude..:? "FileShareStatus")
            Lude.<*> (x Lude..:? "InvalidUserList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "KMSKey")
            Lude.<*> (x Lude..:? "ValidUserList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "Path")
            Lude.<*> (x Lude..:? "Authentication")
            Lude.<*> (x Lude..:? "CacheAttributes")
            Lude.<*> (x Lude..:? "ObjectACL")
            Lude.<*> (x Lude..:? "KMSEncrypted")
            Lude.<*> (x Lude..:? "FileShareId")
            Lude.<*> (x Lude..:? "FileShareARN")
            Lude.<*> (x Lude..:? "DefaultStorageClass")
            Lude.<*> (x Lude..:? "FileShareName")
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "SMBACLEnabled")
            Lude.<*> (x Lude..:? "NotificationPolicy")
            Lude.<*> (x Lude..:? "RequesterPays")
            Lude.<*> (x Lude..:? "LocationARN")
            Lude.<*> (x Lude..:? "GuessMIMETypeEnabled")
            Lude.<*> (x Lude..:? "ReadOnly")
            Lude.<*> (x Lude..:? "CaseSensitivity")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
