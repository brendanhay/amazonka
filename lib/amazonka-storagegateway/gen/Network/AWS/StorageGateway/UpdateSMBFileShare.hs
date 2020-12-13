{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Server Message Block (SMB) file share. This operation is only supported for file gateways.
--
-- /Important:/ File gateways require AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure that AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in this AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- File gateways don't support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.UpdateSMBFileShare
  ( -- * Creating a request
    UpdateSMBFileShare (..),
    mkUpdateSMBFileShare,

    -- ** Request lenses
    usmbfsAccessBasedEnumeration,
    usmbfsAdminUserList,
    usmbfsAuditDestinationARN,
    usmbfsInvalidUserList,
    usmbfsKMSKey,
    usmbfsValidUserList,
    usmbfsCacheAttributes,
    usmbfsObjectACL,
    usmbfsKMSEncrypted,
    usmbfsFileShareARN,
    usmbfsDefaultStorageClass,
    usmbfsFileShareName,
    usmbfsSMBACLEnabled,
    usmbfsNotificationPolicy,
    usmbfsRequesterPays,
    usmbfsGuessMIMETypeEnabled,
    usmbfsReadOnly,
    usmbfsCaseSensitivity,

    -- * Destructuring the response
    UpdateSMBFileShareResponse (..),
    mkUpdateSMBFileShareResponse,

    -- ** Response lenses
    usmbfsrsFileShareARN,
    usmbfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | UpdateSMBFileShareInput
--
-- /See:/ 'mkUpdateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { -- | The files and folders on this share will only be visible to users with read access.
    accessBasedEnumeration :: Lude.Maybe Lude.Bool,
    -- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    adminUserList :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Lude.Maybe Lude.Text,
    -- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    invalidUserList :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
    kmsKey :: Lude.Maybe Lude.Text,
    -- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    validUserList :: Lude.Maybe [Lude.Text],
    -- | Refresh cache information.
    cacheAttributes :: Lude.Maybe CacheAttributes,
    -- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
    objectACL :: Lude.Maybe ObjectACL,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
    fileShareARN :: Lude.Text,
    -- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Lude.Maybe Lude.Text,
    -- | The name of the file share. Optional.
    fileShareName :: Lude.Maybe Lude.Text,
    -- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
    --
    -- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
    -- Valid Values: @true@ | @false@
    sMBACLEnabled :: Lude.Maybe Lude.Bool,
    -- | The notification policy of the file share.
    notificationPolicy :: Lude.Maybe Lude.Text,
    -- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
    --
    -- Valid Values: @true@ | @false@
    requesterPays :: Lude.Maybe Lude.Bool,
    -- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Lude.Maybe Lude.Bool,
    -- | A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Lude.Maybe Lude.Bool,
    -- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
    caseSensitivity :: Lude.Maybe CaseSensitivity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSMBFileShare' with the minimum fields required to make a request.
--
-- * 'accessBasedEnumeration' - The files and folders on this share will only be visible to users with read access.
-- * 'adminUserList' - A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
-- * 'auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
-- * 'invalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'validUserList' - A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
-- * 'cacheAttributes' - Refresh cache information.
-- * 'objectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the SMB file share that you want to update.
-- * 'defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
-- * 'fileShareName' - The name of the file share. Optional.
-- * 'sMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@
-- * 'notificationPolicy' - The notification policy of the file share.
-- * 'requesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
-- * 'guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
-- * 'readOnly' - A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'caseSensitivity' - The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
mkUpdateSMBFileShare ::
  -- | 'fileShareARN'
  Lude.Text ->
  UpdateSMBFileShare
mkUpdateSMBFileShare pFileShareARN_ =
  UpdateSMBFileShare'
    { accessBasedEnumeration = Lude.Nothing,
      adminUserList = Lude.Nothing,
      auditDestinationARN = Lude.Nothing,
      invalidUserList = Lude.Nothing,
      kmsKey = Lude.Nothing,
      validUserList = Lude.Nothing,
      cacheAttributes = Lude.Nothing,
      objectACL = Lude.Nothing,
      kmsEncrypted = Lude.Nothing,
      fileShareARN = pFileShareARN_,
      defaultStorageClass = Lude.Nothing,
      fileShareName = Lude.Nothing,
      sMBACLEnabled = Lude.Nothing,
      notificationPolicy = Lude.Nothing,
      requesterPays = Lude.Nothing,
      guessMIMETypeEnabled = Lude.Nothing,
      readOnly = Lude.Nothing,
      caseSensitivity = Lude.Nothing
    }

-- | The files and folders on this share will only be visible to users with read access.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAccessBasedEnumeration :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Bool)
usmbfsAccessBasedEnumeration = Lens.lens (accessBasedEnumeration :: UpdateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {accessBasedEnumeration = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsAccessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead." #-}

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAdminUserList :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe [Lude.Text])
usmbfsAdminUserList = Lens.lens (adminUserList :: UpdateSMBFileShare -> Lude.Maybe [Lude.Text]) (\s a -> s {adminUserList = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsAdminUserList "Use generic-lens or generic-optics with 'adminUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAuditDestinationARN :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Text)
usmbfsAuditDestinationARN = Lens.lens (auditDestinationARN :: UpdateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {auditDestinationARN = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsAuditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead." #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsInvalidUserList :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe [Lude.Text])
usmbfsInvalidUserList = Lens.lens (invalidUserList :: UpdateSMBFileShare -> Lude.Maybe [Lude.Text]) (\s a -> s {invalidUserList = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsInvalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsKMSKey :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Text)
usmbfsKMSKey = Lens.lens (kmsKey :: UpdateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsValidUserList :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe [Lude.Text])
usmbfsValidUserList = Lens.lens (validUserList :: UpdateSMBFileShare -> Lude.Maybe [Lude.Text]) (\s a -> s {validUserList = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsValidUserList "Use generic-lens or generic-optics with 'validUserList' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsCacheAttributes :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe CacheAttributes)
usmbfsCacheAttributes = Lens.lens (cacheAttributes :: UpdateSMBFileShare -> Lude.Maybe CacheAttributes) (\s a -> s {cacheAttributes = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsObjectACL :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe ObjectACL)
usmbfsObjectACL = Lens.lens (objectACL :: UpdateSMBFileShare -> Lude.Maybe ObjectACL) (\s a -> s {objectACL = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsKMSEncrypted :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Bool)
usmbfsKMSEncrypted = Lens.lens (kmsEncrypted :: UpdateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsFileShareARN :: Lens.Lens' UpdateSMBFileShare Lude.Text
usmbfsFileShareARN = Lens.lens (fileShareARN :: UpdateSMBFileShare -> Lude.Text) (\s a -> s {fileShareARN = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsDefaultStorageClass :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Text)
usmbfsDefaultStorageClass = Lens.lens (defaultStorageClass :: UpdateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {defaultStorageClass = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsFileShareName :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Text)
usmbfsFileShareName = Lens.lens (fileShareName :: UpdateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {fileShareName = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsSMBACLEnabled :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Bool)
usmbfsSMBACLEnabled = Lens.lens (sMBACLEnabled :: UpdateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {sMBACLEnabled = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsSMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsNotificationPolicy :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Text)
usmbfsNotificationPolicy = Lens.lens (notificationPolicy :: UpdateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {notificationPolicy = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsRequesterPays :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Bool)
usmbfsRequesterPays = Lens.lens (requesterPays :: UpdateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPays = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsGuessMIMETypeEnabled :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Bool)
usmbfsGuessMIMETypeEnabled = Lens.lens (guessMIMETypeEnabled :: UpdateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {guessMIMETypeEnabled = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsReadOnly :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe Lude.Bool)
usmbfsReadOnly = Lens.lens (readOnly :: UpdateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsCaseSensitivity :: Lens.Lens' UpdateSMBFileShare (Lude.Maybe CaseSensitivity)
usmbfsCaseSensitivity = Lens.lens (caseSensitivity :: UpdateSMBFileShare -> Lude.Maybe CaseSensitivity) (\s a -> s {caseSensitivity = a} :: UpdateSMBFileShare)
{-# DEPRECATED usmbfsCaseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead." #-}

instance Lude.AWSRequest UpdateSMBFileShare where
  type Rs UpdateSMBFileShare = UpdateSMBFileShareResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareResponse'
            Lude.<$> (x Lude..?> "FileShareARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSMBFileShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.UpdateSMBFileShare" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSMBFileShare where
  toJSON UpdateSMBFileShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessBasedEnumeration" Lude..=)
              Lude.<$> accessBasedEnumeration,
            ("AdminUserList" Lude..=) Lude.<$> adminUserList,
            ("AuditDestinationARN" Lude..=) Lude.<$> auditDestinationARN,
            ("InvalidUserList" Lude..=) Lude.<$> invalidUserList,
            ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("ValidUserList" Lude..=) Lude.<$> validUserList,
            ("CacheAttributes" Lude..=) Lude.<$> cacheAttributes,
            ("ObjectACL" Lude..=) Lude.<$> objectACL,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            Lude.Just ("FileShareARN" Lude..= fileShareARN),
            ("DefaultStorageClass" Lude..=) Lude.<$> defaultStorageClass,
            ("FileShareName" Lude..=) Lude.<$> fileShareName,
            ("SMBACLEnabled" Lude..=) Lude.<$> sMBACLEnabled,
            ("NotificationPolicy" Lude..=) Lude.<$> notificationPolicy,
            ("RequesterPays" Lude..=) Lude.<$> requesterPays,
            ("GuessMIMETypeEnabled" Lude..=) Lude.<$> guessMIMETypeEnabled,
            ("ReadOnly" Lude..=) Lude.<$> readOnly,
            ("CaseSensitivity" Lude..=) Lude.<$> caseSensitivity
          ]
      )

instance Lude.ToPath UpdateSMBFileShare where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSMBFileShare where
  toQuery = Lude.const Lude.mempty

-- | UpdateSMBFileShareOutput
--
-- /See:/ 'mkUpdateSMBFileShareResponse' smart constructor.
data UpdateSMBFileShareResponse = UpdateSMBFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the updated SMB file share.
    fileShareARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSMBFileShareResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the updated SMB file share.
-- * 'responseStatus' - The response status code.
mkUpdateSMBFileShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSMBFileShareResponse
mkUpdateSMBFileShareResponse pResponseStatus_ =
  UpdateSMBFileShareResponse'
    { fileShareARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated SMB file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsrsFileShareARN :: Lens.Lens' UpdateSMBFileShareResponse (Lude.Maybe Lude.Text)
usmbfsrsFileShareARN = Lens.lens (fileShareARN :: UpdateSMBFileShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: UpdateSMBFileShareResponse)
{-# DEPRECATED usmbfsrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsrsResponseStatus :: Lens.Lens' UpdateSMBFileShareResponse Lude.Int
usmbfsrsResponseStatus = Lens.lens (responseStatus :: UpdateSMBFileShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSMBFileShareResponse)
{-# DEPRECATED usmbfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
