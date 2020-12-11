{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateSMBFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Server Message Block (SMB) file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using an SMB interface. This operation is only supported for file gateways.
--
-- /Important:/ File gateways require AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure that AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in this AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- File gateways don't support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.CreateSMBFileShare
  ( -- * Creating a request
    CreateSMBFileShare (..),
    mkCreateSMBFileShare,

    -- ** Request lenses
    csmbfsAccessBasedEnumeration,
    csmbfsAdminUserList,
    csmbfsAuditDestinationARN,
    csmbfsInvalidUserList,
    csmbfsKMSKey,
    csmbfsValidUserList,
    csmbfsAuthentication,
    csmbfsCacheAttributes,
    csmbfsObjectACL,
    csmbfsKMSEncrypted,
    csmbfsDefaultStorageClass,
    csmbfsFileShareName,
    csmbfsSMBACLEnabled,
    csmbfsNotificationPolicy,
    csmbfsRequesterPays,
    csmbfsGuessMIMETypeEnabled,
    csmbfsReadOnly,
    csmbfsCaseSensitivity,
    csmbfsTags,
    csmbfsClientToken,
    csmbfsGatewayARN,
    csmbfsRole,
    csmbfsLocationARN,

    -- * Destructuring the response
    CreateSMBFileShareResponse (..),
    mkCreateSMBFileShareResponse,

    -- ** Response lenses
    csmbfsrsFileShareARN,
    csmbfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | CreateSMBFileShareInput
--
-- /See:/ 'mkCreateSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { accessBasedEnumeration ::
      Lude.Maybe Lude.Bool,
    adminUserList :: Lude.Maybe [Lude.Text],
    auditDestinationARN :: Lude.Maybe Lude.Text,
    invalidUserList :: Lude.Maybe [Lude.Text],
    kmsKey :: Lude.Maybe Lude.Text,
    validUserList :: Lude.Maybe [Lude.Text],
    authentication :: Lude.Maybe Lude.Text,
    cacheAttributes :: Lude.Maybe CacheAttributes,
    objectACL :: Lude.Maybe ObjectACL,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    defaultStorageClass :: Lude.Maybe Lude.Text,
    fileShareName :: Lude.Maybe Lude.Text,
    sMBACLEnabled :: Lude.Maybe Lude.Bool,
    notificationPolicy :: Lude.Maybe Lude.Text,
    requesterPays :: Lude.Maybe Lude.Bool,
    guessMIMETypeEnabled :: Lude.Maybe Lude.Bool,
    readOnly :: Lude.Maybe Lude.Bool,
    caseSensitivity :: Lude.Maybe CaseSensitivity,
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

-- | Creates a value of 'CreateSMBFileShare' with the minimum fields required to make a request.
--
-- * 'accessBasedEnumeration' - The files and folders on this share will only be visible to users with read access.
-- * 'adminUserList' - A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ .
--
-- /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
-- * 'auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
-- * 'authentication' - The authentication method that users use to access the file share. The default is @ActiveDirectory@ .
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
-- * 'cacheAttributes' - Refresh cache information.
-- * 'caseSensitivity' - The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
-- * 'clientToken' - A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
-- * 'defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
-- * 'fileShareName' - The name of the file share. Optional.
-- * 'gatewayARN' - The ARN of the file gateway on which you want to create a file share.
-- * 'guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
-- * 'invalidUserList' - A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'locationARN' - The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
-- * 'notificationPolicy' - The notification policy of the file share.
-- * 'objectACL' - A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
-- * 'readOnly' - A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
-- * 'requesterPays' - A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
-- * 'role'' - The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
-- * 'sMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@
-- * 'tags' - A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
-- * 'validUserList' - A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
mkCreateSMBFileShare ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'role''
  Lude.Text ->
  -- | 'locationARN'
  Lude.Text ->
  CreateSMBFileShare
mkCreateSMBFileShare
  pClientToken_
  pGatewayARN_
  pRole_
  pLocationARN_ =
    CreateSMBFileShare'
      { accessBasedEnumeration = Lude.Nothing,
        adminUserList = Lude.Nothing,
        auditDestinationARN = Lude.Nothing,
        invalidUserList = Lude.Nothing,
        kmsKey = Lude.Nothing,
        validUserList = Lude.Nothing,
        authentication = Lude.Nothing,
        cacheAttributes = Lude.Nothing,
        objectACL = Lude.Nothing,
        kmsEncrypted = Lude.Nothing,
        defaultStorageClass = Lude.Nothing,
        fileShareName = Lude.Nothing,
        sMBACLEnabled = Lude.Nothing,
        notificationPolicy = Lude.Nothing,
        requesterPays = Lude.Nothing,
        guessMIMETypeEnabled = Lude.Nothing,
        readOnly = Lude.Nothing,
        caseSensitivity = Lude.Nothing,
        tags = Lude.Nothing,
        clientToken = pClientToken_,
        gatewayARN = pGatewayARN_,
        role' = pRole_,
        locationARN = pLocationARN_
      }

-- | The files and folders on this share will only be visible to users with read access.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAccessBasedEnumeration :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Bool)
csmbfsAccessBasedEnumeration = Lens.lens (accessBasedEnumeration :: CreateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {accessBasedEnumeration = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsAccessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead." #-}

-- | A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ .
--
-- /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAdminUserList :: Lens.Lens' CreateSMBFileShare (Lude.Maybe [Lude.Text])
csmbfsAdminUserList = Lens.lens (adminUserList :: CreateSMBFileShare -> Lude.Maybe [Lude.Text]) (\s a -> s {adminUserList = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsAdminUserList "Use generic-lens or generic-optics with 'adminUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAuditDestinationARN :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Text)
csmbfsAuditDestinationARN = Lens.lens (auditDestinationARN :: CreateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {auditDestinationARN = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsAuditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead." #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsInvalidUserList :: Lens.Lens' CreateSMBFileShare (Lude.Maybe [Lude.Text])
csmbfsInvalidUserList = Lens.lens (invalidUserList :: CreateSMBFileShare -> Lude.Maybe [Lude.Text]) (\s a -> s {invalidUserList = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsInvalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsKMSKey :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Text)
csmbfsKMSKey = Lens.lens (kmsKey :: CreateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsValidUserList :: Lens.Lens' CreateSMBFileShare (Lude.Maybe [Lude.Text])
csmbfsValidUserList = Lens.lens (validUserList :: CreateSMBFileShare -> Lude.Maybe [Lude.Text]) (\s a -> s {validUserList = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsValidUserList "Use generic-lens or generic-optics with 'validUserList' instead." #-}

-- | The authentication method that users use to access the file share. The default is @ActiveDirectory@ .
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAuthentication :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Text)
csmbfsAuthentication = Lens.lens (authentication :: CreateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {authentication = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsCacheAttributes :: Lens.Lens' CreateSMBFileShare (Lude.Maybe CacheAttributes)
csmbfsCacheAttributes = Lens.lens (cacheAttributes :: CreateSMBFileShare -> Lude.Maybe CacheAttributes) (\s a -> s {cacheAttributes = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsObjectACL :: Lens.Lens' CreateSMBFileShare (Lude.Maybe ObjectACL)
csmbfsObjectACL = Lens.lens (objectACL :: CreateSMBFileShare -> Lude.Maybe ObjectACL) (\s a -> s {objectACL = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsKMSEncrypted :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Bool)
csmbfsKMSEncrypted = Lens.lens (kmsEncrypted :: CreateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsDefaultStorageClass :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Text)
csmbfsDefaultStorageClass = Lens.lens (defaultStorageClass :: CreateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {defaultStorageClass = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsFileShareName :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Text)
csmbfsFileShareName = Lens.lens (fileShareName :: CreateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {fileShareName = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsSMBACLEnabled :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Bool)
csmbfsSMBACLEnabled = Lens.lens (sMBACLEnabled :: CreateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {sMBACLEnabled = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsSMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsNotificationPolicy :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Text)
csmbfsNotificationPolicy = Lens.lens (notificationPolicy :: CreateSMBFileShare -> Lude.Maybe Lude.Text) (\s a -> s {notificationPolicy = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsRequesterPays :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Bool)
csmbfsRequesterPays = Lens.lens (requesterPays :: CreateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {requesterPays = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsGuessMIMETypeEnabled :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Bool)
csmbfsGuessMIMETypeEnabled = Lens.lens (guessMIMETypeEnabled :: CreateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {guessMIMETypeEnabled = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsReadOnly :: Lens.Lens' CreateSMBFileShare (Lude.Maybe Lude.Bool)
csmbfsReadOnly = Lens.lens (readOnly :: CreateSMBFileShare -> Lude.Maybe Lude.Bool) (\s a -> s {readOnly = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsCaseSensitivity :: Lens.Lens' CreateSMBFileShare (Lude.Maybe CaseSensitivity)
csmbfsCaseSensitivity = Lens.lens (caseSensitivity :: CreateSMBFileShare -> Lude.Maybe CaseSensitivity) (\s a -> s {caseSensitivity = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsCaseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead." #-}

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsTags :: Lens.Lens' CreateSMBFileShare (Lude.Maybe [Tag])
csmbfsTags = Lens.lens (tags :: CreateSMBFileShare -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsClientToken :: Lens.Lens' CreateSMBFileShare Lude.Text
csmbfsClientToken = Lens.lens (clientToken :: CreateSMBFileShare -> Lude.Text) (\s a -> s {clientToken = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ARN of the file gateway on which you want to create a file share.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsGatewayARN :: Lens.Lens' CreateSMBFileShare Lude.Text
csmbfsGatewayARN = Lens.lens (gatewayARN :: CreateSMBFileShare -> Lude.Text) (\s a -> s {gatewayARN = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsRole :: Lens.Lens' CreateSMBFileShare Lude.Text
csmbfsRole = Lens.lens (role' :: CreateSMBFileShare -> Lude.Text) (\s a -> s {role' = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsLocationARN :: Lens.Lens' CreateSMBFileShare Lude.Text
csmbfsLocationARN = Lens.lens (locationARN :: CreateSMBFileShare -> Lude.Text) (\s a -> s {locationARN = a} :: CreateSMBFileShare)
{-# DEPRECATED csmbfsLocationARN "Use generic-lens or generic-optics with 'locationARN' instead." #-}

instance Lude.AWSRequest CreateSMBFileShare where
  type Rs CreateSMBFileShare = CreateSMBFileShareResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSMBFileShareResponse'
            Lude.<$> (x Lude..?> "FileShareARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSMBFileShare where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CreateSMBFileShare" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSMBFileShare where
  toJSON CreateSMBFileShare' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AccessBasedEnumeration" Lude..=)
              Lude.<$> accessBasedEnumeration,
            ("AdminUserList" Lude..=) Lude.<$> adminUserList,
            ("AuditDestinationARN" Lude..=) Lude.<$> auditDestinationARN,
            ("InvalidUserList" Lude..=) Lude.<$> invalidUserList,
            ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("ValidUserList" Lude..=) Lude.<$> validUserList,
            ("Authentication" Lude..=) Lude.<$> authentication,
            ("CacheAttributes" Lude..=) Lude.<$> cacheAttributes,
            ("ObjectACL" Lude..=) Lude.<$> objectACL,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            ("DefaultStorageClass" Lude..=) Lude.<$> defaultStorageClass,
            ("FileShareName" Lude..=) Lude.<$> fileShareName,
            ("SMBACLEnabled" Lude..=) Lude.<$> sMBACLEnabled,
            ("NotificationPolicy" Lude..=) Lude.<$> notificationPolicy,
            ("RequesterPays" Lude..=) Lude.<$> requesterPays,
            ("GuessMIMETypeEnabled" Lude..=) Lude.<$> guessMIMETypeEnabled,
            ("ReadOnly" Lude..=) Lude.<$> readOnly,
            ("CaseSensitivity" Lude..=) Lude.<$> caseSensitivity,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("Role" Lude..= role'),
            Lude.Just ("LocationARN" Lude..= locationARN)
          ]
      )

instance Lude.ToPath CreateSMBFileShare where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSMBFileShare where
  toQuery = Lude.const Lude.mempty

-- | CreateSMBFileShareOutput
--
-- /See:/ 'mkCreateSMBFileShareResponse' smart constructor.
data CreateSMBFileShareResponse = CreateSMBFileShareResponse'
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

-- | Creates a value of 'CreateSMBFileShareResponse' with the minimum fields required to make a request.
--
-- * 'fileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
-- * 'responseStatus' - The response status code.
mkCreateSMBFileShareResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSMBFileShareResponse
mkCreateSMBFileShareResponse pResponseStatus_ =
  CreateSMBFileShareResponse'
    { fileShareARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsrsFileShareARN :: Lens.Lens' CreateSMBFileShareResponse (Lude.Maybe Lude.Text)
csmbfsrsFileShareARN = Lens.lens (fileShareARN :: CreateSMBFileShareResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileShareARN = a} :: CreateSMBFileShareResponse)
{-# DEPRECATED csmbfsrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsrsResponseStatus :: Lens.Lens' CreateSMBFileShareResponse Lude.Int
csmbfsrsResponseStatus = Lens.lens (responseStatus :: CreateSMBFileShareResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSMBFileShareResponse)
{-# DEPRECATED csmbfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
