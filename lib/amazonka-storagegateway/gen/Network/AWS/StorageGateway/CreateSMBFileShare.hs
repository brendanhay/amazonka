{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    csmbfsClientToken,
    csmbfsGatewayARN,
    csmbfsRole,
    csmbfsLocationARN,
    csmbfsAccessBasedEnumeration,
    csmbfsAdminUserList,
    csmbfsAuditDestinationARN,
    csmbfsAuthentication,
    csmbfsCacheAttributes,
    csmbfsCaseSensitivity,
    csmbfsDefaultStorageClass,
    csmbfsFileShareName,
    csmbfsGuessMIMETypeEnabled,
    csmbfsInvalidUserList,
    csmbfsKMSEncrypted,
    csmbfsKMSKey,
    csmbfsNotificationPolicy,
    csmbfsObjectACL,
    csmbfsReadOnly,
    csmbfsRequesterPays,
    csmbfsSMBACLEnabled,
    csmbfsTags,
    csmbfsValidUserList,

    -- * Destructuring the response
    CreateSMBFileShareResponse (..),
    mkCreateSMBFileShareResponse,

    -- ** Response lenses
    csmbfsrrsFileShareARN,
    csmbfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CreateSMBFileShareInput
--
-- /See:/ 'mkCreateSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { -- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
    clientToken :: Types.ClientToken,
    -- | The ARN of the file gateway on which you want to create a file share.
    gatewayARN :: Types.GatewayARN,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
    role' :: Types.Role,
    -- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
    locationARN :: Types.LocationARN,
    -- | The files and folders on this share will only be visible to users with read access.
    accessBasedEnumeration :: Core.Maybe Core.Bool,
    -- | A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ .
    --
    -- /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
    adminUserList :: Core.Maybe [Types.FileShareUser],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Core.Maybe Types.AuditDestinationARN,
    -- | The authentication method that users use to access the file share. The default is @ActiveDirectory@ .
    --
    -- Valid Values: @ActiveDirectory@ | @GuestAccess@
    authentication :: Core.Maybe Types.Authentication,
    -- | Refresh cache information.
    cacheAttributes :: Core.Maybe Types.CacheAttributes,
    -- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
    caseSensitivity :: Core.Maybe Types.CaseSensitivity,
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
    -- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    invalidUserList :: Core.Maybe [Types.FileShareUser],
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kMSEncrypted :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
    kMSKey :: Core.Maybe Types.KMSKey,
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
    -- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
    --
    -- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
    -- Valid Values: @true@ | @false@
    sMBACLEnabled :: Core.Maybe Core.Bool,
    -- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag],
    -- | A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    validUserList :: Core.Maybe [Types.FileShareUser]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSMBFileShare' value with any optional fields omitted.
mkCreateSMBFileShare ::
  -- | 'clientToken'
  Types.ClientToken ->
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'role\''
  Types.Role ->
  -- | 'locationARN'
  Types.LocationARN ->
  CreateSMBFileShare
mkCreateSMBFileShare clientToken gatewayARN role' locationARN =
  CreateSMBFileShare'
    { clientToken,
      gatewayARN,
      role',
      locationARN,
      accessBasedEnumeration = Core.Nothing,
      adminUserList = Core.Nothing,
      auditDestinationARN = Core.Nothing,
      authentication = Core.Nothing,
      cacheAttributes = Core.Nothing,
      caseSensitivity = Core.Nothing,
      defaultStorageClass = Core.Nothing,
      fileShareName = Core.Nothing,
      guessMIMETypeEnabled = Core.Nothing,
      invalidUserList = Core.Nothing,
      kMSEncrypted = Core.Nothing,
      kMSKey = Core.Nothing,
      notificationPolicy = Core.Nothing,
      objectACL = Core.Nothing,
      readOnly = Core.Nothing,
      requesterPays = Core.Nothing,
      sMBACLEnabled = Core.Nothing,
      tags = Core.Nothing,
      validUserList = Core.Nothing
    }

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsClientToken :: Lens.Lens' CreateSMBFileShare Types.ClientToken
csmbfsClientToken = Lens.field @"clientToken"
{-# DEPRECATED csmbfsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The ARN of the file gateway on which you want to create a file share.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsGatewayARN :: Lens.Lens' CreateSMBFileShare Types.GatewayARN
csmbfsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED csmbfsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsRole :: Lens.Lens' CreateSMBFileShare Types.Role
csmbfsRole = Lens.field @"role'"
{-# DEPRECATED csmbfsRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsLocationARN :: Lens.Lens' CreateSMBFileShare Types.LocationARN
csmbfsLocationARN = Lens.field @"locationARN"
{-# DEPRECATED csmbfsLocationARN "Use generic-lens or generic-optics with 'locationARN' instead." #-}

-- | The files and folders on this share will only be visible to users with read access.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAccessBasedEnumeration :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsAccessBasedEnumeration = Lens.field @"accessBasedEnumeration"
{-# DEPRECATED csmbfsAccessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead." #-}

-- | A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ .
--
-- /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAdminUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.FileShareUser])
csmbfsAdminUserList = Lens.field @"adminUserList"
{-# DEPRECATED csmbfsAdminUserList "Use generic-lens or generic-optics with 'adminUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAuditDestinationARN :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.AuditDestinationARN)
csmbfsAuditDestinationARN = Lens.field @"auditDestinationARN"
{-# DEPRECATED csmbfsAuditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead." #-}

-- | The authentication method that users use to access the file share. The default is @ActiveDirectory@ .
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAuthentication :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.Authentication)
csmbfsAuthentication = Lens.field @"authentication"
{-# DEPRECATED csmbfsAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsCacheAttributes :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.CacheAttributes)
csmbfsCacheAttributes = Lens.field @"cacheAttributes"
{-# DEPRECATED csmbfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsCaseSensitivity :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.CaseSensitivity)
csmbfsCaseSensitivity = Lens.field @"caseSensitivity"
{-# DEPRECATED csmbfsCaseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsDefaultStorageClass :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.DefaultStorageClass)
csmbfsDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# DEPRECATED csmbfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsFileShareName :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.FileShareName)
csmbfsFileShareName = Lens.field @"fileShareName"
{-# DEPRECATED csmbfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsGuessMIMETypeEnabled :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# DEPRECATED csmbfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsInvalidUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.FileShareUser])
csmbfsInvalidUserList = Lens.field @"invalidUserList"
{-# DEPRECATED csmbfsInvalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsKMSEncrypted :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsKMSEncrypted = Lens.field @"kMSEncrypted"
{-# DEPRECATED csmbfsKMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsKMSKey :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.KMSKey)
csmbfsKMSKey = Lens.field @"kMSKey"
{-# DEPRECATED csmbfsKMSKey "Use generic-lens or generic-optics with 'kMSKey' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsNotificationPolicy :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.NotificationPolicy)
csmbfsNotificationPolicy = Lens.field @"notificationPolicy"
{-# DEPRECATED csmbfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsObjectACL :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.ObjectACL)
csmbfsObjectACL = Lens.field @"objectACL"
{-# DEPRECATED csmbfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsReadOnly :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsReadOnly = Lens.field @"readOnly"
{-# DEPRECATED csmbfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsRequesterPays :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsRequesterPays = Lens.field @"requesterPays"
{-# DEPRECATED csmbfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsSMBACLEnabled :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsSMBACLEnabled = Lens.field @"sMBACLEnabled"
{-# DEPRECATED csmbfsSMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead." #-}

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsTags :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.Tag])
csmbfsTags = Lens.field @"tags"
{-# DEPRECATED csmbfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsValidUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.FileShareUser])
csmbfsValidUserList = Lens.field @"validUserList"
{-# DEPRECATED csmbfsValidUserList "Use generic-lens or generic-optics with 'validUserList' instead." #-}

instance Core.FromJSON CreateSMBFileShare where
  toJSON CreateSMBFileShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("Role" Core..= role'),
            Core.Just ("LocationARN" Core..= locationARN),
            ("AccessBasedEnumeration" Core..=) Core.<$> accessBasedEnumeration,
            ("AdminUserList" Core..=) Core.<$> adminUserList,
            ("AuditDestinationARN" Core..=) Core.<$> auditDestinationARN,
            ("Authentication" Core..=) Core.<$> authentication,
            ("CacheAttributes" Core..=) Core.<$> cacheAttributes,
            ("CaseSensitivity" Core..=) Core.<$> caseSensitivity,
            ("DefaultStorageClass" Core..=) Core.<$> defaultStorageClass,
            ("FileShareName" Core..=) Core.<$> fileShareName,
            ("GuessMIMETypeEnabled" Core..=) Core.<$> guessMIMETypeEnabled,
            ("InvalidUserList" Core..=) Core.<$> invalidUserList,
            ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
            ("KMSKey" Core..=) Core.<$> kMSKey,
            ("NotificationPolicy" Core..=) Core.<$> notificationPolicy,
            ("ObjectACL" Core..=) Core.<$> objectACL,
            ("ReadOnly" Core..=) Core.<$> readOnly,
            ("RequesterPays" Core..=) Core.<$> requesterPays,
            ("SMBACLEnabled" Core..=) Core.<$> sMBACLEnabled,
            ("Tags" Core..=) Core.<$> tags,
            ("ValidUserList" Core..=) Core.<$> validUserList
          ]
      )

instance Core.AWSRequest CreateSMBFileShare where
  type Rs CreateSMBFileShare = CreateSMBFileShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.CreateSMBFileShare")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSMBFileShareResponse'
            Core.<$> (x Core..:? "FileShareARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | CreateSMBFileShareOutput
--
-- /See:/ 'mkCreateSMBFileShareResponse' smart constructor.
data CreateSMBFileShareResponse = CreateSMBFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the newly created file share.
    fileShareARN :: Core.Maybe Types.FileShareARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSMBFileShareResponse' value with any optional fields omitted.
mkCreateSMBFileShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSMBFileShareResponse
mkCreateSMBFileShareResponse responseStatus =
  CreateSMBFileShareResponse'
    { fileShareARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsrrsFileShareARN :: Lens.Lens' CreateSMBFileShareResponse (Core.Maybe Types.FileShareARN)
csmbfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED csmbfsrrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsrrsResponseStatus :: Lens.Lens' CreateSMBFileShareResponse Core.Int
csmbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csmbfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
