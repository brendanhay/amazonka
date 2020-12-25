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
    usmbfsFileShareARN,
    usmbfsAccessBasedEnumeration,
    usmbfsAdminUserList,
    usmbfsAuditDestinationARN,
    usmbfsCacheAttributes,
    usmbfsCaseSensitivity,
    usmbfsDefaultStorageClass,
    usmbfsFileShareName,
    usmbfsGuessMIMETypeEnabled,
    usmbfsInvalidUserList,
    usmbfsKMSEncrypted,
    usmbfsKMSKey,
    usmbfsNotificationPolicy,
    usmbfsObjectACL,
    usmbfsReadOnly,
    usmbfsRequesterPays,
    usmbfsSMBACLEnabled,
    usmbfsValidUserList,

    -- * Destructuring the response
    UpdateSMBFileShareResponse (..),
    mkUpdateSMBFileShareResponse,

    -- ** Response lenses
    usmbfsrrsFileShareARN,
    usmbfsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | UpdateSMBFileShareInput
--
-- /See:/ 'mkUpdateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { -- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
    fileShareARN :: Types.FileShareARN,
    -- | The files and folders on this share will only be visible to users with read access.
    accessBasedEnumeration :: Core.Maybe Core.Bool,
    -- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    adminUserList :: Core.Maybe [Types.FileShareUser],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Core.Maybe Types.AuditDestinationARN,
    -- | Refresh cache information.
    cacheAttributes :: Core.Maybe Types.CacheAttributes,
    -- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
    caseSensitivity :: Core.Maybe Types.CaseSensitivity,
    -- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Core.Maybe Types.StorageClass,
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
    -- | A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
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
    -- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
    validUserList :: Core.Maybe [Types.FileShareUser]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBFileShare' value with any optional fields omitted.
mkUpdateSMBFileShare ::
  -- | 'fileShareARN'
  Types.FileShareARN ->
  UpdateSMBFileShare
mkUpdateSMBFileShare fileShareARN =
  UpdateSMBFileShare'
    { fileShareARN,
      accessBasedEnumeration = Core.Nothing,
      adminUserList = Core.Nothing,
      auditDestinationARN = Core.Nothing,
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
      validUserList = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsFileShareARN :: Lens.Lens' UpdateSMBFileShare Types.FileShareARN
usmbfsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED usmbfsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The files and folders on this share will only be visible to users with read access.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAccessBasedEnumeration :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsAccessBasedEnumeration = Lens.field @"accessBasedEnumeration"
{-# DEPRECATED usmbfsAccessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead." #-}

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAdminUserList :: Lens.Lens' UpdateSMBFileShare (Core.Maybe [Types.FileShareUser])
usmbfsAdminUserList = Lens.field @"adminUserList"
{-# DEPRECATED usmbfsAdminUserList "Use generic-lens or generic-optics with 'adminUserList' instead." #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAuditDestinationARN :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.AuditDestinationARN)
usmbfsAuditDestinationARN = Lens.field @"auditDestinationARN"
{-# DEPRECATED usmbfsAuditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead." #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsCacheAttributes :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.CacheAttributes)
usmbfsCacheAttributes = Lens.field @"cacheAttributes"
{-# DEPRECATED usmbfsCacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead." #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsCaseSensitivity :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.CaseSensitivity)
usmbfsCaseSensitivity = Lens.field @"caseSensitivity"
{-# DEPRECATED usmbfsCaseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead." #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsDefaultStorageClass :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.StorageClass)
usmbfsDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# DEPRECATED usmbfsDefaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead." #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsFileShareName :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.FileShareName)
usmbfsFileShareName = Lens.field @"fileShareName"
{-# DEPRECATED usmbfsFileShareName "Use generic-lens or generic-optics with 'fileShareName' instead." #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsGuessMIMETypeEnabled :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# DEPRECATED usmbfsGuessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead." #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsInvalidUserList :: Lens.Lens' UpdateSMBFileShare (Core.Maybe [Types.FileShareUser])
usmbfsInvalidUserList = Lens.field @"invalidUserList"
{-# DEPRECATED usmbfsInvalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsKMSEncrypted :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsKMSEncrypted = Lens.field @"kMSEncrypted"
{-# DEPRECATED usmbfsKMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsKMSKey :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.KMSKey)
usmbfsKMSKey = Lens.field @"kMSKey"
{-# DEPRECATED usmbfsKMSKey "Use generic-lens or generic-optics with 'kMSKey' instead." #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsNotificationPolicy :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.NotificationPolicy)
usmbfsNotificationPolicy = Lens.field @"notificationPolicy"
{-# DEPRECATED usmbfsNotificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead." #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsObjectACL :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.ObjectACL)
usmbfsObjectACL = Lens.field @"objectACL"
{-# DEPRECATED usmbfsObjectACL "Use generic-lens or generic-optics with 'objectACL' instead." #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsReadOnly :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsReadOnly = Lens.field @"readOnly"
{-# DEPRECATED usmbfsReadOnly "Use generic-lens or generic-optics with 'readOnly' instead." #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsRequesterPays :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsRequesterPays = Lens.field @"requesterPays"
{-# DEPRECATED usmbfsRequesterPays "Use generic-lens or generic-optics with 'requesterPays' instead." #-}

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsSMBACLEnabled :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsSMBACLEnabled = Lens.field @"sMBACLEnabled"
{-# DEPRECATED usmbfsSMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead." #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsValidUserList :: Lens.Lens' UpdateSMBFileShare (Core.Maybe [Types.FileShareUser])
usmbfsValidUserList = Lens.field @"validUserList"
{-# DEPRECATED usmbfsValidUserList "Use generic-lens or generic-optics with 'validUserList' instead." #-}

instance Core.FromJSON UpdateSMBFileShare where
  toJSON UpdateSMBFileShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FileShareARN" Core..= fileShareARN),
            ("AccessBasedEnumeration" Core..=) Core.<$> accessBasedEnumeration,
            ("AdminUserList" Core..=) Core.<$> adminUserList,
            ("AuditDestinationARN" Core..=) Core.<$> auditDestinationARN,
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
            ("ValidUserList" Core..=) Core.<$> validUserList
          ]
      )

instance Core.AWSRequest UpdateSMBFileShare where
  type Rs UpdateSMBFileShare = UpdateSMBFileShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.UpdateSMBFileShare")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareResponse'
            Core.<$> (x Core..:? "FileShareARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | UpdateSMBFileShareOutput
--
-- /See:/ 'mkUpdateSMBFileShareResponse' smart constructor.
data UpdateSMBFileShareResponse = UpdateSMBFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the updated SMB file share.
    fileShareARN :: Core.Maybe Types.FileShareARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBFileShareResponse' value with any optional fields omitted.
mkUpdateSMBFileShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSMBFileShareResponse
mkUpdateSMBFileShareResponse responseStatus =
  UpdateSMBFileShareResponse'
    { fileShareARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the updated SMB file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsrrsFileShareARN :: Lens.Lens' UpdateSMBFileShareResponse (Core.Maybe Types.FileShareARN)
usmbfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# DEPRECATED usmbfsrrsFileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsrrsResponseStatus :: Lens.Lens' UpdateSMBFileShareResponse Core.Int
usmbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usmbfsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
