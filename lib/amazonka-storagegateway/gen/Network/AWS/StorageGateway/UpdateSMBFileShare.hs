{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateSMBFileShare (..)
    , mkUpdateSMBFileShare
    -- ** Request lenses
    , usmbfsFileShareARN
    , usmbfsAccessBasedEnumeration
    , usmbfsAdminUserList
    , usmbfsAuditDestinationARN
    , usmbfsCacheAttributes
    , usmbfsCaseSensitivity
    , usmbfsDefaultStorageClass
    , usmbfsFileShareName
    , usmbfsGuessMIMETypeEnabled
    , usmbfsInvalidUserList
    , usmbfsKMSEncrypted
    , usmbfsKMSKey
    , usmbfsNotificationPolicy
    , usmbfsObjectACL
    , usmbfsReadOnly
    , usmbfsRequesterPays
    , usmbfsSMBACLEnabled
    , usmbfsValidUserList

    -- * Destructuring the response
    , UpdateSMBFileShareResponse (..)
    , mkUpdateSMBFileShareResponse
    -- ** Response lenses
    , usmbfsrrsFileShareARN
    , usmbfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | UpdateSMBFileShareInput
--
-- /See:/ 'mkUpdateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { fileShareARN :: Types.FileShareARN
    -- ^ The Amazon Resource Name (ARN) of the SMB file share that you want to update.
  , accessBasedEnumeration :: Core.Maybe Core.Bool
    -- ^ The files and folders on this share will only be visible to users with read access.
  , adminUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
  , auditDestinationARN :: Core.Maybe Types.AuditDestinationARN
    -- ^ The Amazon Resource Name (ARN) of the storage used for the audit logs.
  , cacheAttributes :: Core.Maybe Types.CacheAttributes
    -- ^ Refresh cache information.
  , caseSensitivity :: Core.Maybe Types.CaseSensitivity
    -- ^ The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
  , defaultStorageClass :: Core.Maybe Types.StorageClass
    -- ^ The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
  , fileShareName :: Core.Maybe Types.FileShareName
    -- ^ The name of the file share. Optional.
  , guessMIMETypeEnabled :: Core.Maybe Core.Bool
    -- ^ A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@ 
  , invalidUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
  , kMSEncrypted :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
  , kMSKey :: Core.Maybe Types.KMSKey
    -- ^ The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
  , notificationPolicy :: Core.Maybe Types.NotificationPolicy
    -- ^ The notification policy of the file share.
  , objectACL :: Core.Maybe Types.ObjectACL
    -- ^ A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
  , readOnly :: Core.Maybe Core.Bool
    -- ^ A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
  , requesterPays :: Core.Maybe Core.Bool
    -- ^ A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
  , sMBACLEnabled :: Core.Maybe Core.Bool
    -- ^ Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@ 
  , validUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBFileShare' value with any optional fields omitted.
mkUpdateSMBFileShare
    :: Types.FileShareARN -- ^ 'fileShareARN'
    -> UpdateSMBFileShare
mkUpdateSMBFileShare fileShareARN
  = UpdateSMBFileShare'{fileShareARN,
                        accessBasedEnumeration = Core.Nothing,
                        adminUserList = Core.Nothing, auditDestinationARN = Core.Nothing,
                        cacheAttributes = Core.Nothing, caseSensitivity = Core.Nothing,
                        defaultStorageClass = Core.Nothing, fileShareName = Core.Nothing,
                        guessMIMETypeEnabled = Core.Nothing,
                        invalidUserList = Core.Nothing, kMSEncrypted = Core.Nothing,
                        kMSKey = Core.Nothing, notificationPolicy = Core.Nothing,
                        objectACL = Core.Nothing, readOnly = Core.Nothing,
                        requesterPays = Core.Nothing, sMBACLEnabled = Core.Nothing,
                        validUserList = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to update.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsFileShareARN :: Lens.Lens' UpdateSMBFileShare Types.FileShareARN
usmbfsFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE usmbfsFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | The files and folders on this share will only be visible to users with read access.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAccessBasedEnumeration :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsAccessBasedEnumeration = Lens.field @"accessBasedEnumeration"
{-# INLINEABLE usmbfsAccessBasedEnumeration #-}
{-# DEPRECATED accessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead"  #-}

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAdminUserList :: Lens.Lens' UpdateSMBFileShare (Core.Maybe [Types.FileShareUser])
usmbfsAdminUserList = Lens.field @"adminUserList"
{-# INLINEABLE usmbfsAdminUserList #-}
{-# DEPRECATED adminUserList "Use generic-lens or generic-optics with 'adminUserList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsAuditDestinationARN :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.AuditDestinationARN)
usmbfsAuditDestinationARN = Lens.field @"auditDestinationARN"
{-# INLINEABLE usmbfsAuditDestinationARN #-}
{-# DEPRECATED auditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead"  #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsCacheAttributes :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.CacheAttributes)
usmbfsCacheAttributes = Lens.field @"cacheAttributes"
{-# INLINEABLE usmbfsCacheAttributes #-}
{-# DEPRECATED cacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead"  #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsCaseSensitivity :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.CaseSensitivity)
usmbfsCaseSensitivity = Lens.field @"caseSensitivity"
{-# INLINEABLE usmbfsCaseSensitivity #-}
{-# DEPRECATED caseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead"  #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsDefaultStorageClass :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.StorageClass)
usmbfsDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# INLINEABLE usmbfsDefaultStorageClass #-}
{-# DEPRECATED defaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead"  #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsFileShareName :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.FileShareName)
usmbfsFileShareName = Lens.field @"fileShareName"
{-# INLINEABLE usmbfsFileShareName #-}
{-# DEPRECATED fileShareName "Use generic-lens or generic-optics with 'fileShareName' instead"  #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsGuessMIMETypeEnabled :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# INLINEABLE usmbfsGuessMIMETypeEnabled #-}
{-# DEPRECATED guessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead"  #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsInvalidUserList :: Lens.Lens' UpdateSMBFileShare (Core.Maybe [Types.FileShareUser])
usmbfsInvalidUserList = Lens.field @"invalidUserList"
{-# INLINEABLE usmbfsInvalidUserList #-}
{-# DEPRECATED invalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsKMSEncrypted :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE usmbfsKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsKMSKey :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.KMSKey)
usmbfsKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE usmbfsKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsNotificationPolicy :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.NotificationPolicy)
usmbfsNotificationPolicy = Lens.field @"notificationPolicy"
{-# INLINEABLE usmbfsNotificationPolicy #-}
{-# DEPRECATED notificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead"  #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsObjectACL :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Types.ObjectACL)
usmbfsObjectACL = Lens.field @"objectACL"
{-# INLINEABLE usmbfsObjectACL #-}
{-# DEPRECATED objectACL "Use generic-lens or generic-optics with 'objectACL' instead"  #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsReadOnly :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsReadOnly = Lens.field @"readOnly"
{-# INLINEABLE usmbfsReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsRequesterPays :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsRequesterPays = Lens.field @"requesterPays"
{-# INLINEABLE usmbfsRequesterPays #-}
{-# DEPRECATED requesterPays "Use generic-lens or generic-optics with 'requesterPays' instead"  #-}

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsSMBACLEnabled :: Lens.Lens' UpdateSMBFileShare (Core.Maybe Core.Bool)
usmbfsSMBACLEnabled = Lens.field @"sMBACLEnabled"
{-# INLINEABLE usmbfsSMBACLEnabled #-}
{-# DEPRECATED sMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead"  #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsValidUserList :: Lens.Lens' UpdateSMBFileShare (Core.Maybe [Types.FileShareUser])
usmbfsValidUserList = Lens.field @"validUserList"
{-# INLINEABLE usmbfsValidUserList #-}
{-# DEPRECATED validUserList "Use generic-lens or generic-optics with 'validUserList' instead"  #-}

instance Core.ToQuery UpdateSMBFileShare where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSMBFileShare where
        toHeaders UpdateSMBFileShare{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.UpdateSMBFileShare")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSMBFileShare where
        toJSON UpdateSMBFileShare{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FileShareARN" Core..= fileShareARN),
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
                  ("ValidUserList" Core..=) Core.<$> validUserList])

instance Core.AWSRequest UpdateSMBFileShare where
        type Rs UpdateSMBFileShare = UpdateSMBFileShareResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSMBFileShareResponse' Core.<$>
                   (x Core..:? "FileShareARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | UpdateSMBFileShareOutput
--
-- /See:/ 'mkUpdateSMBFileShareResponse' smart constructor.
data UpdateSMBFileShareResponse = UpdateSMBFileShareResponse'
  { fileShareARN :: Core.Maybe Types.FileShareARN
    -- ^ The Amazon Resource Name (ARN) of the updated SMB file share.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSMBFileShareResponse' value with any optional fields omitted.
mkUpdateSMBFileShareResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSMBFileShareResponse
mkUpdateSMBFileShareResponse responseStatus
  = UpdateSMBFileShareResponse'{fileShareARN = Core.Nothing,
                                responseStatus}

-- | The Amazon Resource Name (ARN) of the updated SMB file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsrrsFileShareARN :: Lens.Lens' UpdateSMBFileShareResponse (Core.Maybe Types.FileShareARN)
usmbfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE usmbfsrrsFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usmbfsrrsResponseStatus :: Lens.Lens' UpdateSMBFileShareResponse Core.Int
usmbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usmbfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
