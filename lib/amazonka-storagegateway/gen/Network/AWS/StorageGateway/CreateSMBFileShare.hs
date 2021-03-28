{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateSMBFileShare (..)
    , mkCreateSMBFileShare
    -- ** Request lenses
    , csmbfsClientToken
    , csmbfsGatewayARN
    , csmbfsRole
    , csmbfsLocationARN
    , csmbfsAccessBasedEnumeration
    , csmbfsAdminUserList
    , csmbfsAuditDestinationARN
    , csmbfsAuthentication
    , csmbfsCacheAttributes
    , csmbfsCaseSensitivity
    , csmbfsDefaultStorageClass
    , csmbfsFileShareName
    , csmbfsGuessMIMETypeEnabled
    , csmbfsInvalidUserList
    , csmbfsKMSEncrypted
    , csmbfsKMSKey
    , csmbfsNotificationPolicy
    , csmbfsObjectACL
    , csmbfsReadOnly
    , csmbfsRequesterPays
    , csmbfsSMBACLEnabled
    , csmbfsTags
    , csmbfsValidUserList

    -- * Destructuring the response
    , CreateSMBFileShareResponse (..)
    , mkCreateSMBFileShareResponse
    -- ** Response lenses
    , csmbfsrrsFileShareARN
    , csmbfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CreateSMBFileShareInput
--
-- /See:/ 'mkCreateSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { clientToken :: Types.ClientToken
    -- ^ A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
  , gatewayARN :: Types.GatewayARN
    -- ^ The ARN of the file gateway on which you want to create a file share.
  , role' :: Types.Role
    -- ^ The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
  , locationARN :: Types.LocationARN
    -- ^ The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
  , accessBasedEnumeration :: Core.Maybe Core.Bool
    -- ^ The files and folders on this share will only be visible to users with read access.
  , adminUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ .
--
-- /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
  , auditDestinationARN :: Core.Maybe Types.AuditDestinationARN
    -- ^ The Amazon Resource Name (ARN) of the storage used for the audit logs.
  , authentication :: Core.Maybe Types.Authentication
    -- ^ The authentication method that users use to access the file share. The default is @ActiveDirectory@ .
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@ 
  , cacheAttributes :: Core.Maybe Types.CacheAttributes
    -- ^ Refresh cache information.
  , caseSensitivity :: Core.Maybe Types.CaseSensitivity
    -- ^ The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
  , defaultStorageClass :: Core.Maybe Types.DefaultStorageClass
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
    -- ^ A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
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
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
  , validUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSMBFileShare' value with any optional fields omitted.
mkCreateSMBFileShare
    :: Types.ClientToken -- ^ 'clientToken'
    -> Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.Role -- ^ 'role\''
    -> Types.LocationARN -- ^ 'locationARN'
    -> CreateSMBFileShare
mkCreateSMBFileShare clientToken gatewayARN role' locationARN
  = CreateSMBFileShare'{clientToken, gatewayARN, role', locationARN,
                        accessBasedEnumeration = Core.Nothing,
                        adminUserList = Core.Nothing, auditDestinationARN = Core.Nothing,
                        authentication = Core.Nothing, cacheAttributes = Core.Nothing,
                        caseSensitivity = Core.Nothing, defaultStorageClass = Core.Nothing,
                        fileShareName = Core.Nothing, guessMIMETypeEnabled = Core.Nothing,
                        invalidUserList = Core.Nothing, kMSEncrypted = Core.Nothing,
                        kMSKey = Core.Nothing, notificationPolicy = Core.Nothing,
                        objectACL = Core.Nothing, readOnly = Core.Nothing,
                        requesterPays = Core.Nothing, sMBACLEnabled = Core.Nothing,
                        tags = Core.Nothing, validUserList = Core.Nothing}

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsClientToken :: Lens.Lens' CreateSMBFileShare Types.ClientToken
csmbfsClientToken = Lens.field @"clientToken"
{-# INLINEABLE csmbfsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The ARN of the file gateway on which you want to create a file share.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsGatewayARN :: Lens.Lens' CreateSMBFileShare Types.GatewayARN
csmbfsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE csmbfsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsRole :: Lens.Lens' CreateSMBFileShare Types.Role
csmbfsRole = Lens.field @"role'"
{-# INLINEABLE csmbfsRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsLocationARN :: Lens.Lens' CreateSMBFileShare Types.LocationARN
csmbfsLocationARN = Lens.field @"locationARN"
{-# INLINEABLE csmbfsLocationARN #-}
{-# DEPRECATED locationARN "Use generic-lens or generic-optics with 'locationARN' instead"  #-}

-- | The files and folders on this share will only be visible to users with read access.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAccessBasedEnumeration :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsAccessBasedEnumeration = Lens.field @"accessBasedEnumeration"
{-# INLINEABLE csmbfsAccessBasedEnumeration #-}
{-# DEPRECATED accessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead"  #-}

-- | A list of users or groups in the Active Directory that will be granted administrator privileges on the file share. These users can do all file operations as the super-user. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ .
--
-- /Important:/ Use this option very carefully, because any user in this list can do anything they like on the file share, regardless of file permissions.
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAdminUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.FileShareUser])
csmbfsAdminUserList = Lens.field @"adminUserList"
{-# INLINEABLE csmbfsAdminUserList #-}
{-# DEPRECATED adminUserList "Use generic-lens or generic-optics with 'adminUserList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAuditDestinationARN :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.AuditDestinationARN)
csmbfsAuditDestinationARN = Lens.field @"auditDestinationARN"
{-# INLINEABLE csmbfsAuditDestinationARN #-}
{-# DEPRECATED auditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead"  #-}

-- | The authentication method that users use to access the file share. The default is @ActiveDirectory@ .
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@ 
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsAuthentication :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.Authentication)
csmbfsAuthentication = Lens.field @"authentication"
{-# INLINEABLE csmbfsAuthentication #-}
{-# DEPRECATED authentication "Use generic-lens or generic-optics with 'authentication' instead"  #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsCacheAttributes :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.CacheAttributes)
csmbfsCacheAttributes = Lens.field @"cacheAttributes"
{-# INLINEABLE csmbfsCacheAttributes #-}
{-# DEPRECATED cacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead"  #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsCaseSensitivity :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.CaseSensitivity)
csmbfsCaseSensitivity = Lens.field @"caseSensitivity"
{-# INLINEABLE csmbfsCaseSensitivity #-}
{-# DEPRECATED caseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead"  #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsDefaultStorageClass :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.DefaultStorageClass)
csmbfsDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# INLINEABLE csmbfsDefaultStorageClass #-}
{-# DEPRECATED defaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead"  #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsFileShareName :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.FileShareName)
csmbfsFileShareName = Lens.field @"fileShareName"
{-# INLINEABLE csmbfsFileShareName #-}
{-# DEPRECATED fileShareName "Use generic-lens or generic-optics with 'fileShareName' instead"  #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsGuessMIMETypeEnabled :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# INLINEABLE csmbfsGuessMIMETypeEnabled #-}
{-# DEPRECATED guessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead"  #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsInvalidUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.FileShareUser])
csmbfsInvalidUserList = Lens.field @"invalidUserList"
{-# INLINEABLE csmbfsInvalidUserList #-}
{-# DEPRECATED invalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsKMSEncrypted :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE csmbfsKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsKMSKey :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.KMSKey)
csmbfsKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE csmbfsKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsNotificationPolicy :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.NotificationPolicy)
csmbfsNotificationPolicy = Lens.field @"notificationPolicy"
{-# INLINEABLE csmbfsNotificationPolicy #-}
{-# DEPRECATED notificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead"  #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsObjectACL :: Lens.Lens' CreateSMBFileShare (Core.Maybe Types.ObjectACL)
csmbfsObjectACL = Lens.field @"objectACL"
{-# INLINEABLE csmbfsObjectACL #-}
{-# DEPRECATED objectACL "Use generic-lens or generic-optics with 'objectACL' instead"  #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsReadOnly :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsReadOnly = Lens.field @"readOnly"
{-# INLINEABLE csmbfsReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsRequesterPays :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsRequesterPays = Lens.field @"requesterPays"
{-# INLINEABLE csmbfsRequesterPays #-}
{-# DEPRECATED requesterPays "Use generic-lens or generic-optics with 'requesterPays' instead"  #-}

-- | Set this value to @true@ to enable access control list (ACL) on the SMB file share. Set it to @false@ to map file and directory permissions to the POSIX permissions.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsSMBACLEnabled :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
csmbfsSMBACLEnabled = Lens.field @"sMBACLEnabled"
{-# INLINEABLE csmbfsSMBACLEnabled #-}
{-# DEPRECATED sMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead"  #-}

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsTags :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.Tag])
csmbfsTags = Lens.field @"tags"
{-# INLINEABLE csmbfsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file < > share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsValidUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Types.FileShareUser])
csmbfsValidUserList = Lens.field @"validUserList"
{-# INLINEABLE csmbfsValidUserList #-}
{-# DEPRECATED validUserList "Use generic-lens or generic-optics with 'validUserList' instead"  #-}

instance Core.ToQuery CreateSMBFileShare where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSMBFileShare where
        toHeaders CreateSMBFileShare{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.CreateSMBFileShare")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSMBFileShare where
        toJSON CreateSMBFileShare{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientToken" Core..= clientToken),
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
                  ("ValidUserList" Core..=) Core.<$> validUserList])

instance Core.AWSRequest CreateSMBFileShare where
        type Rs CreateSMBFileShare = CreateSMBFileShareResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSMBFileShareResponse' Core.<$>
                   (x Core..:? "FileShareARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | CreateSMBFileShareOutput
--
-- /See:/ 'mkCreateSMBFileShareResponse' smart constructor.
data CreateSMBFileShareResponse = CreateSMBFileShareResponse'
  { fileShareARN :: Core.Maybe Types.FileShareARN
    -- ^ The Amazon Resource Name (ARN) of the newly created file share.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSMBFileShareResponse' value with any optional fields omitted.
mkCreateSMBFileShareResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSMBFileShareResponse
mkCreateSMBFileShareResponse responseStatus
  = CreateSMBFileShareResponse'{fileShareARN = Core.Nothing,
                                responseStatus}

-- | The Amazon Resource Name (ARN) of the newly created file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsrrsFileShareARN :: Lens.Lens' CreateSMBFileShareResponse (Core.Maybe Types.FileShareARN)
csmbfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE csmbfsrrsFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csmbfsrrsResponseStatus :: Lens.Lens' CreateSMBFileShareResponse Core.Int
csmbfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csmbfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
