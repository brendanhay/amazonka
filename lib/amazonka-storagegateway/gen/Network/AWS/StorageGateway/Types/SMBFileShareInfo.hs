{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.SMBFileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.SMBFileShareInfo
  ( SMBFileShareInfo (..)
  -- * Smart constructor
  , mkSMBFileShareInfo
  -- * Lenses
  , smbfsiAccessBasedEnumeration
  , smbfsiAdminUserList
  , smbfsiAuditDestinationARN
  , smbfsiAuthentication
  , smbfsiCacheAttributes
  , smbfsiCaseSensitivity
  , smbfsiDefaultStorageClass
  , smbfsiFileShareARN
  , smbfsiFileShareId
  , smbfsiFileShareName
  , smbfsiFileShareStatus
  , smbfsiGatewayARN
  , smbfsiGuessMIMETypeEnabled
  , smbfsiInvalidUserList
  , smbfsiKMSEncrypted
  , smbfsiKMSKey
  , smbfsiLocationARN
  , smbfsiNotificationPolicy
  , smbfsiObjectACL
  , smbfsiPath
  , smbfsiReadOnly
  , smbfsiRequesterPays
  , smbfsiRole
  , smbfsiSMBACLEnabled
  , smbfsiTags
  , smbfsiValidUserList
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.AuditDestinationARN as Types
import qualified Network.AWS.StorageGateway.Types.Authentication as Types
import qualified Network.AWS.StorageGateway.Types.CacheAttributes as Types
import qualified Network.AWS.StorageGateway.Types.CaseSensitivity as Types
import qualified Network.AWS.StorageGateway.Types.FileShareARN as Types
import qualified Network.AWS.StorageGateway.Types.FileShareId as Types
import qualified Network.AWS.StorageGateway.Types.FileShareName as Types
import qualified Network.AWS.StorageGateway.Types.FileShareStatus as Types
import qualified Network.AWS.StorageGateway.Types.FileShareUser as Types
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types
import qualified Network.AWS.StorageGateway.Types.KMSKey as Types
import qualified Network.AWS.StorageGateway.Types.LocationARN as Types
import qualified Network.AWS.StorageGateway.Types.NotificationPolicy as Types
import qualified Network.AWS.StorageGateway.Types.ObjectACL as Types
import qualified Network.AWS.StorageGateway.Types.Path as Types
import qualified Network.AWS.StorageGateway.Types.Role as Types
import qualified Network.AWS.StorageGateway.Types.StorageClass as Types
import qualified Network.AWS.StorageGateway.Types.Tag as Types

-- | The Windows file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported for file gateways.
--
-- /See:/ 'mkSMBFileShareInfo' smart constructor.
data SMBFileShareInfo = SMBFileShareInfo'
  { accessBasedEnumeration :: Core.Maybe Core.Bool
    -- ^ Indicates whether @AccessBasedEnumeration@ is enabled.
  , adminUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
  , auditDestinationARN :: Core.Maybe Types.AuditDestinationARN
    -- ^ The Amazon Resource Name (ARN) of the storage used for the audit logs.
  , authentication :: Core.Maybe Types.Authentication
  , cacheAttributes :: Core.Maybe Types.CacheAttributes
    -- ^ Refresh cache information.
  , caseSensitivity :: Core.Maybe Types.CaseSensitivity
    -- ^ The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
  , defaultStorageClass :: Core.Maybe Types.StorageClass
    -- ^ The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
  , fileShareARN :: Core.Maybe Types.FileShareARN
  , fileShareId :: Core.Maybe Types.FileShareId
  , fileShareName :: Core.Maybe Types.FileShareName
    -- ^ The name of the file share. Optional.
  , fileShareStatus :: Core.Maybe Types.FileShareStatus
  , gatewayARN :: Core.Maybe Types.GatewayARN
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
  , locationARN :: Core.Maybe Types.LocationARN
  , notificationPolicy :: Core.Maybe Types.NotificationPolicy
    -- ^ The notification policy of the file share.
  , objectACL :: Core.Maybe Types.ObjectACL
  , path :: Core.Maybe Types.Path
    -- ^ The file share path used by the SMB client to identify the mount point.
  , readOnly :: Core.Maybe Core.Bool
    -- ^ A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
  , requesterPays :: Core.Maybe Core.Bool
    -- ^ A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
  , role' :: Core.Maybe Types.Role
  , sMBACLEnabled :: Core.Maybe Core.Bool
    -- ^ If this value is set to @true@ , it indicates that access control list (ACL) is enabled on the SMB file share. If it is set to @false@ , it indicates that file and directory permissions are mapped to the POSIX permission.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags assigned to the SMB file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
  , validUserList :: Core.Maybe [Types.FileShareUser]
    -- ^ A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SMBFileShareInfo' value with any optional fields omitted.
mkSMBFileShareInfo
    :: SMBFileShareInfo
mkSMBFileShareInfo
  = SMBFileShareInfo'{accessBasedEnumeration = Core.Nothing,
                      adminUserList = Core.Nothing, auditDestinationARN = Core.Nothing,
                      authentication = Core.Nothing, cacheAttributes = Core.Nothing,
                      caseSensitivity = Core.Nothing, defaultStorageClass = Core.Nothing,
                      fileShareARN = Core.Nothing, fileShareId = Core.Nothing,
                      fileShareName = Core.Nothing, fileShareStatus = Core.Nothing,
                      gatewayARN = Core.Nothing, guessMIMETypeEnabled = Core.Nothing,
                      invalidUserList = Core.Nothing, kMSEncrypted = Core.Nothing,
                      kMSKey = Core.Nothing, locationARN = Core.Nothing,
                      notificationPolicy = Core.Nothing, objectACL = Core.Nothing,
                      path = Core.Nothing, readOnly = Core.Nothing,
                      requesterPays = Core.Nothing, role' = Core.Nothing,
                      sMBACLEnabled = Core.Nothing, tags = Core.Nothing,
                      validUserList = Core.Nothing}

-- | Indicates whether @AccessBasedEnumeration@ is enabled.
--
-- /Note:/ Consider using 'accessBasedEnumeration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAccessBasedEnumeration :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
smbfsiAccessBasedEnumeration = Lens.field @"accessBasedEnumeration"
{-# INLINEABLE smbfsiAccessBasedEnumeration #-}
{-# DEPRECATED accessBasedEnumeration "Use generic-lens or generic-optics with 'accessBasedEnumeration' instead"  #-}

-- | A list of users or groups in the Active Directory that have administrator rights to the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'adminUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAdminUserList :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Types.FileShareUser])
smbfsiAdminUserList = Lens.field @"adminUserList"
{-# INLINEABLE smbfsiAdminUserList #-}
{-# DEPRECATED adminUserList "Use generic-lens or generic-optics with 'adminUserList' instead"  #-}

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- /Note:/ Consider using 'auditDestinationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAuditDestinationARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.AuditDestinationARN)
smbfsiAuditDestinationARN = Lens.field @"auditDestinationARN"
{-# INLINEABLE smbfsiAuditDestinationARN #-}
{-# DEPRECATED auditDestinationARN "Use generic-lens or generic-optics with 'auditDestinationARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiAuthentication :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.Authentication)
smbfsiAuthentication = Lens.field @"authentication"
{-# INLINEABLE smbfsiAuthentication #-}
{-# DEPRECATED authentication "Use generic-lens or generic-optics with 'authentication' instead"  #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiCacheAttributes :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.CacheAttributes)
smbfsiCacheAttributes = Lens.field @"cacheAttributes"
{-# INLINEABLE smbfsiCacheAttributes #-}
{-# DEPRECATED cacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead"  #-}

-- | The case of an object name in an Amazon S3 bucket. For @ClientSpecified@ , the client determines the case sensitivity. For @CaseSensitive@ , the gateway determines the case sensitivity. The default value is @ClientSpecified@ .
--
-- /Note:/ Consider using 'caseSensitivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiCaseSensitivity :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.CaseSensitivity)
smbfsiCaseSensitivity = Lens.field @"caseSensitivity"
{-# INLINEABLE smbfsiCaseSensitivity #-}
{-# DEPRECATED caseSensitivity "Use generic-lens or generic-optics with 'caseSensitivity' instead"  #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiDefaultStorageClass :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.StorageClass)
smbfsiDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# INLINEABLE smbfsiDefaultStorageClass #-}
{-# DEPRECATED defaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.FileShareARN)
smbfsiFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE smbfsiFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareId :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.FileShareId)
smbfsiFileShareId = Lens.field @"fileShareId"
{-# INLINEABLE smbfsiFileShareId #-}
{-# DEPRECATED fileShareId "Use generic-lens or generic-optics with 'fileShareId' instead"  #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareName :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.FileShareName)
smbfsiFileShareName = Lens.field @"fileShareName"
{-# INLINEABLE smbfsiFileShareName #-}
{-# DEPRECATED fileShareName "Use generic-lens or generic-optics with 'fileShareName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiFileShareStatus :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.FileShareStatus)
smbfsiFileShareStatus = Lens.field @"fileShareStatus"
{-# INLINEABLE smbfsiFileShareStatus #-}
{-# DEPRECATED fileShareStatus "Use generic-lens or generic-optics with 'fileShareStatus' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiGatewayARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.GatewayARN)
smbfsiGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE smbfsiGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiGuessMIMETypeEnabled :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
smbfsiGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# INLINEABLE smbfsiGuessMIMETypeEnabled #-}
{-# DEPRECATED guessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead"  #-}

-- | A list of users or groups in the Active Directory that are not allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'invalidUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiInvalidUserList :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Types.FileShareUser])
smbfsiInvalidUserList = Lens.field @"invalidUserList"
{-# INLINEABLE smbfsiInvalidUserList #-}
{-# DEPRECATED invalidUserList "Use generic-lens or generic-optics with 'invalidUserList' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiKMSEncrypted :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
smbfsiKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE smbfsiKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiKMSKey :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.KMSKey)
smbfsiKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE smbfsiKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiLocationARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.LocationARN)
smbfsiLocationARN = Lens.field @"locationARN"
{-# INLINEABLE smbfsiLocationARN #-}
{-# DEPRECATED locationARN "Use generic-lens or generic-optics with 'locationARN' instead"  #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiNotificationPolicy :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.NotificationPolicy)
smbfsiNotificationPolicy = Lens.field @"notificationPolicy"
{-# INLINEABLE smbfsiNotificationPolicy #-}
{-# DEPRECATED notificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiObjectACL :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.ObjectACL)
smbfsiObjectACL = Lens.field @"objectACL"
{-# INLINEABLE smbfsiObjectACL #-}
{-# DEPRECATED objectACL "Use generic-lens or generic-optics with 'objectACL' instead"  #-}

-- | The file share path used by the SMB client to identify the mount point.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiPath :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.Path)
smbfsiPath = Lens.field @"path"
{-# INLINEABLE smbfsiPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiReadOnly :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
smbfsiReadOnly = Lens.field @"readOnly"
{-# INLINEABLE smbfsiReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiRequesterPays :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
smbfsiRequesterPays = Lens.field @"requesterPays"
{-# INLINEABLE smbfsiRequesterPays #-}
{-# DEPRECATED requesterPays "Use generic-lens or generic-optics with 'requesterPays' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiRole :: Lens.Lens' SMBFileShareInfo (Core.Maybe Types.Role)
smbfsiRole = Lens.field @"role'"
{-# INLINEABLE smbfsiRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | If this value is set to @true@ , it indicates that access control list (ACL) is enabled on the SMB file share. If it is set to @false@ , it indicates that file and directory permissions are mapped to the POSIX permission.
--
-- For more information, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share> in the /AWS Storage Gateway User Guide/ .
--
-- /Note:/ Consider using 'sMBACLEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiSMBACLEnabled :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
smbfsiSMBACLEnabled = Lens.field @"sMBACLEnabled"
{-# INLINEABLE smbfsiSMBACLEnabled #-}
{-# DEPRECATED sMBACLEnabled "Use generic-lens or generic-optics with 'sMBACLEnabled' instead"  #-}

-- | A list of up to 50 tags assigned to the SMB file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiTags :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Types.Tag])
smbfsiTags = Lens.field @"tags"
{-# INLINEABLE smbfsiTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A list of users or groups in the Active Directory that are allowed to access the file share. A group must be prefixed with the @ character. Acceptable formats include: @DOMAIN\User1@ , @user1@ , @@group1@ , and @@DOMAIN\group1@ . Can only be set if Authentication is set to @ActiveDirectory@ .
--
-- /Note:/ Consider using 'validUserList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smbfsiValidUserList :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Types.FileShareUser])
smbfsiValidUserList = Lens.field @"validUserList"
{-# INLINEABLE smbfsiValidUserList #-}
{-# DEPRECATED validUserList "Use generic-lens or generic-optics with 'validUserList' instead"  #-}

instance Core.FromJSON SMBFileShareInfo where
        parseJSON
          = Core.withObject "SMBFileShareInfo" Core.$
              \ x ->
                SMBFileShareInfo' Core.<$>
                  (x Core..:? "AccessBasedEnumeration") Core.<*>
                    x Core..:? "AdminUserList"
                    Core.<*> x Core..:? "AuditDestinationARN"
                    Core.<*> x Core..:? "Authentication"
                    Core.<*> x Core..:? "CacheAttributes"
                    Core.<*> x Core..:? "CaseSensitivity"
                    Core.<*> x Core..:? "DefaultStorageClass"
                    Core.<*> x Core..:? "FileShareARN"
                    Core.<*> x Core..:? "FileShareId"
                    Core.<*> x Core..:? "FileShareName"
                    Core.<*> x Core..:? "FileShareStatus"
                    Core.<*> x Core..:? "GatewayARN"
                    Core.<*> x Core..:? "GuessMIMETypeEnabled"
                    Core.<*> x Core..:? "InvalidUserList"
                    Core.<*> x Core..:? "KMSEncrypted"
                    Core.<*> x Core..:? "KMSKey"
                    Core.<*> x Core..:? "LocationARN"
                    Core.<*> x Core..:? "NotificationPolicy"
                    Core.<*> x Core..:? "ObjectACL"
                    Core.<*> x Core..:? "Path"
                    Core.<*> x Core..:? "ReadOnly"
                    Core.<*> x Core..:? "RequesterPays"
                    Core.<*> x Core..:? "Role"
                    Core.<*> x Core..:? "SMBACLEnabled"
                    Core.<*> x Core..:? "Tags"
                    Core.<*> x Core..:? "ValidUserList"
