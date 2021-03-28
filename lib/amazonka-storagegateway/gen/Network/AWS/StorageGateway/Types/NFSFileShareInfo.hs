{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.NFSFileShareInfo
  ( NFSFileShareInfo (..)
  -- * Smart constructor
  , mkNFSFileShareInfo
  -- * Lenses
  , nfsfsiCacheAttributes
  , nfsfsiClientList
  , nfsfsiDefaultStorageClass
  , nfsfsiFileShareARN
  , nfsfsiFileShareId
  , nfsfsiFileShareName
  , nfsfsiFileShareStatus
  , nfsfsiGatewayARN
  , nfsfsiGuessMIMETypeEnabled
  , nfsfsiKMSEncrypted
  , nfsfsiKMSKey
  , nfsfsiLocationARN
  , nfsfsiNFSFileShareDefaults
  , nfsfsiNotificationPolicy
  , nfsfsiObjectACL
  , nfsfsiPath
  , nfsfsiReadOnly
  , nfsfsiRequesterPays
  , nfsfsiRole
  , nfsfsiSquash
  , nfsfsiTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.CacheAttributes as Types
import qualified Network.AWS.StorageGateway.Types.DefaultStorageClass as Types
import qualified Network.AWS.StorageGateway.Types.FileShareARN as Types
import qualified Network.AWS.StorageGateway.Types.FileShareId as Types
import qualified Network.AWS.StorageGateway.Types.FileShareName as Types
import qualified Network.AWS.StorageGateway.Types.FileShareStatus as Types
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types
import qualified Network.AWS.StorageGateway.Types.IPV4AddressCIDR as Types
import qualified Network.AWS.StorageGateway.Types.KMSKey as Types
import qualified Network.AWS.StorageGateway.Types.LocationARN as Types
import qualified Network.AWS.StorageGateway.Types.NFSFileShareDefaults as Types
import qualified Network.AWS.StorageGateway.Types.NotificationPolicy as Types
import qualified Network.AWS.StorageGateway.Types.ObjectACL as Types
import qualified Network.AWS.StorageGateway.Types.Path as Types
import qualified Network.AWS.StorageGateway.Types.Role as Types
import qualified Network.AWS.StorageGateway.Types.Squash as Types
import qualified Network.AWS.StorageGateway.Types.Tag as Types

-- | The Unix file permissions and ownership information assigned, by default, to native S3 objects when file gateway discovers them in S3 buckets. This operation is only supported in file gateways.
--
-- /See:/ 'mkNFSFileShareInfo' smart constructor.
data NFSFileShareInfo = NFSFileShareInfo'
  { cacheAttributes :: Core.Maybe Types.CacheAttributes
    -- ^ Refresh cache information.
  , clientList :: Core.Maybe (Core.NonEmpty Types.IPV4AddressCIDR)
  , defaultStorageClass :: Core.Maybe Types.DefaultStorageClass
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
  , kMSEncrypted :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
  , kMSKey :: Core.Maybe Types.KMSKey
  , locationARN :: Core.Maybe Types.LocationARN
  , nFSFileShareDefaults :: Core.Maybe Types.NFSFileShareDefaults
  , notificationPolicy :: Core.Maybe Types.NotificationPolicy
    -- ^ The notification policy of the file share.
  , objectACL :: Core.Maybe Types.ObjectACL
  , path :: Core.Maybe Types.Path
  , readOnly :: Core.Maybe Core.Bool
    -- ^ A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
  , requesterPays :: Core.Maybe Core.Bool
    -- ^ A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
  , role' :: Core.Maybe Types.Role
  , squash :: Core.Maybe Types.Squash
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NFSFileShareInfo' value with any optional fields omitted.
mkNFSFileShareInfo
    :: NFSFileShareInfo
mkNFSFileShareInfo
  = NFSFileShareInfo'{cacheAttributes = Core.Nothing,
                      clientList = Core.Nothing, defaultStorageClass = Core.Nothing,
                      fileShareARN = Core.Nothing, fileShareId = Core.Nothing,
                      fileShareName = Core.Nothing, fileShareStatus = Core.Nothing,
                      gatewayARN = Core.Nothing, guessMIMETypeEnabled = Core.Nothing,
                      kMSEncrypted = Core.Nothing, kMSKey = Core.Nothing,
                      locationARN = Core.Nothing, nFSFileShareDefaults = Core.Nothing,
                      notificationPolicy = Core.Nothing, objectACL = Core.Nothing,
                      path = Core.Nothing, readOnly = Core.Nothing,
                      requesterPays = Core.Nothing, role' = Core.Nothing,
                      squash = Core.Nothing, tags = Core.Nothing}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiCacheAttributes :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.CacheAttributes)
nfsfsiCacheAttributes = Lens.field @"cacheAttributes"
{-# INLINEABLE nfsfsiCacheAttributes #-}
{-# DEPRECATED cacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiClientList :: Lens.Lens' NFSFileShareInfo (Core.Maybe (Core.NonEmpty Types.IPV4AddressCIDR))
nfsfsiClientList = Lens.field @"clientList"
{-# INLINEABLE nfsfsiClientList #-}
{-# DEPRECATED clientList "Use generic-lens or generic-optics with 'clientList' instead"  #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiDefaultStorageClass :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.DefaultStorageClass)
nfsfsiDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# INLINEABLE nfsfsiDefaultStorageClass #-}
{-# DEPRECATED defaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareARN :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.FileShareARN)
nfsfsiFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE nfsfsiFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareId :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.FileShareId)
nfsfsiFileShareId = Lens.field @"fileShareId"
{-# INLINEABLE nfsfsiFileShareId #-}
{-# DEPRECATED fileShareId "Use generic-lens or generic-optics with 'fileShareId' instead"  #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareName :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.FileShareName)
nfsfsiFileShareName = Lens.field @"fileShareName"
{-# INLINEABLE nfsfsiFileShareName #-}
{-# DEPRECATED fileShareName "Use generic-lens or generic-optics with 'fileShareName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'fileShareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiFileShareStatus :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.FileShareStatus)
nfsfsiFileShareStatus = Lens.field @"fileShareStatus"
{-# INLINEABLE nfsfsiFileShareStatus #-}
{-# DEPRECATED fileShareStatus "Use generic-lens or generic-optics with 'fileShareStatus' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiGatewayARN :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.GatewayARN)
nfsfsiGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE nfsfsiGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiGuessMIMETypeEnabled :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nfsfsiGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# INLINEABLE nfsfsiGuessMIMETypeEnabled #-}
{-# DEPRECATED guessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiKMSEncrypted :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nfsfsiKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE nfsfsiKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiKMSKey :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.KMSKey)
nfsfsiKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE nfsfsiKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiLocationARN :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.LocationARN)
nfsfsiLocationARN = Lens.field @"locationARN"
{-# INLINEABLE nfsfsiLocationARN #-}
{-# DEPRECATED locationARN "Use generic-lens or generic-optics with 'locationARN' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nFSFileShareDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiNFSFileShareDefaults :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.NFSFileShareDefaults)
nfsfsiNFSFileShareDefaults = Lens.field @"nFSFileShareDefaults"
{-# INLINEABLE nfsfsiNFSFileShareDefaults #-}
{-# DEPRECATED nFSFileShareDefaults "Use generic-lens or generic-optics with 'nFSFileShareDefaults' instead"  #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiNotificationPolicy :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.NotificationPolicy)
nfsfsiNotificationPolicy = Lens.field @"notificationPolicy"
{-# INLINEABLE nfsfsiNotificationPolicy #-}
{-# DEPRECATED notificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiObjectACL :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.ObjectACL)
nfsfsiObjectACL = Lens.field @"objectACL"
{-# INLINEABLE nfsfsiObjectACL #-}
{-# DEPRECATED objectACL "Use generic-lens or generic-optics with 'objectACL' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiPath :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.Path)
nfsfsiPath = Lens.field @"path"
{-# INLINEABLE nfsfsiPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiReadOnly :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nfsfsiReadOnly = Lens.field @"readOnly"
{-# INLINEABLE nfsfsiReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiRequesterPays :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nfsfsiRequesterPays = Lens.field @"requesterPays"
{-# INLINEABLE nfsfsiRequesterPays #-}
{-# DEPRECATED requesterPays "Use generic-lens or generic-optics with 'requesterPays' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiRole :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.Role)
nfsfsiRole = Lens.field @"role'"
{-# INLINEABLE nfsfsiRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'squash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiSquash :: Lens.Lens' NFSFileShareInfo (Core.Maybe Types.Squash)
nfsfsiSquash = Lens.field @"squash"
{-# INLINEABLE nfsfsiSquash #-}
{-# DEPRECATED squash "Use generic-lens or generic-optics with 'squash' instead"  #-}

-- | A list of up to 50 tags assigned to the NFS file share, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsiTags :: Lens.Lens' NFSFileShareInfo (Core.Maybe [Types.Tag])
nfsfsiTags = Lens.field @"tags"
{-# INLINEABLE nfsfsiTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON NFSFileShareInfo where
        parseJSON
          = Core.withObject "NFSFileShareInfo" Core.$
              \ x ->
                NFSFileShareInfo' Core.<$>
                  (x Core..:? "CacheAttributes") Core.<*> x Core..:? "ClientList"
                    Core.<*> x Core..:? "DefaultStorageClass"
                    Core.<*> x Core..:? "FileShareARN"
                    Core.<*> x Core..:? "FileShareId"
                    Core.<*> x Core..:? "FileShareName"
                    Core.<*> x Core..:? "FileShareStatus"
                    Core.<*> x Core..:? "GatewayARN"
                    Core.<*> x Core..:? "GuessMIMETypeEnabled"
                    Core.<*> x Core..:? "KMSEncrypted"
                    Core.<*> x Core..:? "KMSKey"
                    Core.<*> x Core..:? "LocationARN"
                    Core.<*> x Core..:? "NFSFileShareDefaults"
                    Core.<*> x Core..:? "NotificationPolicy"
                    Core.<*> x Core..:? "ObjectACL"
                    Core.<*> x Core..:? "Path"
                    Core.<*> x Core..:? "ReadOnly"
                    Core.<*> x Core..:? "RequesterPays"
                    Core.<*> x Core..:? "Role"
                    Core.<*> x Core..:? "Squash"
                    Core.<*> x Core..:? "Tags"
