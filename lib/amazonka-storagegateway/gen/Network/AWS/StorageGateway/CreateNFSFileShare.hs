{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateNFSFileShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Network File System (NFS) file share on an existing file gateway. In Storage Gateway, a file share is a file system mount point backed by Amazon S3 cloud storage. Storage Gateway exposes file shares using an NFS interface. This operation is only supported for file gateways.
--
-- /Important:/ File gateway requires AWS Security Token Service (AWS STS) to be activated to enable you to create a file share. Make sure AWS STS is activated in the AWS Region you are creating your file gateway in. If AWS STS is not activated in the AWS Region, activate it. For information about how to activate AWS STS, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region> in the /AWS Identity and Access Management User Guide/ .
-- File gateway does not support creating hard or symbolic links on a file share.
module Network.AWS.StorageGateway.CreateNFSFileShare
    (
    -- * Creating a request
      CreateNFSFileShare (..)
    , mkCreateNFSFileShare
    -- ** Request lenses
    , cnfsfsClientToken
    , cnfsfsGatewayARN
    , cnfsfsRole
    , cnfsfsLocationARN
    , cnfsfsCacheAttributes
    , cnfsfsClientList
    , cnfsfsDefaultStorageClass
    , cnfsfsFileShareName
    , cnfsfsGuessMIMETypeEnabled
    , cnfsfsKMSEncrypted
    , cnfsfsKMSKey
    , cnfsfsNFSFileShareDefaults
    , cnfsfsNotificationPolicy
    , cnfsfsObjectACL
    , cnfsfsReadOnly
    , cnfsfsRequesterPays
    , cnfsfsSquash
    , cnfsfsTags

    -- * Destructuring the response
    , CreateNFSFileShareResponse (..)
    , mkCreateNFSFileShareResponse
    -- ** Response lenses
    , cnfsfsrrsFileShareARN
    , cnfsfsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | CreateNFSFileShareInput
--
-- /See:/ 'mkCreateNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
  { clientToken :: Types.ClientToken
    -- ^ A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
  , gatewayARN :: Types.GatewayARN
    -- ^ The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
  , role' :: Types.Role
    -- ^ The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
  , locationARN :: Types.LocationARN
    -- ^ The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
  , cacheAttributes :: Core.Maybe Types.CacheAttributes
    -- ^ Refresh cache information.
  , clientList :: Core.Maybe (Core.NonEmpty Types.IPV4AddressCIDR)
    -- ^ The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
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
  , kMSEncrypted :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
  , kMSKey :: Core.Maybe Types.KMSKey
    -- ^ The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
  , nFSFileShareDefaults :: Core.Maybe Types.NFSFileShareDefaults
    -- ^ File share default values. Optional.
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
  , squash :: Core.Maybe Types.Squash
    -- ^ A value that maps a user to anonymous user.
--
-- Valid values are the following:
--
--     * @RootSquash@ : Only root is mapped to anonymous user.
--
--
--     * @NoSquash@ : No one is mapped to anonymous user.
--
--
--     * @AllSquash@ : Everyone is mapped to anonymous user.
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNFSFileShare' value with any optional fields omitted.
mkCreateNFSFileShare
    :: Types.ClientToken -- ^ 'clientToken'
    -> Types.GatewayARN -- ^ 'gatewayARN'
    -> Types.Role -- ^ 'role\''
    -> Types.LocationARN -- ^ 'locationARN'
    -> CreateNFSFileShare
mkCreateNFSFileShare clientToken gatewayARN role' locationARN
  = CreateNFSFileShare'{clientToken, gatewayARN, role', locationARN,
                        cacheAttributes = Core.Nothing, clientList = Core.Nothing,
                        defaultStorageClass = Core.Nothing, fileShareName = Core.Nothing,
                        guessMIMETypeEnabled = Core.Nothing, kMSEncrypted = Core.Nothing,
                        kMSKey = Core.Nothing, nFSFileShareDefaults = Core.Nothing,
                        notificationPolicy = Core.Nothing, objectACL = Core.Nothing,
                        readOnly = Core.Nothing, requesterPays = Core.Nothing,
                        squash = Core.Nothing, tags = Core.Nothing}

-- | A unique string value that you supply that is used by file gateway to ensure idempotent file share creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsClientToken :: Lens.Lens' CreateNFSFileShare Types.ClientToken
cnfsfsClientToken = Lens.field @"clientToken"
{-# INLINEABLE cnfsfsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to create a file share.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsGatewayARN :: Lens.Lens' CreateNFSFileShare Types.GatewayARN
cnfsfsGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE cnfsfsGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file gateway assumes when it accesses the underlying storage.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsRole :: Lens.Lens' CreateNFSFileShare Types.Role
cnfsfsRole = Lens.field @"role'"
{-# INLINEABLE cnfsfsRole #-}
{-# DEPRECATED role' "Use generic-lens or generic-optics with 'role'' instead"  #-}

-- | The ARN of the backend storage used for storing file data. A prefix name can be added to the S3 bucket name. It must end with a "/".
--
-- /Note:/ Consider using 'locationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsLocationARN :: Lens.Lens' CreateNFSFileShare Types.LocationARN
cnfsfsLocationARN = Lens.field @"locationARN"
{-# INLINEABLE cnfsfsLocationARN #-}
{-# DEPRECATED locationARN "Use generic-lens or generic-optics with 'locationARN' instead"  #-}

-- | Refresh cache information.
--
-- /Note:/ Consider using 'cacheAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsCacheAttributes :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.CacheAttributes)
cnfsfsCacheAttributes = Lens.field @"cacheAttributes"
{-# INLINEABLE cnfsfsCacheAttributes #-}
{-# DEPRECATED cacheAttributes "Use generic-lens or generic-optics with 'cacheAttributes' instead"  #-}

-- | The list of clients that are allowed to access the file gateway. The list must contain either valid IP addresses or valid CIDR blocks.
--
-- /Note:/ Consider using 'clientList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsClientList :: Lens.Lens' CreateNFSFileShare (Core.Maybe (Core.NonEmpty Types.IPV4AddressCIDR))
cnfsfsClientList = Lens.field @"clientList"
{-# INLINEABLE cnfsfsClientList #-}
{-# DEPRECATED clientList "Use generic-lens or generic-optics with 'clientList' instead"  #-}

-- | The default storage class for objects put into an Amazon S3 bucket by the file gateway. The default value is @S3_INTELLIGENT_TIERING@ . Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ | @S3_STANDARD_IA@ | @S3_ONEZONE_IA@ 
--
-- /Note:/ Consider using 'defaultStorageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsDefaultStorageClass :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.StorageClass)
cnfsfsDefaultStorageClass = Lens.field @"defaultStorageClass"
{-# INLINEABLE cnfsfsDefaultStorageClass #-}
{-# DEPRECATED defaultStorageClass "Use generic-lens or generic-optics with 'defaultStorageClass' instead"  #-}

-- | The name of the file share. Optional.
--
-- /Note:/ Consider using 'fileShareName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsFileShareName :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.FileShareName)
cnfsfsFileShareName = Lens.field @"fileShareName"
{-# INLINEABLE cnfsfsFileShareName #-}
{-# DEPRECATED fileShareName "Use generic-lens or generic-optics with 'fileShareName' instead"  #-}

-- | A value that enables guessing of the MIME type for uploaded objects based on file extensions. Set this value to @true@ to enable MIME type guessing, otherwise set to @false@ . The default value is @true@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'guessMIMETypeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsGuessMIMETypeEnabled :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
cnfsfsGuessMIMETypeEnabled = Lens.field @"guessMIMETypeEnabled"
{-# INLINEABLE cnfsfsGuessMIMETypeEnabled #-}
{-# DEPRECATED guessMIMETypeEnabled "Use generic-lens or generic-optics with 'guessMIMETypeEnabled' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsKMSEncrypted :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
cnfsfsKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE cnfsfsKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsKMSKey :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.KMSKey)
cnfsfsKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE cnfsfsKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | File share default values. Optional.
--
-- /Note:/ Consider using 'nFSFileShareDefaults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsNFSFileShareDefaults :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.NFSFileShareDefaults)
cnfsfsNFSFileShareDefaults = Lens.field @"nFSFileShareDefaults"
{-# INLINEABLE cnfsfsNFSFileShareDefaults #-}
{-# DEPRECATED nFSFileShareDefaults "Use generic-lens or generic-optics with 'nFSFileShareDefaults' instead"  #-}

-- | The notification policy of the file share.
--
-- /Note:/ Consider using 'notificationPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsNotificationPolicy :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.NotificationPolicy)
cnfsfsNotificationPolicy = Lens.field @"notificationPolicy"
{-# INLINEABLE cnfsfsNotificationPolicy #-}
{-# DEPRECATED notificationPolicy "Use generic-lens or generic-optics with 'notificationPolicy' instead"  #-}

-- | A value that sets the access control list (ACL) permission for objects in the S3 bucket that a file gateway puts objects into. The default value is @private@ .
--
-- /Note:/ Consider using 'objectACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsObjectACL :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.ObjectACL)
cnfsfsObjectACL = Lens.field @"objectACL"
{-# INLINEABLE cnfsfsObjectACL #-}
{-# DEPRECATED objectACL "Use generic-lens or generic-optics with 'objectACL' instead"  #-}

-- | A value that sets the write status of a file share. Set this value to @true@ to set the write status to read-only, otherwise set to @false@ .
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'readOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsReadOnly :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
cnfsfsReadOnly = Lens.field @"readOnly"
{-# INLINEABLE cnfsfsReadOnly #-}
{-# DEPRECATED readOnly "Use generic-lens or generic-optics with 'readOnly' instead"  #-}

-- | A value that sets who pays the cost of the request and the cost associated with data download from the S3 bucket. If this value is set to @true@ , the requester pays the costs; otherwise, the S3 bucket owner pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'requesterPays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsRequesterPays :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
cnfsfsRequesterPays = Lens.field @"requesterPays"
{-# INLINEABLE cnfsfsRequesterPays #-}
{-# DEPRECATED requesterPays "Use generic-lens or generic-optics with 'requesterPays' instead"  #-}

-- | A value that maps a user to anonymous user.
--
-- Valid values are the following:
--
--     * @RootSquash@ : Only root is mapped to anonymous user.
--
--
--     * @NoSquash@ : No one is mapped to anonymous user.
--
--
--     * @AllSquash@ : Everyone is mapped to anonymous user.
--
--
--
-- /Note:/ Consider using 'squash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsSquash :: Lens.Lens' CreateNFSFileShare (Core.Maybe Types.Squash)
cnfsfsSquash = Lens.field @"squash"
{-# INLINEABLE cnfsfsSquash #-}
{-# DEPRECATED squash "Use generic-lens or generic-optics with 'squash' instead"  #-}

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsTags :: Lens.Lens' CreateNFSFileShare (Core.Maybe [Types.Tag])
cnfsfsTags = Lens.field @"tags"
{-# INLINEABLE cnfsfsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateNFSFileShare where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateNFSFileShare where
        toHeaders CreateNFSFileShare{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.CreateNFSFileShare")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateNFSFileShare where
        toJSON CreateNFSFileShare{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientToken" Core..= clientToken),
                  Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("Role" Core..= role'),
                  Core.Just ("LocationARN" Core..= locationARN),
                  ("CacheAttributes" Core..=) Core.<$> cacheAttributes,
                  ("ClientList" Core..=) Core.<$> clientList,
                  ("DefaultStorageClass" Core..=) Core.<$> defaultStorageClass,
                  ("FileShareName" Core..=) Core.<$> fileShareName,
                  ("GuessMIMETypeEnabled" Core..=) Core.<$> guessMIMETypeEnabled,
                  ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
                  ("KMSKey" Core..=) Core.<$> kMSKey,
                  ("NFSFileShareDefaults" Core..=) Core.<$> nFSFileShareDefaults,
                  ("NotificationPolicy" Core..=) Core.<$> notificationPolicy,
                  ("ObjectACL" Core..=) Core.<$> objectACL,
                  ("ReadOnly" Core..=) Core.<$> readOnly,
                  ("RequesterPays" Core..=) Core.<$> requesterPays,
                  ("Squash" Core..=) Core.<$> squash,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateNFSFileShare where
        type Rs CreateNFSFileShare = CreateNFSFileShareResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateNFSFileShareResponse' Core.<$>
                   (x Core..:? "FileShareARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | CreateNFSFileShareOutput
--
-- /See:/ 'mkCreateNFSFileShareResponse' smart constructor.
data CreateNFSFileShareResponse = CreateNFSFileShareResponse'
  { fileShareARN :: Core.Maybe Types.FileShareARN
    -- ^ The Amazon Resource Name (ARN) of the newly created file share.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNFSFileShareResponse' value with any optional fields omitted.
mkCreateNFSFileShareResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNFSFileShareResponse
mkCreateNFSFileShareResponse responseStatus
  = CreateNFSFileShareResponse'{fileShareARN = Core.Nothing,
                                responseStatus}

-- | The Amazon Resource Name (ARN) of the newly created file share.
--
-- /Note:/ Consider using 'fileShareARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsrrsFileShareARN :: Lens.Lens' CreateNFSFileShareResponse (Core.Maybe Types.FileShareARN)
cnfsfsrrsFileShareARN = Lens.field @"fileShareARN"
{-# INLINEABLE cnfsfsrrsFileShareARN #-}
{-# DEPRECATED fileShareARN "Use generic-lens or generic-optics with 'fileShareARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnfsfsrrsResponseStatus :: Lens.Lens' CreateNFSFileShareResponse Core.Int
cnfsfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cnfsfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
