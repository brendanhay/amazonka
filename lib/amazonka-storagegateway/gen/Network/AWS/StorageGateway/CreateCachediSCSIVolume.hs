{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cached volume on a specified cached volume gateway. This operation is only supported in the cached volume gateway type.
--
-- In the request, you must specify the gateway, size of the volume in bytes, the iSCSI target name, an IP address on which to expose the target, and a unique client token. In response, the gateway creates the volume and returns information about it. This information includes the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.
-- Optionally, you can provide the ARN for an existing volume as the @SourceVolumeARN@ for this cached volume, which creates an exact copy of the existing volume’s latest recovery point. The @VolumeSizeInBytes@ value must be equal to or larger than the size of the copied volume, in bytes.
module Network.AWS.StorageGateway.CreateCachediSCSIVolume
    (
    -- * Creating a request
      CreateCachediSCSIVolume (..)
    , mkCreateCachediSCSIVolume
    -- ** Request lenses
    , ccscsivGatewayARN
    , ccscsivVolumeSizeInBytes
    , ccscsivTargetName
    , ccscsivNetworkInterfaceId
    , ccscsivClientToken
    , ccscsivKMSEncrypted
    , ccscsivKMSKey
    , ccscsivSnapshotId
    , ccscsivSourceVolumeARN
    , ccscsivTags

    -- * Destructuring the response
    , CreateCachediSCSIVolumeResponse (..)
    , mkCreateCachediSCSIVolumeResponse
    -- ** Response lenses
    , ccscsivrrsTargetARN
    , ccscsivrrsVolumeARN
    , ccscsivrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | /See:/ 'mkCreateCachediSCSIVolume' smart constructor.
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'
  { gatewayARN :: Types.GatewayARN
  , volumeSizeInBytes :: Core.Integer
    -- ^ The size of the volume in bytes.
  , targetName :: Types.TargetName
    -- ^ The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
  , networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
  , clientToken :: Types.ClientToken
    -- ^ A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
  , kMSEncrypted :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
  , kMSKey :: Core.Maybe Types.KMSKey
    -- ^ The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
  , snapshotId :: Core.Maybe Types.SnapshotId
    -- ^ The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new cached volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
  , sourceVolumeARN :: Core.Maybe Types.VolumeARN
    -- ^ The ARN for an existing volume. Specifying this ARN makes the new volume into an exact copy of the specified existing volume's latest recovery point. The @VolumeSizeInBytes@ value for this new volume must be equal to or larger than the size of the existing volume, in bytes.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that you can assign to a cached volume. Each tag is a key-value pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCachediSCSIVolume' value with any optional fields omitted.
mkCreateCachediSCSIVolume
    :: Types.GatewayARN -- ^ 'gatewayARN'
    -> Core.Integer -- ^ 'volumeSizeInBytes'
    -> Types.TargetName -- ^ 'targetName'
    -> Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> Types.ClientToken -- ^ 'clientToken'
    -> CreateCachediSCSIVolume
mkCreateCachediSCSIVolume gatewayARN volumeSizeInBytes targetName
  networkInterfaceId clientToken
  = CreateCachediSCSIVolume'{gatewayARN, volumeSizeInBytes,
                             targetName, networkInterfaceId, clientToken,
                             kMSEncrypted = Core.Nothing, kMSKey = Core.Nothing,
                             snapshotId = Core.Nothing, sourceVolumeARN = Core.Nothing,
                             tags = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivGatewayARN :: Lens.Lens' CreateCachediSCSIVolume Types.GatewayARN
ccscsivGatewayARN = Lens.field @"gatewayARN"
{-# INLINEABLE ccscsivGatewayARN #-}
{-# DEPRECATED gatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead"  #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivVolumeSizeInBytes :: Lens.Lens' CreateCachediSCSIVolume Core.Integer
ccscsivVolumeSizeInBytes = Lens.field @"volumeSizeInBytes"
{-# INLINEABLE ccscsivVolumeSizeInBytes #-}
{-# DEPRECATED volumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead"  #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivTargetName :: Lens.Lens' CreateCachediSCSIVolume Types.TargetName
ccscsivTargetName = Lens.field @"targetName"
{-# INLINEABLE ccscsivTargetName #-}
{-# DEPRECATED targetName "Use generic-lens or generic-optics with 'targetName' instead"  #-}

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivNetworkInterfaceId :: Lens.Lens' CreateCachediSCSIVolume Types.NetworkInterfaceId
ccscsivNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE ccscsivNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivClientToken :: Lens.Lens' CreateCachediSCSIVolume Types.ClientToken
ccscsivClientToken = Lens.field @"clientToken"
{-# INLINEABLE ccscsivClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@ 
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivKMSEncrypted :: Lens.Lens' CreateCachediSCSIVolume (Core.Maybe Core.Bool)
ccscsivKMSEncrypted = Lens.field @"kMSEncrypted"
{-# INLINEABLE ccscsivKMSEncrypted #-}
{-# DEPRECATED kMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead"  #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivKMSKey :: Lens.Lens' CreateCachediSCSIVolume (Core.Maybe Types.KMSKey)
ccscsivKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE ccscsivKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new cached volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivSnapshotId :: Lens.Lens' CreateCachediSCSIVolume (Core.Maybe Types.SnapshotId)
ccscsivSnapshotId = Lens.field @"snapshotId"
{-# INLINEABLE ccscsivSnapshotId #-}
{-# DEPRECATED snapshotId "Use generic-lens or generic-optics with 'snapshotId' instead"  #-}

-- | The ARN for an existing volume. Specifying this ARN makes the new volume into an exact copy of the specified existing volume's latest recovery point. The @VolumeSizeInBytes@ value for this new volume must be equal to or larger than the size of the existing volume, in bytes.
--
-- /Note:/ Consider using 'sourceVolumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivSourceVolumeARN :: Lens.Lens' CreateCachediSCSIVolume (Core.Maybe Types.VolumeARN)
ccscsivSourceVolumeARN = Lens.field @"sourceVolumeARN"
{-# INLINEABLE ccscsivSourceVolumeARN #-}
{-# DEPRECATED sourceVolumeARN "Use generic-lens or generic-optics with 'sourceVolumeARN' instead"  #-}

-- | A list of up to 50 tags that you can assign to a cached volume. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivTags :: Lens.Lens' CreateCachediSCSIVolume (Core.Maybe [Types.Tag])
ccscsivTags = Lens.field @"tags"
{-# INLINEABLE ccscsivTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCachediSCSIVolume where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCachediSCSIVolume where
        toHeaders CreateCachediSCSIVolume{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.CreateCachediSCSIVolume")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCachediSCSIVolume where
        toJSON CreateCachediSCSIVolume{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayARN" Core..= gatewayARN),
                  Core.Just ("VolumeSizeInBytes" Core..= volumeSizeInBytes),
                  Core.Just ("TargetName" Core..= targetName),
                  Core.Just ("NetworkInterfaceId" Core..= networkInterfaceId),
                  Core.Just ("ClientToken" Core..= clientToken),
                  ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
                  ("KMSKey" Core..=) Core.<$> kMSKey,
                  ("SnapshotId" Core..=) Core.<$> snapshotId,
                  ("SourceVolumeARN" Core..=) Core.<$> sourceVolumeARN,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateCachediSCSIVolume where
        type Rs CreateCachediSCSIVolume = CreateCachediSCSIVolumeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateCachediSCSIVolumeResponse' Core.<$>
                   (x Core..:? "TargetARN") Core.<*> x Core..:? "VolumeARN" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCachediSCSIVolumeResponse' smart constructor.
data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'
  { targetARN :: Core.Maybe Types.TargetARN
    -- ^ The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
  , volumeARN :: Core.Maybe Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the configured volume.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCachediSCSIVolumeResponse' value with any optional fields omitted.
mkCreateCachediSCSIVolumeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCachediSCSIVolumeResponse
mkCreateCachediSCSIVolumeResponse responseStatus
  = CreateCachediSCSIVolumeResponse'{targetARN = Core.Nothing,
                                     volumeARN = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivrrsTargetARN :: Lens.Lens' CreateCachediSCSIVolumeResponse (Core.Maybe Types.TargetARN)
ccscsivrrsTargetARN = Lens.field @"targetARN"
{-# INLINEABLE ccscsivrrsTargetARN #-}
{-# DEPRECATED targetARN "Use generic-lens or generic-optics with 'targetARN' instead"  #-}

-- | The Amazon Resource Name (ARN) of the configured volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivrrsVolumeARN :: Lens.Lens' CreateCachediSCSIVolumeResponse (Core.Maybe Types.VolumeARN)
ccscsivrrsVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE ccscsivrrsVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivrrsResponseStatus :: Lens.Lens' CreateCachediSCSIVolumeResponse Core.Int
ccscsivrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccscsivrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
