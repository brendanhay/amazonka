{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateStorediSCSIVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a volume on a specified gateway. This operation is only supported in the stored volume gateway type.
--
-- The size of the volume to create is inferred from the disk size. You can choose to preserve existing data on the disk, create volume from an existing snapshot, or create an empty volume. If you choose to create an empty gateway volume, then any existing data on the disk is erased.
-- In the request, you must specify the gateway and the disk information on which you are creating the volume. In response, the gateway creates the volume and returns volume information such as the volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN that initiators can use to connect to the volume target.
module Network.AWS.StorageGateway.CreateStorediSCSIVolume
  ( -- * Creating a request
    CreateStorediSCSIVolume (..),
    mkCreateStorediSCSIVolume,

    -- ** Request lenses
    csscsivGatewayARN,
    csscsivDiskId,
    csscsivPreserveExistingData,
    csscsivTargetName,
    csscsivNetworkInterfaceId,
    csscsivKMSEncrypted,
    csscsivKMSKey,
    csscsivSnapshotId,
    csscsivTags,

    -- * Destructuring the response
    CreateStorediSCSIVolumeResponse (..),
    mkCreateStorediSCSIVolumeResponse,

    -- ** Response lenses
    csscsivrrsTargetARN,
    csscsivrrsVolumeARN,
    csscsivrrsVolumeSizeInBytes,
    csscsivrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'CreateStorediSCSIVolumeInput$DiskId'
--
--
--     * 'CreateStorediSCSIVolumeInput$NetworkInterfaceId'
--
--
--     * 'CreateStorediSCSIVolumeInput$PreserveExistingData'
--
--
--     * 'CreateStorediSCSIVolumeInput$SnapshotId'
--
--
--     * 'CreateStorediSCSIVolumeInput$TargetName'
--
--
--
-- /See:/ 'mkCreateStorediSCSIVolume' smart constructor.
data CreateStorediSCSIVolume = CreateStorediSCSIVolume'
  { gatewayARN :: Types.GatewayARN,
    -- | The unique identifier for the gateway local disk that is configured as a stored volume. Use <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks> to list disk IDs for a gateway.
    diskId :: Types.DiskId,
    -- | Set to true @true@ if you want to preserve the data on the local disk. Otherwise, set to @false@ to create an empty volume.
    --
    -- Valid Values: @true@ | @false@
    preserveExistingData :: Core.Bool,
    -- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
    --
    -- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
    targetName :: Types.TargetName,
    -- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
    --
    -- Valid Values: A valid IP address.
    networkInterfaceId :: Types.NetworkInterfaceId,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kMSEncrypted :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
    kMSKey :: Core.Maybe Types.KMSKey,
    -- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new stored volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
    snapshotId :: Core.Maybe Types.SnapshotId,
    -- | A list of up to 50 tags that can be assigned to a stored volume. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStorediSCSIVolume' value with any optional fields omitted.
mkCreateStorediSCSIVolume ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  -- | 'diskId'
  Types.DiskId ->
  -- | 'preserveExistingData'
  Core.Bool ->
  -- | 'targetName'
  Types.TargetName ->
  -- | 'networkInterfaceId'
  Types.NetworkInterfaceId ->
  CreateStorediSCSIVolume
mkCreateStorediSCSIVolume
  gatewayARN
  diskId
  preserveExistingData
  targetName
  networkInterfaceId =
    CreateStorediSCSIVolume'
      { gatewayARN,
        diskId,
        preserveExistingData,
        targetName,
        networkInterfaceId,
        kMSEncrypted = Core.Nothing,
        kMSKey = Core.Nothing,
        snapshotId = Core.Nothing,
        tags = Core.Nothing
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivGatewayARN :: Lens.Lens' CreateStorediSCSIVolume Types.GatewayARN
csscsivGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED csscsivGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The unique identifier for the gateway local disk that is configured as a stored volume. Use <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks> to list disk IDs for a gateway.
--
-- /Note:/ Consider using 'diskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivDiskId :: Lens.Lens' CreateStorediSCSIVolume Types.DiskId
csscsivDiskId = Lens.field @"diskId"
{-# DEPRECATED csscsivDiskId "Use generic-lens or generic-optics with 'diskId' instead." #-}

-- | Set to true @true@ if you want to preserve the data on the local disk. Otherwise, set to @false@ to create an empty volume.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'preserveExistingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivPreserveExistingData :: Lens.Lens' CreateStorediSCSIVolume Core.Bool
csscsivPreserveExistingData = Lens.field @"preserveExistingData"
{-# DEPRECATED csscsivPreserveExistingData "Use generic-lens or generic-optics with 'preserveExistingData' instead." #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivTargetName :: Lens.Lens' CreateStorediSCSIVolume Types.TargetName
csscsivTargetName = Lens.field @"targetName"
{-# DEPRECATED csscsivTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivNetworkInterfaceId :: Lens.Lens' CreateStorediSCSIVolume Types.NetworkInterfaceId
csscsivNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED csscsivNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kMSEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivKMSEncrypted :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe Core.Bool)
csscsivKMSEncrypted = Lens.field @"kMSEncrypted"
{-# DEPRECATED csscsivKMSEncrypted "Use generic-lens or generic-optics with 'kMSEncrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivKMSKey :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe Types.KMSKey)
csscsivKMSKey = Lens.field @"kMSKey"
{-# DEPRECATED csscsivKMSKey "Use generic-lens or generic-optics with 'kMSKey' instead." #-}

-- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new stored volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivSnapshotId :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe Types.SnapshotId)
csscsivSnapshotId = Lens.field @"snapshotId"
{-# DEPRECATED csscsivSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | A list of up to 50 tags that can be assigned to a stored volume. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivTags :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe [Types.Tag])
csscsivTags = Lens.field @"tags"
{-# DEPRECATED csscsivTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateStorediSCSIVolume where
  toJSON CreateStorediSCSIVolume {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DiskId" Core..= diskId),
            Core.Just ("PreserveExistingData" Core..= preserveExistingData),
            Core.Just ("TargetName" Core..= targetName),
            Core.Just ("NetworkInterfaceId" Core..= networkInterfaceId),
            ("KMSEncrypted" Core..=) Core.<$> kMSEncrypted,
            ("KMSKey" Core..=) Core.<$> kMSKey,
            ("SnapshotId" Core..=) Core.<$> snapshotId,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateStorediSCSIVolume where
  type Rs CreateStorediSCSIVolume = CreateStorediSCSIVolumeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.CreateStorediSCSIVolume")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStorediSCSIVolumeResponse'
            Core.<$> (x Core..:? "TargetARN")
            Core.<*> (x Core..:? "VolumeARN")
            Core.<*> (x Core..:? "VolumeSizeInBytes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkCreateStorediSCSIVolumeResponse' smart constructor.
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
    targetARN :: Core.Maybe Types.TargetARN,
    -- | The Amazon Resource Name (ARN) of the configured volume.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStorediSCSIVolumeResponse' value with any optional fields omitted.
mkCreateStorediSCSIVolumeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStorediSCSIVolumeResponse
mkCreateStorediSCSIVolumeResponse responseStatus =
  CreateStorediSCSIVolumeResponse'
    { targetARN = Core.Nothing,
      volumeARN = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrrsTargetARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Core.Maybe Types.TargetARN)
csscsivrrsTargetARN = Lens.field @"targetARN"
{-# DEPRECATED csscsivrrsTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the configured volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrrsVolumeARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Core.Maybe Types.VolumeARN)
csscsivrrsVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED csscsivrrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrrsVolumeSizeInBytes :: Lens.Lens' CreateStorediSCSIVolumeResponse (Core.Maybe Core.Integer)
csscsivrrsVolumeSizeInBytes = Lens.field @"volumeSizeInBytes"
{-# DEPRECATED csscsivrrsVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrrsResponseStatus :: Lens.Lens' CreateStorediSCSIVolumeResponse Core.Int
csscsivrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csscsivrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
