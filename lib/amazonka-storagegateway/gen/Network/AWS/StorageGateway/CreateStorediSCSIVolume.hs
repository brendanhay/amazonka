{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csscsivKMSKey,
    csscsivKMSEncrypted,
    csscsivTags,
    csscsivSnapshotId,
    csscsivGatewayARN,
    csscsivDiskId,
    csscsivPreserveExistingData,
    csscsivTargetName,
    csscsivNetworkInterfaceId,

    -- * Destructuring the response
    CreateStorediSCSIVolumeResponse (..),
    mkCreateStorediSCSIVolumeResponse,

    -- ** Response lenses
    csscsivrsTargetARN,
    csscsivrsVolumeARN,
    csscsivrsVolumeSizeInBytes,
    csscsivrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

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
  { kmsKey ::
      Lude.Maybe Lude.Text,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag],
    snapshotId :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Text,
    diskId :: Lude.Text,
    preserveExistingData :: Lude.Bool,
    targetName :: Lude.Text,
    networkInterfaceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStorediSCSIVolume' with the minimum fields required to make a request.
--
-- * 'diskId' - The unique identifier for the gateway local disk that is configured as a stored volume. Use <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks> to list disk IDs for a gateway.
-- * 'gatewayARN' - Undocumented field.
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'networkInterfaceId' - The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
-- * 'preserveExistingData' - Set to true @true@ if you want to preserve the data on the local disk. Otherwise, set to @false@ to create an empty volume.
--
-- Valid Values: @true@ | @false@
-- * 'snapshotId' - The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new stored volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
-- * 'tags' - A list of up to 50 tags that can be assigned to a stored volume. Each tag is a key-value pair.
-- * 'targetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
mkCreateStorediSCSIVolume ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'diskId'
  Lude.Text ->
  -- | 'preserveExistingData'
  Lude.Bool ->
  -- | 'targetName'
  Lude.Text ->
  -- | 'networkInterfaceId'
  Lude.Text ->
  CreateStorediSCSIVolume
mkCreateStorediSCSIVolume
  pGatewayARN_
  pDiskId_
  pPreserveExistingData_
  pTargetName_
  pNetworkInterfaceId_ =
    CreateStorediSCSIVolume'
      { kmsKey = Lude.Nothing,
        kmsEncrypted = Lude.Nothing,
        tags = Lude.Nothing,
        snapshotId = Lude.Nothing,
        gatewayARN = pGatewayARN_,
        diskId = pDiskId_,
        preserveExistingData = pPreserveExistingData_,
        targetName = pTargetName_,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivKMSKey :: Lens.Lens' CreateStorediSCSIVolume (Lude.Maybe Lude.Text)
csscsivKMSKey = Lens.lens (kmsKey :: CreateStorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivKMSEncrypted :: Lens.Lens' CreateStorediSCSIVolume (Lude.Maybe Lude.Bool)
csscsivKMSEncrypted = Lens.lens (kmsEncrypted :: CreateStorediSCSIVolume -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | A list of up to 50 tags that can be assigned to a stored volume. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivTags :: Lens.Lens' CreateStorediSCSIVolume (Lude.Maybe [Tag])
csscsivTags = Lens.lens (tags :: CreateStorediSCSIVolume -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new stored volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivSnapshotId :: Lens.Lens' CreateStorediSCSIVolume (Lude.Maybe Lude.Text)
csscsivSnapshotId = Lens.lens (snapshotId :: CreateStorediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivGatewayARN :: Lens.Lens' CreateStorediSCSIVolume Lude.Text
csscsivGatewayARN = Lens.lens (gatewayARN :: CreateStorediSCSIVolume -> Lude.Text) (\s a -> s {gatewayARN = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The unique identifier for the gateway local disk that is configured as a stored volume. Use <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks> to list disk IDs for a gateway.
--
-- /Note:/ Consider using 'diskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivDiskId :: Lens.Lens' CreateStorediSCSIVolume Lude.Text
csscsivDiskId = Lens.lens (diskId :: CreateStorediSCSIVolume -> Lude.Text) (\s a -> s {diskId = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivDiskId "Use generic-lens or generic-optics with 'diskId' instead." #-}

-- | Set to true @true@ if you want to preserve the data on the local disk. Otherwise, set to @false@ to create an empty volume.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'preserveExistingData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivPreserveExistingData :: Lens.Lens' CreateStorediSCSIVolume Lude.Bool
csscsivPreserveExistingData = Lens.lens (preserveExistingData :: CreateStorediSCSIVolume -> Lude.Bool) (\s a -> s {preserveExistingData = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivPreserveExistingData "Use generic-lens or generic-optics with 'preserveExistingData' instead." #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivTargetName :: Lens.Lens' CreateStorediSCSIVolume Lude.Text
csscsivTargetName = Lens.lens (targetName :: CreateStorediSCSIVolume -> Lude.Text) (\s a -> s {targetName = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivNetworkInterfaceId :: Lens.Lens' CreateStorediSCSIVolume Lude.Text
csscsivNetworkInterfaceId = Lens.lens (networkInterfaceId :: CreateStorediSCSIVolume -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: CreateStorediSCSIVolume)
{-# DEPRECATED csscsivNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest CreateStorediSCSIVolume where
  type Rs CreateStorediSCSIVolume = CreateStorediSCSIVolumeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStorediSCSIVolumeResponse'
            Lude.<$> (x Lude..?> "TargetARN")
            Lude.<*> (x Lude..?> "VolumeARN")
            Lude.<*> (x Lude..?> "VolumeSizeInBytes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStorediSCSIVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.CreateStorediSCSIVolume" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStorediSCSIVolume where
  toJSON CreateStorediSCSIVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            ("Tags" Lude..=) Lude.<$> tags,
            ("SnapshotId" Lude..=) Lude.<$> snapshotId,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("DiskId" Lude..= diskId),
            Lude.Just ("PreserveExistingData" Lude..= preserveExistingData),
            Lude.Just ("TargetName" Lude..= targetName),
            Lude.Just ("NetworkInterfaceId" Lude..= networkInterfaceId)
          ]
      )

instance Lude.ToPath CreateStorediSCSIVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStorediSCSIVolume where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkCreateStorediSCSIVolumeResponse' smart constructor.
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse'
  { targetARN ::
      Lude.Maybe Lude.Text,
    volumeARN ::
      Lude.Maybe Lude.Text,
    volumeSizeInBytes ::
      Lude.Maybe Lude.Integer,
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

-- | Creates a value of 'CreateStorediSCSIVolumeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the configured volume.
-- * 'volumeSizeInBytes' - The size of the volume in bytes.
mkCreateStorediSCSIVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStorediSCSIVolumeResponse
mkCreateStorediSCSIVolumeResponse pResponseStatus_ =
  CreateStorediSCSIVolumeResponse'
    { targetARN = Lude.Nothing,
      volumeARN = Lude.Nothing,
      volumeSizeInBytes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrsTargetARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Lude.Maybe Lude.Text)
csscsivrsTargetARN = Lens.lens (targetARN :: CreateStorediSCSIVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: CreateStorediSCSIVolumeResponse)
{-# DEPRECATED csscsivrsTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the configured volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrsVolumeARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Lude.Maybe Lude.Text)
csscsivrsVolumeARN = Lens.lens (volumeARN :: CreateStorediSCSIVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: CreateStorediSCSIVolumeResponse)
{-# DEPRECATED csscsivrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrsVolumeSizeInBytes :: Lens.Lens' CreateStorediSCSIVolumeResponse (Lude.Maybe Lude.Integer)
csscsivrsVolumeSizeInBytes = Lens.lens (volumeSizeInBytes :: CreateStorediSCSIVolumeResponse -> Lude.Maybe Lude.Integer) (\s a -> s {volumeSizeInBytes = a} :: CreateStorediSCSIVolumeResponse)
{-# DEPRECATED csscsivrsVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csscsivrsResponseStatus :: Lens.Lens' CreateStorediSCSIVolumeResponse Lude.Int
csscsivrsResponseStatus = Lens.lens (responseStatus :: CreateStorediSCSIVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStorediSCSIVolumeResponse)
{-# DEPRECATED csscsivrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
