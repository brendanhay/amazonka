{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateCachediSCSIVolume (..),
    mkCreateCachediSCSIVolume,

    -- ** Request lenses
    ccscsivKMSKey,
    ccscsivSourceVolumeARN,
    ccscsivKMSEncrypted,
    ccscsivTags,
    ccscsivSnapshotId,
    ccscsivGatewayARN,
    ccscsivVolumeSizeInBytes,
    ccscsivTargetName,
    ccscsivNetworkInterfaceId,
    ccscsivClientToken,

    -- * Destructuring the response
    CreateCachediSCSIVolumeResponse (..),
    mkCreateCachediSCSIVolumeResponse,

    -- ** Response lenses
    ccscsivrsTargetARN,
    ccscsivrsVolumeARN,
    ccscsivrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkCreateCachediSCSIVolume' smart constructor.
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'
  { kmsKey ::
      Lude.Maybe Lude.Text,
    sourceVolumeARN :: Lude.Maybe Lude.Text,
    kmsEncrypted :: Lude.Maybe Lude.Bool,
    tags :: Lude.Maybe [Tag],
    snapshotId :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Text,
    volumeSizeInBytes :: Lude.Integer,
    targetName :: Lude.Text,
    networkInterfaceId :: Lude.Text,
    clientToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCachediSCSIVolume' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
-- * 'gatewayARN' - Undocumented field.
-- * 'kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
-- * 'kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
-- * 'networkInterfaceId' - The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
-- * 'snapshotId' - The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new cached volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
-- * 'sourceVolumeARN' - The ARN for an existing volume. Specifying this ARN makes the new volume into an exact copy of the specified existing volume's latest recovery point. The @VolumeSizeInBytes@ value for this new volume must be equal to or larger than the size of the existing volume, in bytes.
-- * 'tags' - A list of up to 50 tags that you can assign to a cached volume. Each tag is a key-value pair.
-- * 'targetName' - The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
-- * 'volumeSizeInBytes' - The size of the volume in bytes.
mkCreateCachediSCSIVolume ::
  -- | 'gatewayARN'
  Lude.Text ->
  -- | 'volumeSizeInBytes'
  Lude.Integer ->
  -- | 'targetName'
  Lude.Text ->
  -- | 'networkInterfaceId'
  Lude.Text ->
  -- | 'clientToken'
  Lude.Text ->
  CreateCachediSCSIVolume
mkCreateCachediSCSIVolume
  pGatewayARN_
  pVolumeSizeInBytes_
  pTargetName_
  pNetworkInterfaceId_
  pClientToken_ =
    CreateCachediSCSIVolume'
      { kmsKey = Lude.Nothing,
        sourceVolumeARN = Lude.Nothing,
        kmsEncrypted = Lude.Nothing,
        tags = Lude.Nothing,
        snapshotId = Lude.Nothing,
        gatewayARN = pGatewayARN_,
        volumeSizeInBytes = pVolumeSizeInBytes_,
        targetName = pTargetName_,
        networkInterfaceId = pNetworkInterfaceId_,
        clientToken = pClientToken_
      }

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK) used for Amazon S3 server-side encryption. Storage Gateway does not support asymmetric CMKs. This value can only be set when @KMSEncrypted@ is @true@ . Optional.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivKMSKey :: Lens.Lens' CreateCachediSCSIVolume (Lude.Maybe Lude.Text)
ccscsivKMSKey = Lens.lens (kmsKey :: CreateCachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | The ARN for an existing volume. Specifying this ARN makes the new volume into an exact copy of the specified existing volume's latest recovery point. The @VolumeSizeInBytes@ value for this new volume must be equal to or larger than the size of the existing volume, in bytes.
--
-- /Note:/ Consider using 'sourceVolumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivSourceVolumeARN :: Lens.Lens' CreateCachediSCSIVolume (Lude.Maybe Lude.Text)
ccscsivSourceVolumeARN = Lens.lens (sourceVolumeARN :: CreateCachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {sourceVolumeARN = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivSourceVolumeARN "Use generic-lens or generic-optics with 'sourceVolumeARN' instead." #-}

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- /Note:/ Consider using 'kmsEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivKMSEncrypted :: Lens.Lens' CreateCachediSCSIVolume (Lude.Maybe Lude.Bool)
ccscsivKMSEncrypted = Lens.lens (kmsEncrypted :: CreateCachediSCSIVolume -> Lude.Maybe Lude.Bool) (\s a -> s {kmsEncrypted = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivKMSEncrypted "Use generic-lens or generic-optics with 'kmsEncrypted' instead." #-}

-- | A list of up to 50 tags that you can assign to a cached volume. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivTags :: Lens.Lens' CreateCachediSCSIVolume (Lude.Maybe [Tag])
ccscsivTags = Lens.lens (tags :: CreateCachediSCSIVolume -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The snapshot ID (e.g. "snap-1122aabb") of the snapshot to restore as the new cached volume. Specify this field if you want to create the iSCSI storage volume from a snapshot; otherwise, do not include this field. To list snapshots for your account use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivSnapshotId :: Lens.Lens' CreateCachediSCSIVolume (Lude.Maybe Lude.Text)
ccscsivSnapshotId = Lens.lens (snapshotId :: CreateCachediSCSIVolume -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivGatewayARN :: Lens.Lens' CreateCachediSCSIVolume Lude.Text
ccscsivGatewayARN = Lens.lens (gatewayARN :: CreateCachediSCSIVolume -> Lude.Text) (\s a -> s {gatewayARN = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The size of the volume in bytes.
--
-- /Note:/ Consider using 'volumeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivVolumeSizeInBytes :: Lens.Lens' CreateCachediSCSIVolume Lude.Integer
ccscsivVolumeSizeInBytes = Lens.lens (volumeSizeInBytes :: CreateCachediSCSIVolume -> Lude.Integer) (\s a -> s {volumeSizeInBytes = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivVolumeSizeInBytes "Use generic-lens or generic-optics with 'volumeSizeInBytes' instead." #-}

-- | The name of the iSCSI target used by an initiator to connect to a volume and used as a suffix for the target ARN. For example, specifying @TargetName@ as /myvolume/ results in the target ARN of @arn:aws:storagegateway:us-east-2:111122223333:gateway/sgw-12A3456B/target/iqn.1997-05.com.amazon:myvolume@ . The target name must be unique across all volumes on a gateway.
--
-- If you don't specify a value, Storage Gateway uses the value that was previously used for this volume as the new target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivTargetName :: Lens.Lens' CreateCachediSCSIVolume Lude.Text
ccscsivTargetName = Lens.lens (targetName :: CreateCachediSCSIVolume -> Lude.Text) (\s a -> s {targetName = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

-- | The network interface of the gateway on which to expose the iSCSI target. Only IPv4 addresses are accepted. Use 'DescribeGatewayInformation' to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivNetworkInterfaceId :: Lens.Lens' CreateCachediSCSIVolume Lude.Text
ccscsivNetworkInterfaceId = Lens.lens (networkInterfaceId :: CreateCachediSCSIVolume -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | A unique identifier that you use to retry a request. If you retry a request, use the same @ClientToken@ you specified in the initial request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivClientToken :: Lens.Lens' CreateCachediSCSIVolume Lude.Text
ccscsivClientToken = Lens.lens (clientToken :: CreateCachediSCSIVolume -> Lude.Text) (\s a -> s {clientToken = a} :: CreateCachediSCSIVolume)
{-# DEPRECATED ccscsivClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Lude.AWSRequest CreateCachediSCSIVolume where
  type Rs CreateCachediSCSIVolume = CreateCachediSCSIVolumeResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCachediSCSIVolumeResponse'
            Lude.<$> (x Lude..?> "TargetARN")
            Lude.<*> (x Lude..?> "VolumeARN")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCachediSCSIVolume where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.CreateCachediSCSIVolume" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCachediSCSIVolume where
  toJSON CreateCachediSCSIVolume' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KMSKey" Lude..=) Lude.<$> kmsKey,
            ("SourceVolumeARN" Lude..=) Lude.<$> sourceVolumeARN,
            ("KMSEncrypted" Lude..=) Lude.<$> kmsEncrypted,
            ("Tags" Lude..=) Lude.<$> tags,
            ("SnapshotId" Lude..=) Lude.<$> snapshotId,
            Lude.Just ("GatewayARN" Lude..= gatewayARN),
            Lude.Just ("VolumeSizeInBytes" Lude..= volumeSizeInBytes),
            Lude.Just ("TargetName" Lude..= targetName),
            Lude.Just ("NetworkInterfaceId" Lude..= networkInterfaceId),
            Lude.Just ("ClientToken" Lude..= clientToken)
          ]
      )

instance Lude.ToPath CreateCachediSCSIVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCachediSCSIVolume where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCachediSCSIVolumeResponse' smart constructor.
data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'
  { targetARN ::
      Lude.Maybe Lude.Text,
    volumeARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateCachediSCSIVolumeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the configured volume.
mkCreateCachediSCSIVolumeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCachediSCSIVolumeResponse
mkCreateCachediSCSIVolumeResponse pResponseStatus_ =
  CreateCachediSCSIVolumeResponse'
    { targetARN = Lude.Nothing,
      volumeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume target, which includes the iSCSI name that initiators can use to connect to the target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivrsTargetARN :: Lens.Lens' CreateCachediSCSIVolumeResponse (Lude.Maybe Lude.Text)
ccscsivrsTargetARN = Lens.lens (targetARN :: CreateCachediSCSIVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: CreateCachediSCSIVolumeResponse)
{-# DEPRECATED ccscsivrsTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the configured volume.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivrsVolumeARN :: Lens.Lens' CreateCachediSCSIVolumeResponse (Lude.Maybe Lude.Text)
ccscsivrsVolumeARN = Lens.lens (volumeARN :: CreateCachediSCSIVolumeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: CreateCachediSCSIVolumeResponse)
{-# DEPRECATED ccscsivrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccscsivrsResponseStatus :: Lens.Lens' CreateCachediSCSIVolumeResponse Lude.Int
ccscsivrsResponseStatus = Lens.lens (responseStatus :: CreateCachediSCSIVolumeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCachediSCSIVolumeResponse)
{-# DEPRECATED ccscsivrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
