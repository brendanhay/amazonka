{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateStorediSCSIVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a volume on a specified gateway. This operation is only
-- supported in the stored volume gateway type.
--
-- The size of the volume to create is inferred from the disk size. You can
-- choose to preserve existing data on the disk, create volume from an
-- existing snapshot, or create an empty volume. If you choose to create an
-- empty gateway volume, then any existing data on the disk is erased.
--
-- In the request, you must specify the gateway and the disk information on
-- which you are creating the volume. In response, the gateway creates the
-- volume and returns volume information such as the volume Amazon Resource
-- Name (ARN), its size, and the iSCSI target ARN that initiators can use
-- to connect to the volume target.
module Network.AWS.StorageGateway.CreateStorediSCSIVolume
  ( -- * Creating a Request
    CreateStorediSCSIVolume (..),
    newCreateStorediSCSIVolume,

    -- * Request Lenses
    createStorediSCSIVolume_kmsEncrypted,
    createStorediSCSIVolume_kmsKey,
    createStorediSCSIVolume_snapshotId,
    createStorediSCSIVolume_tags,
    createStorediSCSIVolume_gatewayARN,
    createStorediSCSIVolume_diskId,
    createStorediSCSIVolume_preserveExistingData,
    createStorediSCSIVolume_targetName,
    createStorediSCSIVolume_networkInterfaceId,

    -- * Destructuring the Response
    CreateStorediSCSIVolumeResponse (..),
    newCreateStorediSCSIVolumeResponse,

    -- * Response Lenses
    createStorediSCSIVolumeResponse_volumeARN,
    createStorediSCSIVolumeResponse_targetARN,
    createStorediSCSIVolumeResponse_volumeSizeInBytes,
    createStorediSCSIVolumeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   CreateStorediSCSIVolumeInput$DiskId
--
-- -   CreateStorediSCSIVolumeInput$NetworkInterfaceId
--
-- -   CreateStorediSCSIVolumeInput$PreserveExistingData
--
-- -   CreateStorediSCSIVolumeInput$SnapshotId
--
-- -   CreateStorediSCSIVolumeInput$TargetName
--
-- /See:/ 'newCreateStorediSCSIVolume' smart constructor.
data CreateStorediSCSIVolume = CreateStorediSCSIVolume'
  { -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Core.Maybe Core.Text,
    -- | The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
    -- the new stored volume. Specify this field if you want to create the
    -- iSCSI storage volume from a snapshot; otherwise, do not include this
    -- field. To list snapshots for your account use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
    -- in the /Amazon Elastic Compute Cloud API Reference/.
    snapshotId :: Core.Maybe Core.Text,
    -- | A list of up to 50 tags that can be assigned to a stored volume. Each
    -- tag is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Core.Maybe [Tag],
    gatewayARN :: Core.Text,
    -- | The unique identifier for the gateway local disk that is configured as a
    -- stored volume. Use
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
    -- to list disk IDs for a gateway.
    diskId :: Core.Text,
    -- | Set to true @true@ if you want to preserve the data on the local disk.
    -- Otherwise, set to @false@ to create an empty volume.
    --
    -- Valid Values: @true@ | @false@
    preserveExistingData :: Core.Bool,
    -- | The name of the iSCSI target used by an initiator to connect to a volume
    -- and used as a suffix for the target ARN. For example, specifying
    -- @TargetName@ as /myvolume/ results in the target ARN of
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
    -- The target name must be unique across all volumes on a gateway.
    --
    -- If you don\'t specify a value, Storage Gateway uses the value that was
    -- previously used for this volume as the new target name.
    targetName :: Core.Text,
    -- | The network interface of the gateway on which to expose the iSCSI
    -- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
    -- to get a list of the network interfaces available on a gateway.
    --
    -- Valid Values: A valid IP address.
    networkInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStorediSCSIVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsEncrypted', 'createStorediSCSIVolume_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsKey', 'createStorediSCSIVolume_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'snapshotId', 'createStorediSCSIVolume_snapshotId' - The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
-- the new stored volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot; otherwise, do not include this
-- field. To list snapshots for your account use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
--
-- 'tags', 'createStorediSCSIVolume_tags' - A list of up to 50 tags that can be assigned to a stored volume. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'gatewayARN', 'createStorediSCSIVolume_gatewayARN' - Undocumented member.
--
-- 'diskId', 'createStorediSCSIVolume_diskId' - The unique identifier for the gateway local disk that is configured as a
-- stored volume. Use
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
-- to list disk IDs for a gateway.
--
-- 'preserveExistingData', 'createStorediSCSIVolume_preserveExistingData' - Set to true @true@ if you want to preserve the data on the local disk.
-- Otherwise, set to @false@ to create an empty volume.
--
-- Valid Values: @true@ | @false@
--
-- 'targetName', 'createStorediSCSIVolume_targetName' - The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
--
-- 'networkInterfaceId', 'createStorediSCSIVolume_networkInterfaceId' - The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
newCreateStorediSCSIVolume ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'diskId'
  Core.Text ->
  -- | 'preserveExistingData'
  Core.Bool ->
  -- | 'targetName'
  Core.Text ->
  -- | 'networkInterfaceId'
  Core.Text ->
  CreateStorediSCSIVolume
newCreateStorediSCSIVolume
  pGatewayARN_
  pDiskId_
  pPreserveExistingData_
  pTargetName_
  pNetworkInterfaceId_ =
    CreateStorediSCSIVolume'
      { kmsEncrypted =
          Core.Nothing,
        kmsKey = Core.Nothing,
        snapshotId = Core.Nothing,
        tags = Core.Nothing,
        gatewayARN = pGatewayARN_,
        diskId = pDiskId_,
        preserveExistingData = pPreserveExistingData_,
        targetName = pTargetName_,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createStorediSCSIVolume_kmsEncrypted :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe Core.Bool)
createStorediSCSIVolume_kmsEncrypted = Lens.lens (\CreateStorediSCSIVolume' {kmsEncrypted} -> kmsEncrypted) (\s@CreateStorediSCSIVolume' {} a -> s {kmsEncrypted = a} :: CreateStorediSCSIVolume)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createStorediSCSIVolume_kmsKey :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe Core.Text)
createStorediSCSIVolume_kmsKey = Lens.lens (\CreateStorediSCSIVolume' {kmsKey} -> kmsKey) (\s@CreateStorediSCSIVolume' {} a -> s {kmsKey = a} :: CreateStorediSCSIVolume)

-- | The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
-- the new stored volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot; otherwise, do not include this
-- field. To list snapshots for your account use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
createStorediSCSIVolume_snapshotId :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe Core.Text)
createStorediSCSIVolume_snapshotId = Lens.lens (\CreateStorediSCSIVolume' {snapshotId} -> snapshotId) (\s@CreateStorediSCSIVolume' {} a -> s {snapshotId = a} :: CreateStorediSCSIVolume)

-- | A list of up to 50 tags that can be assigned to a stored volume. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createStorediSCSIVolume_tags :: Lens.Lens' CreateStorediSCSIVolume (Core.Maybe [Tag])
createStorediSCSIVolume_tags = Lens.lens (\CreateStorediSCSIVolume' {tags} -> tags) (\s@CreateStorediSCSIVolume' {} a -> s {tags = a} :: CreateStorediSCSIVolume) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createStorediSCSIVolume_gatewayARN :: Lens.Lens' CreateStorediSCSIVolume Core.Text
createStorediSCSIVolume_gatewayARN = Lens.lens (\CreateStorediSCSIVolume' {gatewayARN} -> gatewayARN) (\s@CreateStorediSCSIVolume' {} a -> s {gatewayARN = a} :: CreateStorediSCSIVolume)

-- | The unique identifier for the gateway local disk that is configured as a
-- stored volume. Use
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
-- to list disk IDs for a gateway.
createStorediSCSIVolume_diskId :: Lens.Lens' CreateStorediSCSIVolume Core.Text
createStorediSCSIVolume_diskId = Lens.lens (\CreateStorediSCSIVolume' {diskId} -> diskId) (\s@CreateStorediSCSIVolume' {} a -> s {diskId = a} :: CreateStorediSCSIVolume)

-- | Set to true @true@ if you want to preserve the data on the local disk.
-- Otherwise, set to @false@ to create an empty volume.
--
-- Valid Values: @true@ | @false@
createStorediSCSIVolume_preserveExistingData :: Lens.Lens' CreateStorediSCSIVolume Core.Bool
createStorediSCSIVolume_preserveExistingData = Lens.lens (\CreateStorediSCSIVolume' {preserveExistingData} -> preserveExistingData) (\s@CreateStorediSCSIVolume' {} a -> s {preserveExistingData = a} :: CreateStorediSCSIVolume)

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
createStorediSCSIVolume_targetName :: Lens.Lens' CreateStorediSCSIVolume Core.Text
createStorediSCSIVolume_targetName = Lens.lens (\CreateStorediSCSIVolume' {targetName} -> targetName) (\s@CreateStorediSCSIVolume' {} a -> s {targetName = a} :: CreateStorediSCSIVolume)

-- | The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
createStorediSCSIVolume_networkInterfaceId :: Lens.Lens' CreateStorediSCSIVolume Core.Text
createStorediSCSIVolume_networkInterfaceId = Lens.lens (\CreateStorediSCSIVolume' {networkInterfaceId} -> networkInterfaceId) (\s@CreateStorediSCSIVolume' {} a -> s {networkInterfaceId = a} :: CreateStorediSCSIVolume)

instance Core.AWSRequest CreateStorediSCSIVolume where
  type
    AWSResponse CreateStorediSCSIVolume =
      CreateStorediSCSIVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStorediSCSIVolumeResponse'
            Core.<$> (x Core..?> "VolumeARN")
            Core.<*> (x Core..?> "TargetARN")
            Core.<*> (x Core..?> "VolumeSizeInBytes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateStorediSCSIVolume

instance Core.NFData CreateStorediSCSIVolume

instance Core.ToHeaders CreateStorediSCSIVolume where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateStorediSCSIVolume" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateStorediSCSIVolume where
  toJSON CreateStorediSCSIVolume' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KMSEncrypted" Core..=) Core.<$> kmsEncrypted,
            ("KMSKey" Core..=) Core.<$> kmsKey,
            ("SnapshotId" Core..=) Core.<$> snapshotId,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DiskId" Core..= diskId),
            Core.Just
              ( "PreserveExistingData"
                  Core..= preserveExistingData
              ),
            Core.Just ("TargetName" Core..= targetName),
            Core.Just
              ("NetworkInterfaceId" Core..= networkInterfaceId)
          ]
      )

instance Core.ToPath CreateStorediSCSIVolume where
  toPath = Core.const "/"

instance Core.ToQuery CreateStorediSCSIVolume where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newCreateStorediSCSIVolumeResponse' smart constructor.
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the configured volume.
    volumeARN :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the volume target, which includes the
    -- iSCSI name that initiators can use to connect to the target.
    targetARN :: Core.Maybe Core.Text,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateStorediSCSIVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'createStorediSCSIVolumeResponse_volumeARN' - The Amazon Resource Name (ARN) of the configured volume.
--
-- 'targetARN', 'createStorediSCSIVolumeResponse_targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name that initiators can use to connect to the target.
--
-- 'volumeSizeInBytes', 'createStorediSCSIVolumeResponse_volumeSizeInBytes' - The size of the volume in bytes.
--
-- 'httpStatus', 'createStorediSCSIVolumeResponse_httpStatus' - The response's http status code.
newCreateStorediSCSIVolumeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateStorediSCSIVolumeResponse
newCreateStorediSCSIVolumeResponse pHttpStatus_ =
  CreateStorediSCSIVolumeResponse'
    { volumeARN =
        Core.Nothing,
      targetARN = Core.Nothing,
      volumeSizeInBytes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the configured volume.
createStorediSCSIVolumeResponse_volumeARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Core.Maybe Core.Text)
createStorediSCSIVolumeResponse_volumeARN = Lens.lens (\CreateStorediSCSIVolumeResponse' {volumeARN} -> volumeARN) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {volumeARN = a} :: CreateStorediSCSIVolumeResponse)

-- | The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name that initiators can use to connect to the target.
createStorediSCSIVolumeResponse_targetARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Core.Maybe Core.Text)
createStorediSCSIVolumeResponse_targetARN = Lens.lens (\CreateStorediSCSIVolumeResponse' {targetARN} -> targetARN) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {targetARN = a} :: CreateStorediSCSIVolumeResponse)

-- | The size of the volume in bytes.
createStorediSCSIVolumeResponse_volumeSizeInBytes :: Lens.Lens' CreateStorediSCSIVolumeResponse (Core.Maybe Core.Integer)
createStorediSCSIVolumeResponse_volumeSizeInBytes = Lens.lens (\CreateStorediSCSIVolumeResponse' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {volumeSizeInBytes = a} :: CreateStorediSCSIVolumeResponse)

-- | The response's http status code.
createStorediSCSIVolumeResponse_httpStatus :: Lens.Lens' CreateStorediSCSIVolumeResponse Core.Int
createStorediSCSIVolumeResponse_httpStatus = Lens.lens (\CreateStorediSCSIVolumeResponse' {httpStatus} -> httpStatus) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {httpStatus = a} :: CreateStorediSCSIVolumeResponse)

instance Core.NFData CreateStorediSCSIVolumeResponse
