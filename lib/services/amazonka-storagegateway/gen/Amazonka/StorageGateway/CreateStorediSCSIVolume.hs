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
-- Module      : Amazonka.StorageGateway.CreateStorediSCSIVolume
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.StorageGateway.CreateStorediSCSIVolume
  ( -- * Creating a Request
    CreateStorediSCSIVolume (..),
    newCreateStorediSCSIVolume,

    -- * Request Lenses
    createStorediSCSIVolume_tags,
    createStorediSCSIVolume_snapshotId,
    createStorediSCSIVolume_kmsKey,
    createStorediSCSIVolume_kmsEncrypted,
    createStorediSCSIVolume_gatewayARN,
    createStorediSCSIVolume_diskId,
    createStorediSCSIVolume_preserveExistingData,
    createStorediSCSIVolume_targetName,
    createStorediSCSIVolume_networkInterfaceId,

    -- * Destructuring the Response
    CreateStorediSCSIVolumeResponse (..),
    newCreateStorediSCSIVolumeResponse,

    -- * Response Lenses
    createStorediSCSIVolumeResponse_volumeSizeInBytes,
    createStorediSCSIVolumeResponse_targetARN,
    createStorediSCSIVolumeResponse_volumeARN,
    createStorediSCSIVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

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
  { -- | A list of up to 50 tags that can be assigned to a stored volume. Each
    -- tag is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | The snapshot ID (e.g., \"snap-1122aabb\") of the snapshot to restore as
    -- the new stored volume. Specify this field if you want to create the
    -- iSCSI storage volume from a snapshot; otherwise, do not include this
    -- field. To list snapshots for your account use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
    -- in the /Amazon Elastic Compute Cloud API Reference/.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    gatewayARN :: Prelude.Text,
    -- | The unique identifier for the gateway local disk that is configured as a
    -- stored volume. Use
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
    -- to list disk IDs for a gateway.
    diskId :: Prelude.Text,
    -- | Set to @true@ if you want to preserve the data on the local disk.
    -- Otherwise, set to @false@ to create an empty volume.
    --
    -- Valid Values: @true@ | @false@
    preserveExistingData :: Prelude.Bool,
    -- | The name of the iSCSI target used by an initiator to connect to a volume
    -- and used as a suffix for the target ARN. For example, specifying
    -- @TargetName@ as /myvolume/ results in the target ARN of
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
    -- The target name must be unique across all volumes on a gateway.
    --
    -- If you don\'t specify a value, Storage Gateway uses the value that was
    -- previously used for this volume as the new target name.
    targetName :: Prelude.Text,
    -- | The network interface of the gateway on which to expose the iSCSI
    -- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
    -- to get a list of the network interfaces available on a gateway.
    --
    -- Valid Values: A valid IP address.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStorediSCSIVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createStorediSCSIVolume_tags' - A list of up to 50 tags that can be assigned to a stored volume. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'snapshotId', 'createStorediSCSIVolume_snapshotId' - The snapshot ID (e.g., \"snap-1122aabb\") of the snapshot to restore as
-- the new stored volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot; otherwise, do not include this
-- field. To list snapshots for your account use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
--
-- 'kmsKey', 'createStorediSCSIVolume_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'kmsEncrypted', 'createStorediSCSIVolume_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'gatewayARN', 'createStorediSCSIVolume_gatewayARN' - Undocumented member.
--
-- 'diskId', 'createStorediSCSIVolume_diskId' - The unique identifier for the gateway local disk that is configured as a
-- stored volume. Use
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
-- to list disk IDs for a gateway.
--
-- 'preserveExistingData', 'createStorediSCSIVolume_preserveExistingData' - Set to @true@ if you want to preserve the data on the local disk.
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
  Prelude.Text ->
  -- | 'diskId'
  Prelude.Text ->
  -- | 'preserveExistingData'
  Prelude.Bool ->
  -- | 'targetName'
  Prelude.Text ->
  -- | 'networkInterfaceId'
  Prelude.Text ->
  CreateStorediSCSIVolume
newCreateStorediSCSIVolume
  pGatewayARN_
  pDiskId_
  pPreserveExistingData_
  pTargetName_
  pNetworkInterfaceId_ =
    CreateStorediSCSIVolume'
      { tags = Prelude.Nothing,
        snapshotId = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        kmsEncrypted = Prelude.Nothing,
        gatewayARN = pGatewayARN_,
        diskId = pDiskId_,
        preserveExistingData = pPreserveExistingData_,
        targetName = pTargetName_,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | A list of up to 50 tags that can be assigned to a stored volume. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createStorediSCSIVolume_tags :: Lens.Lens' CreateStorediSCSIVolume (Prelude.Maybe [Tag])
createStorediSCSIVolume_tags = Lens.lens (\CreateStorediSCSIVolume' {tags} -> tags) (\s@CreateStorediSCSIVolume' {} a -> s {tags = a} :: CreateStorediSCSIVolume) Prelude.. Lens.mapping Lens.coerced

-- | The snapshot ID (e.g., \"snap-1122aabb\") of the snapshot to restore as
-- the new stored volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot; otherwise, do not include this
-- field. To list snapshots for your account use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
createStorediSCSIVolume_snapshotId :: Lens.Lens' CreateStorediSCSIVolume (Prelude.Maybe Prelude.Text)
createStorediSCSIVolume_snapshotId = Lens.lens (\CreateStorediSCSIVolume' {snapshotId} -> snapshotId) (\s@CreateStorediSCSIVolume' {} a -> s {snapshotId = a} :: CreateStorediSCSIVolume)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createStorediSCSIVolume_kmsKey :: Lens.Lens' CreateStorediSCSIVolume (Prelude.Maybe Prelude.Text)
createStorediSCSIVolume_kmsKey = Lens.lens (\CreateStorediSCSIVolume' {kmsKey} -> kmsKey) (\s@CreateStorediSCSIVolume' {} a -> s {kmsKey = a} :: CreateStorediSCSIVolume)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createStorediSCSIVolume_kmsEncrypted :: Lens.Lens' CreateStorediSCSIVolume (Prelude.Maybe Prelude.Bool)
createStorediSCSIVolume_kmsEncrypted = Lens.lens (\CreateStorediSCSIVolume' {kmsEncrypted} -> kmsEncrypted) (\s@CreateStorediSCSIVolume' {} a -> s {kmsEncrypted = a} :: CreateStorediSCSIVolume)

-- | Undocumented member.
createStorediSCSIVolume_gatewayARN :: Lens.Lens' CreateStorediSCSIVolume Prelude.Text
createStorediSCSIVolume_gatewayARN = Lens.lens (\CreateStorediSCSIVolume' {gatewayARN} -> gatewayARN) (\s@CreateStorediSCSIVolume' {} a -> s {gatewayARN = a} :: CreateStorediSCSIVolume)

-- | The unique identifier for the gateway local disk that is configured as a
-- stored volume. Use
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/API_ListLocalDisks.html ListLocalDisks>
-- to list disk IDs for a gateway.
createStorediSCSIVolume_diskId :: Lens.Lens' CreateStorediSCSIVolume Prelude.Text
createStorediSCSIVolume_diskId = Lens.lens (\CreateStorediSCSIVolume' {diskId} -> diskId) (\s@CreateStorediSCSIVolume' {} a -> s {diskId = a} :: CreateStorediSCSIVolume)

-- | Set to @true@ if you want to preserve the data on the local disk.
-- Otherwise, set to @false@ to create an empty volume.
--
-- Valid Values: @true@ | @false@
createStorediSCSIVolume_preserveExistingData :: Lens.Lens' CreateStorediSCSIVolume Prelude.Bool
createStorediSCSIVolume_preserveExistingData = Lens.lens (\CreateStorediSCSIVolume' {preserveExistingData} -> preserveExistingData) (\s@CreateStorediSCSIVolume' {} a -> s {preserveExistingData = a} :: CreateStorediSCSIVolume)

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
createStorediSCSIVolume_targetName :: Lens.Lens' CreateStorediSCSIVolume Prelude.Text
createStorediSCSIVolume_targetName = Lens.lens (\CreateStorediSCSIVolume' {targetName} -> targetName) (\s@CreateStorediSCSIVolume' {} a -> s {targetName = a} :: CreateStorediSCSIVolume)

-- | The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
createStorediSCSIVolume_networkInterfaceId :: Lens.Lens' CreateStorediSCSIVolume Prelude.Text
createStorediSCSIVolume_networkInterfaceId = Lens.lens (\CreateStorediSCSIVolume' {networkInterfaceId} -> networkInterfaceId) (\s@CreateStorediSCSIVolume' {} a -> s {networkInterfaceId = a} :: CreateStorediSCSIVolume)

instance Core.AWSRequest CreateStorediSCSIVolume where
  type
    AWSResponse CreateStorediSCSIVolume =
      CreateStorediSCSIVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStorediSCSIVolumeResponse'
            Prelude.<$> (x Core..?> "VolumeSizeInBytes")
            Prelude.<*> (x Core..?> "TargetARN")
            Prelude.<*> (x Core..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStorediSCSIVolume where
  hashWithSalt _salt CreateStorediSCSIVolume' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` diskId
      `Prelude.hashWithSalt` preserveExistingData
      `Prelude.hashWithSalt` targetName
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData CreateStorediSCSIVolume where
  rnf CreateStorediSCSIVolume' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf diskId
      `Prelude.seq` Prelude.rnf preserveExistingData
      `Prelude.seq` Prelude.rnf targetName
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Core.ToHeaders CreateStorediSCSIVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateStorediSCSIVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateStorediSCSIVolume where
  toJSON CreateStorediSCSIVolume' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("SnapshotId" Core..=) Prelude.<$> snapshotId,
            ("KMSKey" Core..=) Prelude.<$> kmsKey,
            ("KMSEncrypted" Core..=) Prelude.<$> kmsEncrypted,
            Prelude.Just ("GatewayARN" Core..= gatewayARN),
            Prelude.Just ("DiskId" Core..= diskId),
            Prelude.Just
              ( "PreserveExistingData"
                  Core..= preserveExistingData
              ),
            Prelude.Just ("TargetName" Core..= targetName),
            Prelude.Just
              ("NetworkInterfaceId" Core..= networkInterfaceId)
          ]
      )

instance Core.ToPath CreateStorediSCSIVolume where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateStorediSCSIVolume where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newCreateStorediSCSIVolumeResponse' smart constructor.
data CreateStorediSCSIVolumeResponse = CreateStorediSCSIVolumeResponse'
  { -- | The size of the volume in bytes.
    volumeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the volume target, which includes the
    -- iSCSI name that initiators can use to connect to the target.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the configured volume.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStorediSCSIVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeSizeInBytes', 'createStorediSCSIVolumeResponse_volumeSizeInBytes' - The size of the volume in bytes.
--
-- 'targetARN', 'createStorediSCSIVolumeResponse_targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name that initiators can use to connect to the target.
--
-- 'volumeARN', 'createStorediSCSIVolumeResponse_volumeARN' - The Amazon Resource Name (ARN) of the configured volume.
--
-- 'httpStatus', 'createStorediSCSIVolumeResponse_httpStatus' - The response's http status code.
newCreateStorediSCSIVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStorediSCSIVolumeResponse
newCreateStorediSCSIVolumeResponse pHttpStatus_ =
  CreateStorediSCSIVolumeResponse'
    { volumeSizeInBytes =
        Prelude.Nothing,
      targetARN = Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The size of the volume in bytes.
createStorediSCSIVolumeResponse_volumeSizeInBytes :: Lens.Lens' CreateStorediSCSIVolumeResponse (Prelude.Maybe Prelude.Integer)
createStorediSCSIVolumeResponse_volumeSizeInBytes = Lens.lens (\CreateStorediSCSIVolumeResponse' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {volumeSizeInBytes = a} :: CreateStorediSCSIVolumeResponse)

-- | The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name that initiators can use to connect to the target.
createStorediSCSIVolumeResponse_targetARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Prelude.Maybe Prelude.Text)
createStorediSCSIVolumeResponse_targetARN = Lens.lens (\CreateStorediSCSIVolumeResponse' {targetARN} -> targetARN) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {targetARN = a} :: CreateStorediSCSIVolumeResponse)

-- | The Amazon Resource Name (ARN) of the configured volume.
createStorediSCSIVolumeResponse_volumeARN :: Lens.Lens' CreateStorediSCSIVolumeResponse (Prelude.Maybe Prelude.Text)
createStorediSCSIVolumeResponse_volumeARN = Lens.lens (\CreateStorediSCSIVolumeResponse' {volumeARN} -> volumeARN) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {volumeARN = a} :: CreateStorediSCSIVolumeResponse)

-- | The response's http status code.
createStorediSCSIVolumeResponse_httpStatus :: Lens.Lens' CreateStorediSCSIVolumeResponse Prelude.Int
createStorediSCSIVolumeResponse_httpStatus = Lens.lens (\CreateStorediSCSIVolumeResponse' {httpStatus} -> httpStatus) (\s@CreateStorediSCSIVolumeResponse' {} a -> s {httpStatus = a} :: CreateStorediSCSIVolumeResponse)

instance
  Prelude.NFData
    CreateStorediSCSIVolumeResponse
  where
  rnf CreateStorediSCSIVolumeResponse' {..} =
    Prelude.rnf volumeSizeInBytes
      `Prelude.seq` Prelude.rnf targetARN
      `Prelude.seq` Prelude.rnf volumeARN
      `Prelude.seq` Prelude.rnf httpStatus
