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
-- Module      : Amazonka.StorageGateway.CreateCachediSCSIVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cached volume on a specified cached volume gateway. This
-- operation is only supported in the cached volume gateway type.
--
-- Cache storage must be allocated to the gateway before you can create a
-- cached volume. Use the AddCache operation to add cache storage to a
-- gateway.
--
-- In the request, you must specify the gateway, size of the volume in
-- bytes, the iSCSI target name, an IP address on which to expose the
-- target, and a unique client token. In response, the gateway creates the
-- volume and returns information about it. This information includes the
-- volume Amazon Resource Name (ARN), its size, and the iSCSI target ARN
-- that initiators can use to connect to the volume target.
--
-- Optionally, you can provide the ARN for an existing volume as the
-- @SourceVolumeARN@ for this cached volume, which creates an exact copy of
-- the existing volume’s latest recovery point. The @VolumeSizeInBytes@
-- value must be equal to or larger than the size of the copied volume, in
-- bytes.
module Amazonka.StorageGateway.CreateCachediSCSIVolume
  ( -- * Creating a Request
    CreateCachediSCSIVolume (..),
    newCreateCachediSCSIVolume,

    -- * Request Lenses
    createCachediSCSIVolume_kmsEncrypted,
    createCachediSCSIVolume_kmsKey,
    createCachediSCSIVolume_snapshotId,
    createCachediSCSIVolume_sourceVolumeARN,
    createCachediSCSIVolume_tags,
    createCachediSCSIVolume_gatewayARN,
    createCachediSCSIVolume_volumeSizeInBytes,
    createCachediSCSIVolume_targetName,
    createCachediSCSIVolume_networkInterfaceId,
    createCachediSCSIVolume_clientToken,

    -- * Destructuring the Response
    CreateCachediSCSIVolumeResponse (..),
    newCreateCachediSCSIVolumeResponse,

    -- * Response Lenses
    createCachediSCSIVolumeResponse_targetARN,
    createCachediSCSIVolumeResponse_volumeARN,
    createCachediSCSIVolumeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newCreateCachediSCSIVolume' smart constructor.
data CreateCachediSCSIVolume = CreateCachediSCSIVolume'
  { -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
    -- the new cached volume. Specify this field if you want to create the
    -- iSCSI storage volume from a snapshot; otherwise, do not include this
    -- field. To list snapshots for your account use
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
    -- in the /Amazon Elastic Compute Cloud API Reference/.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for an existing volume. Specifying this ARN makes the new volume
    -- into an exact copy of the specified existing volume\'s latest recovery
    -- point. The @VolumeSizeInBytes@ value for this new volume must be equal
    -- to or larger than the size of the existing volume, in bytes.
    sourceVolumeARN :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags that you can assign to a cached volume. Each tag
    -- is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers that
    -- you can represent in UTF-8 format, and the following special characters:
    -- + - = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters,
    -- and the maximum length for a tag\'s value is 256 characters.
    tags :: Prelude.Maybe [Tag],
    gatewayARN :: Prelude.Text,
    -- | The size of the volume in bytes.
    volumeSizeInBytes :: Prelude.Integer,
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
    networkInterfaceId :: Prelude.Text,
    -- | A unique identifier that you use to retry a request. If you retry a
    -- request, use the same @ClientToken@ you specified in the initial
    -- request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCachediSCSIVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsEncrypted', 'createCachediSCSIVolume_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsKey', 'createCachediSCSIVolume_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'snapshotId', 'createCachediSCSIVolume_snapshotId' - The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
-- the new cached volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot; otherwise, do not include this
-- field. To list snapshots for your account use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
--
-- 'sourceVolumeARN', 'createCachediSCSIVolume_sourceVolumeARN' - The ARN for an existing volume. Specifying this ARN makes the new volume
-- into an exact copy of the specified existing volume\'s latest recovery
-- point. The @VolumeSizeInBytes@ value for this new volume must be equal
-- to or larger than the size of the existing volume, in bytes.
--
-- 'tags', 'createCachediSCSIVolume_tags' - A list of up to 50 tags that you can assign to a cached volume. Each tag
-- is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers that
-- you can represent in UTF-8 format, and the following special characters:
-- + - = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters,
-- and the maximum length for a tag\'s value is 256 characters.
--
-- 'gatewayARN', 'createCachediSCSIVolume_gatewayARN' - Undocumented member.
--
-- 'volumeSizeInBytes', 'createCachediSCSIVolume_volumeSizeInBytes' - The size of the volume in bytes.
--
-- 'targetName', 'createCachediSCSIVolume_targetName' - The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
--
-- 'networkInterfaceId', 'createCachediSCSIVolume_networkInterfaceId' - The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
--
-- 'clientToken', 'createCachediSCSIVolume_clientToken' - A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
newCreateCachediSCSIVolume ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'volumeSizeInBytes'
  Prelude.Integer ->
  -- | 'targetName'
  Prelude.Text ->
  -- | 'networkInterfaceId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateCachediSCSIVolume
newCreateCachediSCSIVolume
  pGatewayARN_
  pVolumeSizeInBytes_
  pTargetName_
  pNetworkInterfaceId_
  pClientToken_ =
    CreateCachediSCSIVolume'
      { kmsEncrypted =
          Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        snapshotId = Prelude.Nothing,
        sourceVolumeARN = Prelude.Nothing,
        tags = Prelude.Nothing,
        gatewayARN = pGatewayARN_,
        volumeSizeInBytes = pVolumeSizeInBytes_,
        targetName = pTargetName_,
        networkInterfaceId = pNetworkInterfaceId_,
        clientToken = pClientToken_
      }

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createCachediSCSIVolume_kmsEncrypted :: Lens.Lens' CreateCachediSCSIVolume (Prelude.Maybe Prelude.Bool)
createCachediSCSIVolume_kmsEncrypted = Lens.lens (\CreateCachediSCSIVolume' {kmsEncrypted} -> kmsEncrypted) (\s@CreateCachediSCSIVolume' {} a -> s {kmsEncrypted = a} :: CreateCachediSCSIVolume)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createCachediSCSIVolume_kmsKey :: Lens.Lens' CreateCachediSCSIVolume (Prelude.Maybe Prelude.Text)
createCachediSCSIVolume_kmsKey = Lens.lens (\CreateCachediSCSIVolume' {kmsKey} -> kmsKey) (\s@CreateCachediSCSIVolume' {} a -> s {kmsKey = a} :: CreateCachediSCSIVolume)

-- | The snapshot ID (e.g. \"snap-1122aabb\") of the snapshot to restore as
-- the new cached volume. Specify this field if you want to create the
-- iSCSI storage volume from a snapshot; otherwise, do not include this
-- field. To list snapshots for your account use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
createCachediSCSIVolume_snapshotId :: Lens.Lens' CreateCachediSCSIVolume (Prelude.Maybe Prelude.Text)
createCachediSCSIVolume_snapshotId = Lens.lens (\CreateCachediSCSIVolume' {snapshotId} -> snapshotId) (\s@CreateCachediSCSIVolume' {} a -> s {snapshotId = a} :: CreateCachediSCSIVolume)

-- | The ARN for an existing volume. Specifying this ARN makes the new volume
-- into an exact copy of the specified existing volume\'s latest recovery
-- point. The @VolumeSizeInBytes@ value for this new volume must be equal
-- to or larger than the size of the existing volume, in bytes.
createCachediSCSIVolume_sourceVolumeARN :: Lens.Lens' CreateCachediSCSIVolume (Prelude.Maybe Prelude.Text)
createCachediSCSIVolume_sourceVolumeARN = Lens.lens (\CreateCachediSCSIVolume' {sourceVolumeARN} -> sourceVolumeARN) (\s@CreateCachediSCSIVolume' {} a -> s {sourceVolumeARN = a} :: CreateCachediSCSIVolume)

-- | A list of up to 50 tags that you can assign to a cached volume. Each tag
-- is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers that
-- you can represent in UTF-8 format, and the following special characters:
-- + - = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters,
-- and the maximum length for a tag\'s value is 256 characters.
createCachediSCSIVolume_tags :: Lens.Lens' CreateCachediSCSIVolume (Prelude.Maybe [Tag])
createCachediSCSIVolume_tags = Lens.lens (\CreateCachediSCSIVolume' {tags} -> tags) (\s@CreateCachediSCSIVolume' {} a -> s {tags = a} :: CreateCachediSCSIVolume) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createCachediSCSIVolume_gatewayARN :: Lens.Lens' CreateCachediSCSIVolume Prelude.Text
createCachediSCSIVolume_gatewayARN = Lens.lens (\CreateCachediSCSIVolume' {gatewayARN} -> gatewayARN) (\s@CreateCachediSCSIVolume' {} a -> s {gatewayARN = a} :: CreateCachediSCSIVolume)

-- | The size of the volume in bytes.
createCachediSCSIVolume_volumeSizeInBytes :: Lens.Lens' CreateCachediSCSIVolume Prelude.Integer
createCachediSCSIVolume_volumeSizeInBytes = Lens.lens (\CreateCachediSCSIVolume' {volumeSizeInBytes} -> volumeSizeInBytes) (\s@CreateCachediSCSIVolume' {} a -> s {volumeSizeInBytes = a} :: CreateCachediSCSIVolume)

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
createCachediSCSIVolume_targetName :: Lens.Lens' CreateCachediSCSIVolume Prelude.Text
createCachediSCSIVolume_targetName = Lens.lens (\CreateCachediSCSIVolume' {targetName} -> targetName) (\s@CreateCachediSCSIVolume' {} a -> s {targetName = a} :: CreateCachediSCSIVolume)

-- | The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
createCachediSCSIVolume_networkInterfaceId :: Lens.Lens' CreateCachediSCSIVolume Prelude.Text
createCachediSCSIVolume_networkInterfaceId = Lens.lens (\CreateCachediSCSIVolume' {networkInterfaceId} -> networkInterfaceId) (\s@CreateCachediSCSIVolume' {} a -> s {networkInterfaceId = a} :: CreateCachediSCSIVolume)

-- | A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
createCachediSCSIVolume_clientToken :: Lens.Lens' CreateCachediSCSIVolume Prelude.Text
createCachediSCSIVolume_clientToken = Lens.lens (\CreateCachediSCSIVolume' {clientToken} -> clientToken) (\s@CreateCachediSCSIVolume' {} a -> s {clientToken = a} :: CreateCachediSCSIVolume)

instance Core.AWSRequest CreateCachediSCSIVolume where
  type
    AWSResponse CreateCachediSCSIVolume =
      CreateCachediSCSIVolumeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCachediSCSIVolumeResponse'
            Prelude.<$> (x Data..?> "TargetARN")
            Prelude.<*> (x Data..?> "VolumeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCachediSCSIVolume where
  hashWithSalt _salt CreateCachediSCSIVolume' {..} =
    _salt
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` sourceVolumeARN
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` volumeSizeInBytes
      `Prelude.hashWithSalt` targetName
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateCachediSCSIVolume where
  rnf CreateCachediSCSIVolume' {..} =
    Prelude.rnf kmsEncrypted `Prelude.seq`
      Prelude.rnf kmsKey `Prelude.seq`
        Prelude.rnf snapshotId `Prelude.seq`
          Prelude.rnf sourceVolumeARN `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf gatewayARN `Prelude.seq`
                Prelude.rnf volumeSizeInBytes `Prelude.seq`
                  Prelude.rnf targetName `Prelude.seq`
                    Prelude.rnf networkInterfaceId `Prelude.seq`
                      Prelude.rnf clientToken

instance Data.ToHeaders CreateCachediSCSIVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.CreateCachediSCSIVolume" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCachediSCSIVolume where
  toJSON CreateCachediSCSIVolume' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KMSEncrypted" Data..=) Prelude.<$> kmsEncrypted,
            ("KMSKey" Data..=) Prelude.<$> kmsKey,
            ("SnapshotId" Data..=) Prelude.<$> snapshotId,
            ("SourceVolumeARN" Data..=)
              Prelude.<$> sourceVolumeARN,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just
              ("VolumeSizeInBytes" Data..= volumeSizeInBytes),
            Prelude.Just ("TargetName" Data..= targetName),
            Prelude.Just
              ("NetworkInterfaceId" Data..= networkInterfaceId),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateCachediSCSIVolume where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCachediSCSIVolume where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCachediSCSIVolumeResponse' smart constructor.
data CreateCachediSCSIVolumeResponse = CreateCachediSCSIVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the volume target, which includes the
    -- iSCSI name that initiators can use to connect to the target.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the configured volume.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCachediSCSIVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetARN', 'createCachediSCSIVolumeResponse_targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name that initiators can use to connect to the target.
--
-- 'volumeARN', 'createCachediSCSIVolumeResponse_volumeARN' - The Amazon Resource Name (ARN) of the configured volume.
--
-- 'httpStatus', 'createCachediSCSIVolumeResponse_httpStatus' - The response's http status code.
newCreateCachediSCSIVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCachediSCSIVolumeResponse
newCreateCachediSCSIVolumeResponse pHttpStatus_ =
  CreateCachediSCSIVolumeResponse'
    { targetARN =
        Prelude.Nothing,
      volumeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name that initiators can use to connect to the target.
createCachediSCSIVolumeResponse_targetARN :: Lens.Lens' CreateCachediSCSIVolumeResponse (Prelude.Maybe Prelude.Text)
createCachediSCSIVolumeResponse_targetARN = Lens.lens (\CreateCachediSCSIVolumeResponse' {targetARN} -> targetARN) (\s@CreateCachediSCSIVolumeResponse' {} a -> s {targetARN = a} :: CreateCachediSCSIVolumeResponse)

-- | The Amazon Resource Name (ARN) of the configured volume.
createCachediSCSIVolumeResponse_volumeARN :: Lens.Lens' CreateCachediSCSIVolumeResponse (Prelude.Maybe Prelude.Text)
createCachediSCSIVolumeResponse_volumeARN = Lens.lens (\CreateCachediSCSIVolumeResponse' {volumeARN} -> volumeARN) (\s@CreateCachediSCSIVolumeResponse' {} a -> s {volumeARN = a} :: CreateCachediSCSIVolumeResponse)

-- | The response's http status code.
createCachediSCSIVolumeResponse_httpStatus :: Lens.Lens' CreateCachediSCSIVolumeResponse Prelude.Int
createCachediSCSIVolumeResponse_httpStatus = Lens.lens (\CreateCachediSCSIVolumeResponse' {httpStatus} -> httpStatus) (\s@CreateCachediSCSIVolumeResponse' {} a -> s {httpStatus = a} :: CreateCachediSCSIVolumeResponse)

instance
  Prelude.NFData
    CreateCachediSCSIVolumeResponse
  where
  rnf CreateCachediSCSIVolumeResponse' {..} =
    Prelude.rnf targetARN `Prelude.seq`
      Prelude.rnf volumeARN `Prelude.seq`
        Prelude.rnf httpStatus
