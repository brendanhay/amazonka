{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.AttachVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connects a volume to an iSCSI connection and then attaches the volume to
-- the specified gateway. Detaching and attaching a volume enables you to
-- recover your data from one gateway to a different gateway without
-- creating a snapshot. It also makes it easier to move your volumes from
-- an on-premises gateway to a gateway hosted on an Amazon EC2 instance.
module Network.AWS.StorageGateway.AttachVolume
  ( -- * Creating a Request
    AttachVolume (..),
    newAttachVolume,

    -- * Request Lenses
    attachVolume_targetName,
    attachVolume_diskId,
    attachVolume_gatewayARN,
    attachVolume_volumeARN,
    attachVolume_networkInterfaceId,

    -- * Destructuring the Response
    AttachVolumeResponse (..),
    newAttachVolumeResponse,

    -- * Response Lenses
    attachVolumeResponse_volumeARN,
    attachVolumeResponse_targetARN,
    attachVolumeResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | AttachVolumeInput
--
-- /See:/ 'newAttachVolume' smart constructor.
data AttachVolume = AttachVolume'
  { -- | The name of the iSCSI target used by an initiator to connect to a volume
    -- and used as a suffix for the target ARN. For example, specifying
    -- @TargetName@ as /myvolume/ results in the target ARN of
    -- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
    -- The target name must be unique across all volumes on a gateway.
    --
    -- If you don\'t specify a value, Storage Gateway uses the value that was
    -- previously used for this volume as the new target name.
    targetName :: Prelude.Maybe Prelude.Text,
    -- | The unique device ID or other distinguishing data that identifies the
    -- local disk used to create the volume. This value is only required when
    -- you are attaching a stored volume.
    diskId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the gateway that you want to attach
    -- the volume to.
    gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the volume to attach to the specified
    -- gateway.
    volumeARN :: Prelude.Text,
    -- | The network interface of the gateway on which to expose the iSCSI
    -- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
    -- to get a list of the network interfaces available on a gateway.
    --
    -- Valid Values: A valid IP address.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetName', 'attachVolume_targetName' - The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
--
-- 'diskId', 'attachVolume_diskId' - The unique device ID or other distinguishing data that identifies the
-- local disk used to create the volume. This value is only required when
-- you are attaching a stored volume.
--
-- 'gatewayARN', 'attachVolume_gatewayARN' - The Amazon Resource Name (ARN) of the gateway that you want to attach
-- the volume to.
--
-- 'volumeARN', 'attachVolume_volumeARN' - The Amazon Resource Name (ARN) of the volume to attach to the specified
-- gateway.
--
-- 'networkInterfaceId', 'attachVolume_networkInterfaceId' - The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
newAttachVolume ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'volumeARN'
  Prelude.Text ->
  -- | 'networkInterfaceId'
  Prelude.Text ->
  AttachVolume
newAttachVolume
  pGatewayARN_
  pVolumeARN_
  pNetworkInterfaceId_ =
    AttachVolume'
      { targetName = Prelude.Nothing,
        diskId = Prelude.Nothing,
        gatewayARN = pGatewayARN_,
        volumeARN = pVolumeARN_,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | The name of the iSCSI target used by an initiator to connect to a volume
-- and used as a suffix for the target ARN. For example, specifying
-- @TargetName@ as /myvolume/ results in the target ARN of
-- @arn:aws:storagegateway:us-east-2:111122223333:gateway\/sgw-12A3456B\/target\/iqn.1997-05.com.amazon:myvolume@.
-- The target name must be unique across all volumes on a gateway.
--
-- If you don\'t specify a value, Storage Gateway uses the value that was
-- previously used for this volume as the new target name.
attachVolume_targetName :: Lens.Lens' AttachVolume (Prelude.Maybe Prelude.Text)
attachVolume_targetName = Lens.lens (\AttachVolume' {targetName} -> targetName) (\s@AttachVolume' {} a -> s {targetName = a} :: AttachVolume)

-- | The unique device ID or other distinguishing data that identifies the
-- local disk used to create the volume. This value is only required when
-- you are attaching a stored volume.
attachVolume_diskId :: Lens.Lens' AttachVolume (Prelude.Maybe Prelude.Text)
attachVolume_diskId = Lens.lens (\AttachVolume' {diskId} -> diskId) (\s@AttachVolume' {} a -> s {diskId = a} :: AttachVolume)

-- | The Amazon Resource Name (ARN) of the gateway that you want to attach
-- the volume to.
attachVolume_gatewayARN :: Lens.Lens' AttachVolume Prelude.Text
attachVolume_gatewayARN = Lens.lens (\AttachVolume' {gatewayARN} -> gatewayARN) (\s@AttachVolume' {} a -> s {gatewayARN = a} :: AttachVolume)

-- | The Amazon Resource Name (ARN) of the volume to attach to the specified
-- gateway.
attachVolume_volumeARN :: Lens.Lens' AttachVolume Prelude.Text
attachVolume_volumeARN = Lens.lens (\AttachVolume' {volumeARN} -> volumeARN) (\s@AttachVolume' {} a -> s {volumeARN = a} :: AttachVolume)

-- | The network interface of the gateway on which to expose the iSCSI
-- target. Only IPv4 addresses are accepted. Use DescribeGatewayInformation
-- to get a list of the network interfaces available on a gateway.
--
-- Valid Values: A valid IP address.
attachVolume_networkInterfaceId :: Lens.Lens' AttachVolume Prelude.Text
attachVolume_networkInterfaceId = Lens.lens (\AttachVolume' {networkInterfaceId} -> networkInterfaceId) (\s@AttachVolume' {} a -> s {networkInterfaceId = a} :: AttachVolume)

instance Prelude.AWSRequest AttachVolume where
  type Rs AttachVolume = AttachVolumeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachVolumeResponse'
            Prelude.<$> (x Prelude..?> "VolumeARN")
            Prelude.<*> (x Prelude..?> "TargetARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachVolume

instance Prelude.NFData AttachVolume

instance Prelude.ToHeaders AttachVolume where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.AttachVolume" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AttachVolume where
  toJSON AttachVolume' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TargetName" Prelude..=) Prelude.<$> targetName,
            ("DiskId" Prelude..=) Prelude.<$> diskId,
            Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just ("VolumeARN" Prelude..= volumeARN),
            Prelude.Just
              ( "NetworkInterfaceId"
                  Prelude..= networkInterfaceId
              )
          ]
      )

instance Prelude.ToPath AttachVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AttachVolume where
  toQuery = Prelude.const Prelude.mempty

-- | AttachVolumeOutput
--
-- /See:/ 'newAttachVolumeResponse' smart constructor.
data AttachVolumeResponse = AttachVolumeResponse'
  { -- | The Amazon Resource Name (ARN) of the volume that was attached to the
    -- gateway.
    volumeARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the volume target, which includes the
    -- iSCSI name for the initiator that was used to connect to the target.
    targetARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachVolumeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARN', 'attachVolumeResponse_volumeARN' - The Amazon Resource Name (ARN) of the volume that was attached to the
-- gateway.
--
-- 'targetARN', 'attachVolumeResponse_targetARN' - The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name for the initiator that was used to connect to the target.
--
-- 'httpStatus', 'attachVolumeResponse_httpStatus' - The response's http status code.
newAttachVolumeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachVolumeResponse
newAttachVolumeResponse pHttpStatus_ =
  AttachVolumeResponse'
    { volumeARN = Prelude.Nothing,
      targetARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume that was attached to the
-- gateway.
attachVolumeResponse_volumeARN :: Lens.Lens' AttachVolumeResponse (Prelude.Maybe Prelude.Text)
attachVolumeResponse_volumeARN = Lens.lens (\AttachVolumeResponse' {volumeARN} -> volumeARN) (\s@AttachVolumeResponse' {} a -> s {volumeARN = a} :: AttachVolumeResponse)

-- | The Amazon Resource Name (ARN) of the volume target, which includes the
-- iSCSI name for the initiator that was used to connect to the target.
attachVolumeResponse_targetARN :: Lens.Lens' AttachVolumeResponse (Prelude.Maybe Prelude.Text)
attachVolumeResponse_targetARN = Lens.lens (\AttachVolumeResponse' {targetARN} -> targetARN) (\s@AttachVolumeResponse' {} a -> s {targetARN = a} :: AttachVolumeResponse)

-- | The response's http status code.
attachVolumeResponse_httpStatus :: Lens.Lens' AttachVolumeResponse Prelude.Int
attachVolumeResponse_httpStatus = Lens.lens (\AttachVolumeResponse' {httpStatus} -> httpStatus) (\s@AttachVolumeResponse' {} a -> s {httpStatus = a} :: AttachVolumeResponse)

instance Prelude.NFData AttachVolumeResponse
