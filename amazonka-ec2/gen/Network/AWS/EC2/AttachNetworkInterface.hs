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
-- Module      : Network.AWS.EC2.AttachNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a network interface to an instance.
module Network.AWS.EC2.AttachNetworkInterface
  ( -- * Creating a Request
    AttachNetworkInterface (..),
    newAttachNetworkInterface,

    -- * Request Lenses
    attachNetworkInterface_dryRun,
    attachNetworkInterface_networkCardIndex,
    attachNetworkInterface_deviceIndex,
    attachNetworkInterface_instanceId,
    attachNetworkInterface_networkInterfaceId,

    -- * Destructuring the Response
    AttachNetworkInterfaceResponse (..),
    newAttachNetworkInterfaceResponse,

    -- * Response Lenses
    attachNetworkInterfaceResponse_attachmentId,
    attachNetworkInterfaceResponse_networkCardIndex,
    attachNetworkInterfaceResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AttachNetworkInterface.
--
-- /See:/ 'newAttachNetworkInterface' smart constructor.
data AttachNetworkInterface = AttachNetworkInterface'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The index of the network card. Some instance types support multiple
    -- network cards. The primary network interface must be assigned to network
    -- card index 0. The default is network card index 0.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The index of the device for the network interface attachment.
    deviceIndex :: Prelude.Int,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'attachNetworkInterface_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkCardIndex', 'attachNetworkInterface_networkCardIndex' - The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
--
-- 'deviceIndex', 'attachNetworkInterface_deviceIndex' - The index of the device for the network interface attachment.
--
-- 'instanceId', 'attachNetworkInterface_instanceId' - The ID of the instance.
--
-- 'networkInterfaceId', 'attachNetworkInterface_networkInterfaceId' - The ID of the network interface.
newAttachNetworkInterface ::
  -- | 'deviceIndex'
  Prelude.Int ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'networkInterfaceId'
  Prelude.Text ->
  AttachNetworkInterface
newAttachNetworkInterface
  pDeviceIndex_
  pInstanceId_
  pNetworkInterfaceId_ =
    AttachNetworkInterface'
      { dryRun = Prelude.Nothing,
        networkCardIndex = Prelude.Nothing,
        deviceIndex = pDeviceIndex_,
        instanceId = pInstanceId_,
        networkInterfaceId = pNetworkInterfaceId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachNetworkInterface_dryRun :: Lens.Lens' AttachNetworkInterface (Prelude.Maybe Prelude.Bool)
attachNetworkInterface_dryRun = Lens.lens (\AttachNetworkInterface' {dryRun} -> dryRun) (\s@AttachNetworkInterface' {} a -> s {dryRun = a} :: AttachNetworkInterface)

-- | The index of the network card. Some instance types support multiple
-- network cards. The primary network interface must be assigned to network
-- card index 0. The default is network card index 0.
attachNetworkInterface_networkCardIndex :: Lens.Lens' AttachNetworkInterface (Prelude.Maybe Prelude.Int)
attachNetworkInterface_networkCardIndex = Lens.lens (\AttachNetworkInterface' {networkCardIndex} -> networkCardIndex) (\s@AttachNetworkInterface' {} a -> s {networkCardIndex = a} :: AttachNetworkInterface)

-- | The index of the device for the network interface attachment.
attachNetworkInterface_deviceIndex :: Lens.Lens' AttachNetworkInterface Prelude.Int
attachNetworkInterface_deviceIndex = Lens.lens (\AttachNetworkInterface' {deviceIndex} -> deviceIndex) (\s@AttachNetworkInterface' {} a -> s {deviceIndex = a} :: AttachNetworkInterface)

-- | The ID of the instance.
attachNetworkInterface_instanceId :: Lens.Lens' AttachNetworkInterface Prelude.Text
attachNetworkInterface_instanceId = Lens.lens (\AttachNetworkInterface' {instanceId} -> instanceId) (\s@AttachNetworkInterface' {} a -> s {instanceId = a} :: AttachNetworkInterface)

-- | The ID of the network interface.
attachNetworkInterface_networkInterfaceId :: Lens.Lens' AttachNetworkInterface Prelude.Text
attachNetworkInterface_networkInterfaceId = Lens.lens (\AttachNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@AttachNetworkInterface' {} a -> s {networkInterfaceId = a} :: AttachNetworkInterface)

instance Prelude.AWSRequest AttachNetworkInterface where
  type
    Rs AttachNetworkInterface =
      AttachNetworkInterfaceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AttachNetworkInterfaceResponse'
            Prelude.<$> (x Prelude..@? "attachmentId")
            Prelude.<*> (x Prelude..@? "networkCardIndex")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachNetworkInterface

instance Prelude.NFData AttachNetworkInterface

instance Prelude.ToHeaders AttachNetworkInterface where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AttachNetworkInterface where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AttachNetworkInterface where
  toQuery AttachNetworkInterface' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AttachNetworkInterface" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "NetworkCardIndex" Prelude.=: networkCardIndex,
        "DeviceIndex" Prelude.=: deviceIndex,
        "InstanceId" Prelude.=: instanceId,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId
      ]

-- | Contains the output of AttachNetworkInterface.
--
-- /See:/ 'newAttachNetworkInterfaceResponse' smart constructor.
data AttachNetworkInterfaceResponse = AttachNetworkInterfaceResponse'
  { -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachNetworkInterfaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachmentId', 'attachNetworkInterfaceResponse_attachmentId' - The ID of the network interface attachment.
--
-- 'networkCardIndex', 'attachNetworkInterfaceResponse_networkCardIndex' - The index of the network card.
--
-- 'httpStatus', 'attachNetworkInterfaceResponse_httpStatus' - The response's http status code.
newAttachNetworkInterfaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachNetworkInterfaceResponse
newAttachNetworkInterfaceResponse pHttpStatus_ =
  AttachNetworkInterfaceResponse'
    { attachmentId =
        Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the network interface attachment.
attachNetworkInterfaceResponse_attachmentId :: Lens.Lens' AttachNetworkInterfaceResponse (Prelude.Maybe Prelude.Text)
attachNetworkInterfaceResponse_attachmentId = Lens.lens (\AttachNetworkInterfaceResponse' {attachmentId} -> attachmentId) (\s@AttachNetworkInterfaceResponse' {} a -> s {attachmentId = a} :: AttachNetworkInterfaceResponse)

-- | The index of the network card.
attachNetworkInterfaceResponse_networkCardIndex :: Lens.Lens' AttachNetworkInterfaceResponse (Prelude.Maybe Prelude.Int)
attachNetworkInterfaceResponse_networkCardIndex = Lens.lens (\AttachNetworkInterfaceResponse' {networkCardIndex} -> networkCardIndex) (\s@AttachNetworkInterfaceResponse' {} a -> s {networkCardIndex = a} :: AttachNetworkInterfaceResponse)

-- | The response's http status code.
attachNetworkInterfaceResponse_httpStatus :: Lens.Lens' AttachNetworkInterfaceResponse Prelude.Int
attachNetworkInterfaceResponse_httpStatus = Lens.lens (\AttachNetworkInterfaceResponse' {httpStatus} -> httpStatus) (\s@AttachNetworkInterfaceResponse' {} a -> s {httpStatus = a} :: AttachNetworkInterfaceResponse)

instance
  Prelude.NFData
    AttachNetworkInterfaceResponse
