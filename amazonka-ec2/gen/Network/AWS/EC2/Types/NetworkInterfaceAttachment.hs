{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface attachment.
--
-- /See:/ 'newNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp indicating when the attachment initiated.
    attachTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The AWS account ID of the owner of the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'networkInterfaceAttachment_status' - The attachment state.
--
-- 'instanceId', 'networkInterfaceAttachment_instanceId' - The ID of the instance.
--
-- 'attachTime', 'networkInterfaceAttachment_attachTime' - The timestamp indicating when the attachment initiated.
--
-- 'attachmentId', 'networkInterfaceAttachment_attachmentId' - The ID of the network interface attachment.
--
-- 'networkCardIndex', 'networkInterfaceAttachment_networkCardIndex' - The index of the network card.
--
-- 'deleteOnTermination', 'networkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'deviceIndex', 'networkInterfaceAttachment_deviceIndex' - The device index of the network interface attachment on the instance.
--
-- 'instanceOwnerId', 'networkInterfaceAttachment_instanceOwnerId' - The AWS account ID of the owner of the instance.
newNetworkInterfaceAttachment ::
  NetworkInterfaceAttachment
newNetworkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { status =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      attachTime = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      instanceOwnerId = Prelude.Nothing
    }

-- | The attachment state.
networkInterfaceAttachment_status :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe AttachmentStatus)
networkInterfaceAttachment_status = Lens.lens (\NetworkInterfaceAttachment' {status} -> status) (\s@NetworkInterfaceAttachment' {} a -> s {status = a} :: NetworkInterfaceAttachment)

-- | The ID of the instance.
networkInterfaceAttachment_instanceId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_instanceId = Lens.lens (\NetworkInterfaceAttachment' {instanceId} -> instanceId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceId = a} :: NetworkInterfaceAttachment)

-- | The timestamp indicating when the attachment initiated.
networkInterfaceAttachment_attachTime :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.UTCTime)
networkInterfaceAttachment_attachTime = Lens.lens (\NetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@NetworkInterfaceAttachment' {} a -> s {attachTime = a} :: NetworkInterfaceAttachment) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the network interface attachment.
networkInterfaceAttachment_attachmentId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_attachmentId = Lens.lens (\NetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachment)

-- | The index of the network card.
networkInterfaceAttachment_networkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
networkInterfaceAttachment_networkCardIndex = Lens.lens (\NetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@NetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: NetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachment_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
networkInterfaceAttachment_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachment)

-- | The device index of the network interface attachment on the instance.
networkInterfaceAttachment_deviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
networkInterfaceAttachment_deviceIndex = Lens.lens (\NetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@NetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: NetworkInterfaceAttachment)

-- | The AWS account ID of the owner of the instance.
networkInterfaceAttachment_instanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_instanceOwnerId = Lens.lens (\NetworkInterfaceAttachment' {instanceOwnerId} -> instanceOwnerId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceOwnerId = a} :: NetworkInterfaceAttachment)

instance Prelude.FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      Prelude.<$> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "attachTime")
      Prelude.<*> (x Prelude..@? "attachmentId")
      Prelude.<*> (x Prelude..@? "networkCardIndex")
      Prelude.<*> (x Prelude..@? "deleteOnTermination")
      Prelude.<*> (x Prelude..@? "deviceIndex")
      Prelude.<*> (x Prelude..@? "instanceOwnerId")

instance Prelude.Hashable NetworkInterfaceAttachment

instance Prelude.NFData NetworkInterfaceAttachment
