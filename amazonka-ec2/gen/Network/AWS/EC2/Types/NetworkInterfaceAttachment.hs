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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens

-- | Describes a network interface attachment.
--
-- /See:/ 'newNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { -- | The attachment state.
    status :: Core.Maybe AttachmentStatus,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The timestamp indicating when the attachment initiated.
    attachTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the network interface attachment.
    attachmentId :: Core.Maybe Core.Text,
    -- | The index of the network card.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The AWS account ID of the owner of the instance.
    instanceOwnerId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      instanceId = Core.Nothing,
      attachTime = Core.Nothing,
      attachmentId = Core.Nothing,
      networkCardIndex = Core.Nothing,
      deleteOnTermination = Core.Nothing,
      deviceIndex = Core.Nothing,
      instanceOwnerId = Core.Nothing
    }

-- | The attachment state.
networkInterfaceAttachment_status :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe AttachmentStatus)
networkInterfaceAttachment_status = Lens.lens (\NetworkInterfaceAttachment' {status} -> status) (\s@NetworkInterfaceAttachment' {} a -> s {status = a} :: NetworkInterfaceAttachment)

-- | The ID of the instance.
networkInterfaceAttachment_instanceId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Text)
networkInterfaceAttachment_instanceId = Lens.lens (\NetworkInterfaceAttachment' {instanceId} -> instanceId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceId = a} :: NetworkInterfaceAttachment)

-- | The timestamp indicating when the attachment initiated.
networkInterfaceAttachment_attachTime :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.UTCTime)
networkInterfaceAttachment_attachTime = Lens.lens (\NetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@NetworkInterfaceAttachment' {} a -> s {attachTime = a} :: NetworkInterfaceAttachment) Core.. Lens.mapping Core._Time

-- | The ID of the network interface attachment.
networkInterfaceAttachment_attachmentId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Text)
networkInterfaceAttachment_attachmentId = Lens.lens (\NetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachment)

-- | The index of the network card.
networkInterfaceAttachment_networkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Int)
networkInterfaceAttachment_networkCardIndex = Lens.lens (\NetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@NetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: NetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachment_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Bool)
networkInterfaceAttachment_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachment)

-- | The device index of the network interface attachment on the instance.
networkInterfaceAttachment_deviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Int)
networkInterfaceAttachment_deviceIndex = Lens.lens (\NetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@NetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: NetworkInterfaceAttachment)

-- | The AWS account ID of the owner of the instance.
networkInterfaceAttachment_instanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Core.Maybe Core.Text)
networkInterfaceAttachment_instanceOwnerId = Lens.lens (\NetworkInterfaceAttachment' {instanceOwnerId} -> instanceOwnerId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceOwnerId = a} :: NetworkInterfaceAttachment)

instance Core.FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      Core.<$> (x Core..@? "status")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "attachTime")
      Core.<*> (x Core..@? "attachmentId")
      Core.<*> (x Core..@? "networkCardIndex")
      Core.<*> (x Core..@? "deleteOnTermination")
      Core.<*> (x Core..@? "deviceIndex")
      Core.<*> (x Core..@? "instanceOwnerId")

instance Core.Hashable NetworkInterfaceAttachment

instance Core.NFData NetworkInterfaceAttachment
