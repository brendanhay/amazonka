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
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens

-- | Describes a network interface attachment.
--
-- /See:/ 'newInstanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { -- | The attachment state.
    status :: Core.Maybe AttachmentStatus,
    -- | The time stamp when the attachment initiated.
    attachTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the network interface attachment.
    attachmentId :: Core.Maybe Core.Text,
    -- | The index of the network card.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The index of the device on the instance for the network interface
    -- attachment.
    deviceIndex :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceNetworkInterfaceAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'instanceNetworkInterfaceAttachment_status' - The attachment state.
--
-- 'attachTime', 'instanceNetworkInterfaceAttachment_attachTime' - The time stamp when the attachment initiated.
--
-- 'attachmentId', 'instanceNetworkInterfaceAttachment_attachmentId' - The ID of the network interface attachment.
--
-- 'networkCardIndex', 'instanceNetworkInterfaceAttachment_networkCardIndex' - The index of the network card.
--
-- 'deleteOnTermination', 'instanceNetworkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'deviceIndex', 'instanceNetworkInterfaceAttachment_deviceIndex' - The index of the device on the instance for the network interface
-- attachment.
newInstanceNetworkInterfaceAttachment ::
  InstanceNetworkInterfaceAttachment
newInstanceNetworkInterfaceAttachment =
  InstanceNetworkInterfaceAttachment'
    { status =
        Core.Nothing,
      attachTime = Core.Nothing,
      attachmentId = Core.Nothing,
      networkCardIndex = Core.Nothing,
      deleteOnTermination = Core.Nothing,
      deviceIndex = Core.Nothing
    }

-- | The attachment state.
instanceNetworkInterfaceAttachment_status :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe AttachmentStatus)
instanceNetworkInterfaceAttachment_status = Lens.lens (\InstanceNetworkInterfaceAttachment' {status} -> status) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {status = a} :: InstanceNetworkInterfaceAttachment)

-- | The time stamp when the attachment initiated.
instanceNetworkInterfaceAttachment_attachTime :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.UTCTime)
instanceNetworkInterfaceAttachment_attachTime = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachTime = a} :: InstanceNetworkInterfaceAttachment) Core.. Lens.mapping Core._Time

-- | The ID of the network interface attachment.
instanceNetworkInterfaceAttachment_attachmentId :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Text)
instanceNetworkInterfaceAttachment_attachmentId = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: InstanceNetworkInterfaceAttachment)

-- | The index of the network card.
instanceNetworkInterfaceAttachment_networkCardIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Int)
instanceNetworkInterfaceAttachment_networkCardIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
instanceNetworkInterfaceAttachment_deleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Bool)
instanceNetworkInterfaceAttachment_deleteOnTermination = Lens.lens (\InstanceNetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceAttachment)

-- | The index of the device on the instance for the network interface
-- attachment.
instanceNetworkInterfaceAttachment_deviceIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Core.Maybe Core.Int)
instanceNetworkInterfaceAttachment_deviceIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: InstanceNetworkInterfaceAttachment)

instance
  Core.FromXML
    InstanceNetworkInterfaceAttachment
  where
  parseXML x =
    InstanceNetworkInterfaceAttachment'
      Core.<$> (x Core..@? "status")
      Core.<*> (x Core..@? "attachTime")
      Core.<*> (x Core..@? "attachmentId")
      Core.<*> (x Core..@? "networkCardIndex")
      Core.<*> (x Core..@? "deleteOnTermination")
      Core.<*> (x Core..@? "deviceIndex")

instance
  Core.Hashable
    InstanceNetworkInterfaceAttachment

instance
  Core.NFData
    InstanceNetworkInterfaceAttachment
