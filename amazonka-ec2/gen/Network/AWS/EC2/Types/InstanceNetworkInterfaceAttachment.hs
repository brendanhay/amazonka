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
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AttachmentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface attachment.
--
-- /See:/ 'newInstanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus,
    -- | The time stamp when the attachment initiated.
    attachTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The index of the device on the instance for the network interface
    -- attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      attachTime = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      deviceIndex = Prelude.Nothing
    }

-- | The attachment state.
instanceNetworkInterfaceAttachment_status :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe AttachmentStatus)
instanceNetworkInterfaceAttachment_status = Lens.lens (\InstanceNetworkInterfaceAttachment' {status} -> status) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {status = a} :: InstanceNetworkInterfaceAttachment)

-- | The time stamp when the attachment initiated.
instanceNetworkInterfaceAttachment_attachTime :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.UTCTime)
instanceNetworkInterfaceAttachment_attachTime = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachTime = a} :: InstanceNetworkInterfaceAttachment) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the network interface attachment.
instanceNetworkInterfaceAttachment_attachmentId :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAttachment_attachmentId = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: InstanceNetworkInterfaceAttachment)

-- | The index of the network card.
instanceNetworkInterfaceAttachment_networkCardIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceAttachment_networkCardIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
instanceNetworkInterfaceAttachment_deleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceAttachment_deleteOnTermination = Lens.lens (\InstanceNetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceAttachment)

-- | The index of the device on the instance for the network interface
-- attachment.
instanceNetworkInterfaceAttachment_deviceIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceAttachment_deviceIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: InstanceNetworkInterfaceAttachment)

instance
  Prelude.FromXML
    InstanceNetworkInterfaceAttachment
  where
  parseXML x =
    InstanceNetworkInterfaceAttachment'
      Prelude.<$> (x Prelude..@? "status")
      Prelude.<*> (x Prelude..@? "attachTime")
      Prelude.<*> (x Prelude..@? "attachmentId")
      Prelude.<*> (x Prelude..@? "networkCardIndex")
      Prelude.<*> (x Prelude..@? "deleteOnTermination")
      Prelude.<*> (x Prelude..@? "deviceIndex")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceAttachment

instance
  Prelude.NFData
    InstanceNetworkInterfaceAttachment
