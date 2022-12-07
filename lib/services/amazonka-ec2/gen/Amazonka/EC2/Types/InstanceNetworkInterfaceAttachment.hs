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
-- Module      : Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttachmentStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface attachment.
--
-- /See:/ 'newInstanceNetworkInterfaceAttachment' smart constructor.
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment'
  { -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the attachment initiated.
    attachTime :: Prelude.Maybe Data.ISO8601,
    -- | The index of the device on the instance for the network interface
    -- attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworkInterfaceAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkCardIndex', 'instanceNetworkInterfaceAttachment_networkCardIndex' - The index of the network card.
--
-- 'deleteOnTermination', 'instanceNetworkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'status', 'instanceNetworkInterfaceAttachment_status' - The attachment state.
--
-- 'attachmentId', 'instanceNetworkInterfaceAttachment_attachmentId' - The ID of the network interface attachment.
--
-- 'attachTime', 'instanceNetworkInterfaceAttachment_attachTime' - The time stamp when the attachment initiated.
--
-- 'deviceIndex', 'instanceNetworkInterfaceAttachment_deviceIndex' - The index of the device on the instance for the network interface
-- attachment.
newInstanceNetworkInterfaceAttachment ::
  InstanceNetworkInterfaceAttachment
newInstanceNetworkInterfaceAttachment =
  InstanceNetworkInterfaceAttachment'
    { networkCardIndex =
        Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      status = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      attachTime = Prelude.Nothing,
      deviceIndex = Prelude.Nothing
    }

-- | The index of the network card.
instanceNetworkInterfaceAttachment_networkCardIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceAttachment_networkCardIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
instanceNetworkInterfaceAttachment_deleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceAttachment_deleteOnTermination = Lens.lens (\InstanceNetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceAttachment)

-- | The attachment state.
instanceNetworkInterfaceAttachment_status :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe AttachmentStatus)
instanceNetworkInterfaceAttachment_status = Lens.lens (\InstanceNetworkInterfaceAttachment' {status} -> status) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {status = a} :: InstanceNetworkInterfaceAttachment)

-- | The ID of the network interface attachment.
instanceNetworkInterfaceAttachment_attachmentId :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAttachment_attachmentId = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: InstanceNetworkInterfaceAttachment)

-- | The time stamp when the attachment initiated.
instanceNetworkInterfaceAttachment_attachTime :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.UTCTime)
instanceNetworkInterfaceAttachment_attachTime = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachTime = a} :: InstanceNetworkInterfaceAttachment) Prelude.. Lens.mapping Data._Time

-- | The index of the device on the instance for the network interface
-- attachment.
instanceNetworkInterfaceAttachment_deviceIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceAttachment_deviceIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: InstanceNetworkInterfaceAttachment)

instance
  Data.FromXML
    InstanceNetworkInterfaceAttachment
  where
  parseXML x =
    InstanceNetworkInterfaceAttachment'
      Prelude.<$> (x Data..@? "networkCardIndex")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "attachmentId")
      Prelude.<*> (x Data..@? "attachTime")
      Prelude.<*> (x Data..@? "deviceIndex")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceAttachment
  where
  hashWithSalt
    _salt
    InstanceNetworkInterfaceAttachment' {..} =
      _salt `Prelude.hashWithSalt` networkCardIndex
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` attachmentId
        `Prelude.hashWithSalt` attachTime
        `Prelude.hashWithSalt` deviceIndex

instance
  Prelude.NFData
    InstanceNetworkInterfaceAttachment
  where
  rnf InstanceNetworkInterfaceAttachment' {..} =
    Prelude.rnf networkCardIndex
      `Prelude.seq` Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf deviceIndex
