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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The time stamp when the attachment initiated.
    attachTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The index of the device on the instance for the network interface
    -- attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus
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
-- 'attachTime', 'instanceNetworkInterfaceAttachment_attachTime' - The time stamp when the attachment initiated.
--
-- 'attachmentId', 'instanceNetworkInterfaceAttachment_attachmentId' - The ID of the network interface attachment.
--
-- 'deleteOnTermination', 'instanceNetworkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'deviceIndex', 'instanceNetworkInterfaceAttachment_deviceIndex' - The index of the device on the instance for the network interface
-- attachment.
--
-- 'networkCardIndex', 'instanceNetworkInterfaceAttachment_networkCardIndex' - The index of the network card.
--
-- 'status', 'instanceNetworkInterfaceAttachment_status' - The attachment state.
newInstanceNetworkInterfaceAttachment ::
  InstanceNetworkInterfaceAttachment
newInstanceNetworkInterfaceAttachment =
  InstanceNetworkInterfaceAttachment'
    { attachTime =
        Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The time stamp when the attachment initiated.
instanceNetworkInterfaceAttachment_attachTime :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.UTCTime)
instanceNetworkInterfaceAttachment_attachTime = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachTime = a} :: InstanceNetworkInterfaceAttachment) Prelude.. Lens.mapping Data._Time

-- | The ID of the network interface attachment.
instanceNetworkInterfaceAttachment_attachmentId :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
instanceNetworkInterfaceAttachment_attachmentId = Lens.lens (\InstanceNetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: InstanceNetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
instanceNetworkInterfaceAttachment_deleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
instanceNetworkInterfaceAttachment_deleteOnTermination = Lens.lens (\InstanceNetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: InstanceNetworkInterfaceAttachment)

-- | The index of the device on the instance for the network interface
-- attachment.
instanceNetworkInterfaceAttachment_deviceIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceAttachment_deviceIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: InstanceNetworkInterfaceAttachment)

-- | The index of the network card.
instanceNetworkInterfaceAttachment_networkCardIndex :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
instanceNetworkInterfaceAttachment_networkCardIndex = Lens.lens (\InstanceNetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: InstanceNetworkInterfaceAttachment)

-- | The attachment state.
instanceNetworkInterfaceAttachment_status :: Lens.Lens' InstanceNetworkInterfaceAttachment (Prelude.Maybe AttachmentStatus)
instanceNetworkInterfaceAttachment_status = Lens.lens (\InstanceNetworkInterfaceAttachment' {status} -> status) (\s@InstanceNetworkInterfaceAttachment' {} a -> s {status = a} :: InstanceNetworkInterfaceAttachment)

instance
  Data.FromXML
    InstanceNetworkInterfaceAttachment
  where
  parseXML x =
    InstanceNetworkInterfaceAttachment'
      Prelude.<$> (x Data..@? "attachTime")
      Prelude.<*> (x Data..@? "attachmentId")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "deviceIndex")
      Prelude.<*> (x Data..@? "networkCardIndex")
      Prelude.<*> (x Data..@? "status")

instance
  Prelude.Hashable
    InstanceNetworkInterfaceAttachment
  where
  hashWithSalt
    _salt
    InstanceNetworkInterfaceAttachment' {..} =
      _salt
        `Prelude.hashWithSalt` attachTime
        `Prelude.hashWithSalt` attachmentId
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` networkCardIndex
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    InstanceNetworkInterfaceAttachment
  where
  rnf InstanceNetworkInterfaceAttachment' {..} =
    Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf deviceIndex
      `Prelude.seq` Prelude.rnf networkCardIndex
      `Prelude.seq` Prelude.rnf status
