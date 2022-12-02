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
-- Module      : Amazonka.EC2.Types.NetworkInterfaceAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttachmentStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface attachment.
--
-- /See:/ 'newNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp indicating when the attachment initiated.
    attachTime :: Prelude.Maybe Data.ISO8601,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account ID of the owner of the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkCardIndex', 'networkInterfaceAttachment_networkCardIndex' - The index of the network card.
--
-- 'deleteOnTermination', 'networkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'status', 'networkInterfaceAttachment_status' - The attachment state.
--
-- 'attachmentId', 'networkInterfaceAttachment_attachmentId' - The ID of the network interface attachment.
--
-- 'instanceId', 'networkInterfaceAttachment_instanceId' - The ID of the instance.
--
-- 'attachTime', 'networkInterfaceAttachment_attachTime' - The timestamp indicating when the attachment initiated.
--
-- 'deviceIndex', 'networkInterfaceAttachment_deviceIndex' - The device index of the network interface attachment on the instance.
--
-- 'instanceOwnerId', 'networkInterfaceAttachment_instanceOwnerId' - The Amazon Web Services account ID of the owner of the instance.
newNetworkInterfaceAttachment ::
  NetworkInterfaceAttachment
newNetworkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { networkCardIndex =
        Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      status = Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      attachTime = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      instanceOwnerId = Prelude.Nothing
    }

-- | The index of the network card.
networkInterfaceAttachment_networkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
networkInterfaceAttachment_networkCardIndex = Lens.lens (\NetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@NetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: NetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachment_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
networkInterfaceAttachment_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachment)

-- | The attachment state.
networkInterfaceAttachment_status :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe AttachmentStatus)
networkInterfaceAttachment_status = Lens.lens (\NetworkInterfaceAttachment' {status} -> status) (\s@NetworkInterfaceAttachment' {} a -> s {status = a} :: NetworkInterfaceAttachment)

-- | The ID of the network interface attachment.
networkInterfaceAttachment_attachmentId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_attachmentId = Lens.lens (\NetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachment)

-- | The ID of the instance.
networkInterfaceAttachment_instanceId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_instanceId = Lens.lens (\NetworkInterfaceAttachment' {instanceId} -> instanceId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceId = a} :: NetworkInterfaceAttachment)

-- | The timestamp indicating when the attachment initiated.
networkInterfaceAttachment_attachTime :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.UTCTime)
networkInterfaceAttachment_attachTime = Lens.lens (\NetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@NetworkInterfaceAttachment' {} a -> s {attachTime = a} :: NetworkInterfaceAttachment) Prelude.. Lens.mapping Data._Time

-- | The device index of the network interface attachment on the instance.
networkInterfaceAttachment_deviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
networkInterfaceAttachment_deviceIndex = Lens.lens (\NetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@NetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: NetworkInterfaceAttachment)

-- | The Amazon Web Services account ID of the owner of the instance.
networkInterfaceAttachment_instanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_instanceOwnerId = Lens.lens (\NetworkInterfaceAttachment' {instanceOwnerId} -> instanceOwnerId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceOwnerId = a} :: NetworkInterfaceAttachment)

instance Data.FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      Prelude.<$> (x Data..@? "networkCardIndex")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "attachmentId")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "attachTime")
      Prelude.<*> (x Data..@? "deviceIndex")
      Prelude.<*> (x Data..@? "instanceOwnerId")

instance Prelude.Hashable NetworkInterfaceAttachment where
  hashWithSalt _salt NetworkInterfaceAttachment' {..} =
    _salt `Prelude.hashWithSalt` networkCardIndex
      `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` attachTime
      `Prelude.hashWithSalt` deviceIndex
      `Prelude.hashWithSalt` instanceOwnerId

instance Prelude.NFData NetworkInterfaceAttachment where
  rnf NetworkInterfaceAttachment' {..} =
    Prelude.rnf networkCardIndex
      `Prelude.seq` Prelude.rnf deleteOnTermination
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf attachmentId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf attachTime
      `Prelude.seq` Prelude.rnf deviceIndex
      `Prelude.seq` Prelude.rnf instanceOwnerId
