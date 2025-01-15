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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AttachmentEnaSrdSpecification
import Amazonka.EC2.Types.AttachmentStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface attachment.
--
-- /See:/ 'newNetworkInterfaceAttachment' smart constructor.
data NetworkInterfaceAttachment = NetworkInterfaceAttachment'
  { -- | The timestamp indicating when the attachment initiated.
    attachTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the network interface attachment.
    attachmentId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The device index of the network interface attachment on the instance.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | Configures ENA Express for the network interface that this action
    -- attaches to the instance.
    enaSrdSpecification :: Prelude.Maybe AttachmentEnaSrdSpecification,
    -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the instance.
    instanceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The attachment state.
    status :: Prelude.Maybe AttachmentStatus
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
-- 'attachTime', 'networkInterfaceAttachment_attachTime' - The timestamp indicating when the attachment initiated.
--
-- 'attachmentId', 'networkInterfaceAttachment_attachmentId' - The ID of the network interface attachment.
--
-- 'deleteOnTermination', 'networkInterfaceAttachment_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'deviceIndex', 'networkInterfaceAttachment_deviceIndex' - The device index of the network interface attachment on the instance.
--
-- 'enaSrdSpecification', 'networkInterfaceAttachment_enaSrdSpecification' - Configures ENA Express for the network interface that this action
-- attaches to the instance.
--
-- 'instanceId', 'networkInterfaceAttachment_instanceId' - The ID of the instance.
--
-- 'instanceOwnerId', 'networkInterfaceAttachment_instanceOwnerId' - The Amazon Web Services account ID of the owner of the instance.
--
-- 'networkCardIndex', 'networkInterfaceAttachment_networkCardIndex' - The index of the network card.
--
-- 'status', 'networkInterfaceAttachment_status' - The attachment state.
newNetworkInterfaceAttachment ::
  NetworkInterfaceAttachment
newNetworkInterfaceAttachment =
  NetworkInterfaceAttachment'
    { attachTime =
        Prelude.Nothing,
      attachmentId = Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      enaSrdSpecification = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceOwnerId = Prelude.Nothing,
      networkCardIndex = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The timestamp indicating when the attachment initiated.
networkInterfaceAttachment_attachTime :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.UTCTime)
networkInterfaceAttachment_attachTime = Lens.lens (\NetworkInterfaceAttachment' {attachTime} -> attachTime) (\s@NetworkInterfaceAttachment' {} a -> s {attachTime = a} :: NetworkInterfaceAttachment) Prelude.. Lens.mapping Data._Time

-- | The ID of the network interface attachment.
networkInterfaceAttachment_attachmentId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_attachmentId = Lens.lens (\NetworkInterfaceAttachment' {attachmentId} -> attachmentId) (\s@NetworkInterfaceAttachment' {} a -> s {attachmentId = a} :: NetworkInterfaceAttachment)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
networkInterfaceAttachment_deleteOnTermination :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Bool)
networkInterfaceAttachment_deleteOnTermination = Lens.lens (\NetworkInterfaceAttachment' {deleteOnTermination} -> deleteOnTermination) (\s@NetworkInterfaceAttachment' {} a -> s {deleteOnTermination = a} :: NetworkInterfaceAttachment)

-- | The device index of the network interface attachment on the instance.
networkInterfaceAttachment_deviceIndex :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
networkInterfaceAttachment_deviceIndex = Lens.lens (\NetworkInterfaceAttachment' {deviceIndex} -> deviceIndex) (\s@NetworkInterfaceAttachment' {} a -> s {deviceIndex = a} :: NetworkInterfaceAttachment)

-- | Configures ENA Express for the network interface that this action
-- attaches to the instance.
networkInterfaceAttachment_enaSrdSpecification :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe AttachmentEnaSrdSpecification)
networkInterfaceAttachment_enaSrdSpecification = Lens.lens (\NetworkInterfaceAttachment' {enaSrdSpecification} -> enaSrdSpecification) (\s@NetworkInterfaceAttachment' {} a -> s {enaSrdSpecification = a} :: NetworkInterfaceAttachment)

-- | The ID of the instance.
networkInterfaceAttachment_instanceId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_instanceId = Lens.lens (\NetworkInterfaceAttachment' {instanceId} -> instanceId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceId = a} :: NetworkInterfaceAttachment)

-- | The Amazon Web Services account ID of the owner of the instance.
networkInterfaceAttachment_instanceOwnerId :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Text)
networkInterfaceAttachment_instanceOwnerId = Lens.lens (\NetworkInterfaceAttachment' {instanceOwnerId} -> instanceOwnerId) (\s@NetworkInterfaceAttachment' {} a -> s {instanceOwnerId = a} :: NetworkInterfaceAttachment)

-- | The index of the network card.
networkInterfaceAttachment_networkCardIndex :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe Prelude.Int)
networkInterfaceAttachment_networkCardIndex = Lens.lens (\NetworkInterfaceAttachment' {networkCardIndex} -> networkCardIndex) (\s@NetworkInterfaceAttachment' {} a -> s {networkCardIndex = a} :: NetworkInterfaceAttachment)

-- | The attachment state.
networkInterfaceAttachment_status :: Lens.Lens' NetworkInterfaceAttachment (Prelude.Maybe AttachmentStatus)
networkInterfaceAttachment_status = Lens.lens (\NetworkInterfaceAttachment' {status} -> status) (\s@NetworkInterfaceAttachment' {} a -> s {status = a} :: NetworkInterfaceAttachment)

instance Data.FromXML NetworkInterfaceAttachment where
  parseXML x =
    NetworkInterfaceAttachment'
      Prelude.<$> (x Data..@? "attachTime")
      Prelude.<*> (x Data..@? "attachmentId")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "deviceIndex")
      Prelude.<*> (x Data..@? "enaSrdSpecification")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "instanceOwnerId")
      Prelude.<*> (x Data..@? "networkCardIndex")
      Prelude.<*> (x Data..@? "status")

instance Prelude.Hashable NetworkInterfaceAttachment where
  hashWithSalt _salt NetworkInterfaceAttachment' {..} =
    _salt
      `Prelude.hashWithSalt` attachTime
      `Prelude.hashWithSalt` attachmentId
      `Prelude.hashWithSalt` deleteOnTermination
      `Prelude.hashWithSalt` deviceIndex
      `Prelude.hashWithSalt` enaSrdSpecification
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceOwnerId
      `Prelude.hashWithSalt` networkCardIndex
      `Prelude.hashWithSalt` status

instance Prelude.NFData NetworkInterfaceAttachment where
  rnf NetworkInterfaceAttachment' {..} =
    Prelude.rnf attachTime `Prelude.seq`
      Prelude.rnf attachmentId `Prelude.seq`
        Prelude.rnf deleteOnTermination `Prelude.seq`
          Prelude.rnf deviceIndex `Prelude.seq`
            Prelude.rnf enaSrdSpecification `Prelude.seq`
              Prelude.rnf instanceId `Prelude.seq`
                Prelude.rnf instanceOwnerId `Prelude.seq`
                  Prelude.rnf networkCardIndex `Prelude.seq`
                    Prelude.rnf status
