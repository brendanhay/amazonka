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
-- Module      : Amazonka.EC2.Types.TransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayPeeringAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PeeringAttachmentStatus
import Amazonka.EC2.Types.PeeringTgwInfo
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayAttachmentState
import Amazonka.EC2.Types.TransitGatewayPeeringAttachmentOptions
import qualified Amazonka.Prelude as Prelude

-- | Describes the transit gateway peering attachment.
--
-- /See:/ 'newTransitGatewayPeeringAttachment' smart constructor.
data TransitGatewayPeeringAttachment = TransitGatewayPeeringAttachment'
  { -- | Information about the accepter transit gateway.
    accepterTgwInfo :: Prelude.Maybe PeeringTgwInfo,
    -- | The ID of the accepter transit gateway attachment.
    accepterTransitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The time the transit gateway peering attachment was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | Details about the transit gateway peering attachment.
    options :: Prelude.Maybe TransitGatewayPeeringAttachmentOptions,
    -- | Information about the requester transit gateway.
    requesterTgwInfo :: Prelude.Maybe PeeringTgwInfo,
    -- | The state of the transit gateway peering attachment. Note that the
    -- @initiating@ state has been deprecated.
    state :: Prelude.Maybe TransitGatewayAttachmentState,
    -- | The status of the transit gateway peering attachment.
    status :: Prelude.Maybe PeeringAttachmentStatus,
    -- | The tags for the transit gateway peering attachment.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the transit gateway peering attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accepterTgwInfo', 'transitGatewayPeeringAttachment_accepterTgwInfo' - Information about the accepter transit gateway.
--
-- 'accepterTransitGatewayAttachmentId', 'transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId' - The ID of the accepter transit gateway attachment.
--
-- 'creationTime', 'transitGatewayPeeringAttachment_creationTime' - The time the transit gateway peering attachment was created.
--
-- 'options', 'transitGatewayPeeringAttachment_options' - Details about the transit gateway peering attachment.
--
-- 'requesterTgwInfo', 'transitGatewayPeeringAttachment_requesterTgwInfo' - Information about the requester transit gateway.
--
-- 'state', 'transitGatewayPeeringAttachment_state' - The state of the transit gateway peering attachment. Note that the
-- @initiating@ state has been deprecated.
--
-- 'status', 'transitGatewayPeeringAttachment_status' - The status of the transit gateway peering attachment.
--
-- 'tags', 'transitGatewayPeeringAttachment_tags' - The tags for the transit gateway peering attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayPeeringAttachment_transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
newTransitGatewayPeeringAttachment ::
  TransitGatewayPeeringAttachment
newTransitGatewayPeeringAttachment =
  TransitGatewayPeeringAttachment'
    { accepterTgwInfo =
        Prelude.Nothing,
      accepterTransitGatewayAttachmentId =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      options = Prelude.Nothing,
      requesterTgwInfo = Prelude.Nothing,
      state = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayAttachmentId =
        Prelude.Nothing
    }

-- | Information about the accepter transit gateway.
transitGatewayPeeringAttachment_accepterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe PeeringTgwInfo)
transitGatewayPeeringAttachment_accepterTgwInfo = Lens.lens (\TransitGatewayPeeringAttachment' {accepterTgwInfo} -> accepterTgwInfo) (\s@TransitGatewayPeeringAttachment' {} a -> s {accepterTgwInfo = a} :: TransitGatewayPeeringAttachment)

-- | The ID of the accepter transit gateway attachment.
transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe Prelude.Text)
transitGatewayPeeringAttachment_accepterTransitGatewayAttachmentId = Lens.lens (\TransitGatewayPeeringAttachment' {accepterTransitGatewayAttachmentId} -> accepterTransitGatewayAttachmentId) (\s@TransitGatewayPeeringAttachment' {} a -> s {accepterTransitGatewayAttachmentId = a} :: TransitGatewayPeeringAttachment)

-- | The time the transit gateway peering attachment was created.
transitGatewayPeeringAttachment_creationTime :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe Prelude.UTCTime)
transitGatewayPeeringAttachment_creationTime = Lens.lens (\TransitGatewayPeeringAttachment' {creationTime} -> creationTime) (\s@TransitGatewayPeeringAttachment' {} a -> s {creationTime = a} :: TransitGatewayPeeringAttachment) Prelude.. Lens.mapping Data._Time

-- | Details about the transit gateway peering attachment.
transitGatewayPeeringAttachment_options :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe TransitGatewayPeeringAttachmentOptions)
transitGatewayPeeringAttachment_options = Lens.lens (\TransitGatewayPeeringAttachment' {options} -> options) (\s@TransitGatewayPeeringAttachment' {} a -> s {options = a} :: TransitGatewayPeeringAttachment)

-- | Information about the requester transit gateway.
transitGatewayPeeringAttachment_requesterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe PeeringTgwInfo)
transitGatewayPeeringAttachment_requesterTgwInfo = Lens.lens (\TransitGatewayPeeringAttachment' {requesterTgwInfo} -> requesterTgwInfo) (\s@TransitGatewayPeeringAttachment' {} a -> s {requesterTgwInfo = a} :: TransitGatewayPeeringAttachment)

-- | The state of the transit gateway peering attachment. Note that the
-- @initiating@ state has been deprecated.
transitGatewayPeeringAttachment_state :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe TransitGatewayAttachmentState)
transitGatewayPeeringAttachment_state = Lens.lens (\TransitGatewayPeeringAttachment' {state} -> state) (\s@TransitGatewayPeeringAttachment' {} a -> s {state = a} :: TransitGatewayPeeringAttachment)

-- | The status of the transit gateway peering attachment.
transitGatewayPeeringAttachment_status :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe PeeringAttachmentStatus)
transitGatewayPeeringAttachment_status = Lens.lens (\TransitGatewayPeeringAttachment' {status} -> status) (\s@TransitGatewayPeeringAttachment' {} a -> s {status = a} :: TransitGatewayPeeringAttachment)

-- | The tags for the transit gateway peering attachment.
transitGatewayPeeringAttachment_tags :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe [Tag])
transitGatewayPeeringAttachment_tags = Lens.lens (\TransitGatewayPeeringAttachment' {tags} -> tags) (\s@TransitGatewayPeeringAttachment' {} a -> s {tags = a} :: TransitGatewayPeeringAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway peering attachment.
transitGatewayPeeringAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayPeeringAttachment (Prelude.Maybe Prelude.Text)
transitGatewayPeeringAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayPeeringAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayPeeringAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPeeringAttachment)

instance Data.FromXML TransitGatewayPeeringAttachment where
  parseXML x =
    TransitGatewayPeeringAttachment'
      Prelude.<$> (x Data..@? "accepterTgwInfo")
      Prelude.<*> (x Data..@? "accepterTransitGatewayAttachmentId")
      Prelude.<*> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "options")
      Prelude.<*> (x Data..@? "requesterTgwInfo")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transitGatewayAttachmentId")

instance
  Prelude.Hashable
    TransitGatewayPeeringAttachment
  where
  hashWithSalt
    _salt
    TransitGatewayPeeringAttachment' {..} =
      _salt `Prelude.hashWithSalt` accepterTgwInfo
        `Prelude.hashWithSalt` accepterTransitGatewayAttachmentId
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` requesterTgwInfo
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` transitGatewayAttachmentId

instance
  Prelude.NFData
    TransitGatewayPeeringAttachment
  where
  rnf TransitGatewayPeeringAttachment' {..} =
    Prelude.rnf accepterTgwInfo
      `Prelude.seq` Prelude.rnf accepterTransitGatewayAttachmentId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf requesterTgwInfo
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayAttachmentId
