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
-- Module      : Network.AWS.EC2.Types.TransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayPeeringAttachment where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PeeringAttachmentStatus
import Network.AWS.EC2.Types.PeeringTgwInfo
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayAttachmentState
import qualified Network.AWS.Lens as Lens

-- | Describes the transit gateway peering attachment.
--
-- /See:/ 'newTransitGatewayPeeringAttachment' smart constructor.
data TransitGatewayPeeringAttachment = TransitGatewayPeeringAttachment'
  { -- | The status of the transit gateway peering attachment.
    status :: Core.Maybe PeeringAttachmentStatus,
    -- | The time the transit gateway peering attachment was created.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | Information about the requester transit gateway.
    requesterTgwInfo :: Core.Maybe PeeringTgwInfo,
    -- | Information about the accepter transit gateway.
    accepterTgwInfo :: Core.Maybe PeeringTgwInfo,
    -- | The state of the transit gateway peering attachment. Note that the
    -- @initiating@ state has been deprecated.
    state :: Core.Maybe TransitGatewayAttachmentState,
    -- | The tags for the transit gateway peering attachment.
    tags :: Core.Maybe [Tag],
    -- | The ID of the transit gateway peering attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayPeeringAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'transitGatewayPeeringAttachment_status' - The status of the transit gateway peering attachment.
--
-- 'creationTime', 'transitGatewayPeeringAttachment_creationTime' - The time the transit gateway peering attachment was created.
--
-- 'requesterTgwInfo', 'transitGatewayPeeringAttachment_requesterTgwInfo' - Information about the requester transit gateway.
--
-- 'accepterTgwInfo', 'transitGatewayPeeringAttachment_accepterTgwInfo' - Information about the accepter transit gateway.
--
-- 'state', 'transitGatewayPeeringAttachment_state' - The state of the transit gateway peering attachment. Note that the
-- @initiating@ state has been deprecated.
--
-- 'tags', 'transitGatewayPeeringAttachment_tags' - The tags for the transit gateway peering attachment.
--
-- 'transitGatewayAttachmentId', 'transitGatewayPeeringAttachment_transitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
newTransitGatewayPeeringAttachment ::
  TransitGatewayPeeringAttachment
newTransitGatewayPeeringAttachment =
  TransitGatewayPeeringAttachment'
    { status =
        Core.Nothing,
      creationTime = Core.Nothing,
      requesterTgwInfo = Core.Nothing,
      accepterTgwInfo = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing
    }

-- | The status of the transit gateway peering attachment.
transitGatewayPeeringAttachment_status :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe PeeringAttachmentStatus)
transitGatewayPeeringAttachment_status = Lens.lens (\TransitGatewayPeeringAttachment' {status} -> status) (\s@TransitGatewayPeeringAttachment' {} a -> s {status = a} :: TransitGatewayPeeringAttachment)

-- | The time the transit gateway peering attachment was created.
transitGatewayPeeringAttachment_creationTime :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Core.UTCTime)
transitGatewayPeeringAttachment_creationTime = Lens.lens (\TransitGatewayPeeringAttachment' {creationTime} -> creationTime) (\s@TransitGatewayPeeringAttachment' {} a -> s {creationTime = a} :: TransitGatewayPeeringAttachment) Core.. Lens.mapping Core._Time

-- | Information about the requester transit gateway.
transitGatewayPeeringAttachment_requesterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe PeeringTgwInfo)
transitGatewayPeeringAttachment_requesterTgwInfo = Lens.lens (\TransitGatewayPeeringAttachment' {requesterTgwInfo} -> requesterTgwInfo) (\s@TransitGatewayPeeringAttachment' {} a -> s {requesterTgwInfo = a} :: TransitGatewayPeeringAttachment)

-- | Information about the accepter transit gateway.
transitGatewayPeeringAttachment_accepterTgwInfo :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe PeeringTgwInfo)
transitGatewayPeeringAttachment_accepterTgwInfo = Lens.lens (\TransitGatewayPeeringAttachment' {accepterTgwInfo} -> accepterTgwInfo) (\s@TransitGatewayPeeringAttachment' {} a -> s {accepterTgwInfo = a} :: TransitGatewayPeeringAttachment)

-- | The state of the transit gateway peering attachment. Note that the
-- @initiating@ state has been deprecated.
transitGatewayPeeringAttachment_state :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe TransitGatewayAttachmentState)
transitGatewayPeeringAttachment_state = Lens.lens (\TransitGatewayPeeringAttachment' {state} -> state) (\s@TransitGatewayPeeringAttachment' {} a -> s {state = a} :: TransitGatewayPeeringAttachment)

-- | The tags for the transit gateway peering attachment.
transitGatewayPeeringAttachment_tags :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe [Tag])
transitGatewayPeeringAttachment_tags = Lens.lens (\TransitGatewayPeeringAttachment' {tags} -> tags) (\s@TransitGatewayPeeringAttachment' {} a -> s {tags = a} :: TransitGatewayPeeringAttachment) Core.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway peering attachment.
transitGatewayPeeringAttachment_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayPeeringAttachment (Core.Maybe Core.Text)
transitGatewayPeeringAttachment_transitGatewayAttachmentId = Lens.lens (\TransitGatewayPeeringAttachment' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayPeeringAttachment' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayPeeringAttachment)

instance Core.FromXML TransitGatewayPeeringAttachment where
  parseXML x =
    TransitGatewayPeeringAttachment'
      Core.<$> (x Core..@? "status")
      Core.<*> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "requesterTgwInfo")
      Core.<*> (x Core..@? "accepterTgwInfo")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "transitGatewayAttachmentId")

instance
  Core.Hashable
    TransitGatewayPeeringAttachment

instance Core.NFData TransitGatewayPeeringAttachment
