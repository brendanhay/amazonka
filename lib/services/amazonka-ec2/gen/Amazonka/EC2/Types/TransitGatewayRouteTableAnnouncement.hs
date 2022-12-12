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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncementDirection
import Amazonka.EC2.Types.TransitGatewayRouteTableAnnouncementState
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway route table announcement.
--
-- /See:/ 'newTransitGatewayRouteTableAnnouncement' smart constructor.
data TransitGatewayRouteTableAnnouncement = TransitGatewayRouteTableAnnouncement'
  { -- | The direction for the route table announcement.
    announcementDirection :: Prelude.Maybe TransitGatewayRouteTableAnnouncementDirection,
    -- | The ID of the core network for the transit gateway route table
    -- announcement.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the transit gateway route table announcement was
    -- created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the core network ID for the peer.
    peerCoreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the peer transit gateway.
    peerTransitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the peering attachment.
    peeringAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The state of the transit gateway announcement.
    state :: Prelude.Maybe TransitGatewayRouteTableAnnouncementState,
    -- | The key-value pairs associated with the route table announcement.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table announcement.
    transitGatewayRouteTableAnnouncementId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTableAnnouncement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'announcementDirection', 'transitGatewayRouteTableAnnouncement_announcementDirection' - The direction for the route table announcement.
--
-- 'coreNetworkId', 'transitGatewayRouteTableAnnouncement_coreNetworkId' - The ID of the core network for the transit gateway route table
-- announcement.
--
-- 'creationTime', 'transitGatewayRouteTableAnnouncement_creationTime' - The timestamp when the transit gateway route table announcement was
-- created.
--
-- 'peerCoreNetworkId', 'transitGatewayRouteTableAnnouncement_peerCoreNetworkId' - The ID of the core network ID for the peer.
--
-- 'peerTransitGatewayId', 'transitGatewayRouteTableAnnouncement_peerTransitGatewayId' - The ID of the peer transit gateway.
--
-- 'peeringAttachmentId', 'transitGatewayRouteTableAnnouncement_peeringAttachmentId' - The ID of the peering attachment.
--
-- 'state', 'transitGatewayRouteTableAnnouncement_state' - The state of the transit gateway announcement.
--
-- 'tags', 'transitGatewayRouteTableAnnouncement_tags' - The key-value pairs associated with the route table announcement.
--
-- 'transitGatewayId', 'transitGatewayRouteTableAnnouncement_transitGatewayId' - The ID of the transit gateway.
--
-- 'transitGatewayRouteTableAnnouncementId', 'transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId' - The ID of the transit gateway route table announcement.
--
-- 'transitGatewayRouteTableId', 'transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newTransitGatewayRouteTableAnnouncement ::
  TransitGatewayRouteTableAnnouncement
newTransitGatewayRouteTableAnnouncement =
  TransitGatewayRouteTableAnnouncement'
    { announcementDirection =
        Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      peerCoreNetworkId = Prelude.Nothing,
      peerTransitGatewayId =
        Prelude.Nothing,
      peeringAttachmentId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      transitGatewayRouteTableAnnouncementId =
        Prelude.Nothing,
      transitGatewayRouteTableId =
        Prelude.Nothing
    }

-- | The direction for the route table announcement.
transitGatewayRouteTableAnnouncement_announcementDirection :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe TransitGatewayRouteTableAnnouncementDirection)
transitGatewayRouteTableAnnouncement_announcementDirection = Lens.lens (\TransitGatewayRouteTableAnnouncement' {announcementDirection} -> announcementDirection) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {announcementDirection = a} :: TransitGatewayRouteTableAnnouncement)

-- | The ID of the core network for the transit gateway route table
-- announcement.
transitGatewayRouteTableAnnouncement_coreNetworkId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_coreNetworkId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {coreNetworkId} -> coreNetworkId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {coreNetworkId = a} :: TransitGatewayRouteTableAnnouncement)

-- | The timestamp when the transit gateway route table announcement was
-- created.
transitGatewayRouteTableAnnouncement_creationTime :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.UTCTime)
transitGatewayRouteTableAnnouncement_creationTime = Lens.lens (\TransitGatewayRouteTableAnnouncement' {creationTime} -> creationTime) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {creationTime = a} :: TransitGatewayRouteTableAnnouncement) Prelude.. Lens.mapping Data._Time

-- | The ID of the core network ID for the peer.
transitGatewayRouteTableAnnouncement_peerCoreNetworkId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_peerCoreNetworkId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {peerCoreNetworkId} -> peerCoreNetworkId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {peerCoreNetworkId = a} :: TransitGatewayRouteTableAnnouncement)

-- | The ID of the peer transit gateway.
transitGatewayRouteTableAnnouncement_peerTransitGatewayId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_peerTransitGatewayId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {peerTransitGatewayId} -> peerTransitGatewayId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {peerTransitGatewayId = a} :: TransitGatewayRouteTableAnnouncement)

-- | The ID of the peering attachment.
transitGatewayRouteTableAnnouncement_peeringAttachmentId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_peeringAttachmentId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {peeringAttachmentId} -> peeringAttachmentId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {peeringAttachmentId = a} :: TransitGatewayRouteTableAnnouncement)

-- | The state of the transit gateway announcement.
transitGatewayRouteTableAnnouncement_state :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe TransitGatewayRouteTableAnnouncementState)
transitGatewayRouteTableAnnouncement_state = Lens.lens (\TransitGatewayRouteTableAnnouncement' {state} -> state) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {state = a} :: TransitGatewayRouteTableAnnouncement)

-- | The key-value pairs associated with the route table announcement.
transitGatewayRouteTableAnnouncement_tags :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe [Tag])
transitGatewayRouteTableAnnouncement_tags = Lens.lens (\TransitGatewayRouteTableAnnouncement' {tags} -> tags) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {tags = a} :: TransitGatewayRouteTableAnnouncement) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway.
transitGatewayRouteTableAnnouncement_transitGatewayId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_transitGatewayId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {transitGatewayId = a} :: TransitGatewayRouteTableAnnouncement)

-- | The ID of the transit gateway route table announcement.
transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_transitGatewayRouteTableAnnouncementId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {transitGatewayRouteTableAnnouncementId} -> transitGatewayRouteTableAnnouncementId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {transitGatewayRouteTableAnnouncementId = a} :: TransitGatewayRouteTableAnnouncement)

-- | The ID of the transit gateway route table.
transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTableAnnouncement (Prelude.Maybe Prelude.Text)
transitGatewayRouteTableAnnouncement_transitGatewayRouteTableId = Lens.lens (\TransitGatewayRouteTableAnnouncement' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayRouteTableAnnouncement' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayRouteTableAnnouncement)

instance
  Data.FromXML
    TransitGatewayRouteTableAnnouncement
  where
  parseXML x =
    TransitGatewayRouteTableAnnouncement'
      Prelude.<$> (x Data..@? "announcementDirection")
      Prelude.<*> (x Data..@? "coreNetworkId")
      Prelude.<*> (x Data..@? "creationTime")
      Prelude.<*> (x Data..@? "peerCoreNetworkId")
      Prelude.<*> (x Data..@? "peerTransitGatewayId")
      Prelude.<*> (x Data..@? "peeringAttachmentId")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "transitGatewayId")
      Prelude.<*> (x Data..@? "transitGatewayRouteTableAnnouncementId")
      Prelude.<*> (x Data..@? "transitGatewayRouteTableId")

instance
  Prelude.Hashable
    TransitGatewayRouteTableAnnouncement
  where
  hashWithSalt
    _salt
    TransitGatewayRouteTableAnnouncement' {..} =
      _salt `Prelude.hashWithSalt` announcementDirection
        `Prelude.hashWithSalt` coreNetworkId
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` peerCoreNetworkId
        `Prelude.hashWithSalt` peerTransitGatewayId
        `Prelude.hashWithSalt` peeringAttachmentId
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` transitGatewayId
        `Prelude.hashWithSalt` transitGatewayRouteTableAnnouncementId
        `Prelude.hashWithSalt` transitGatewayRouteTableId

instance
  Prelude.NFData
    TransitGatewayRouteTableAnnouncement
  where
  rnf TransitGatewayRouteTableAnnouncement' {..} =
    Prelude.rnf announcementDirection
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf peerCoreNetworkId
      `Prelude.seq` Prelude.rnf peerTransitGatewayId
      `Prelude.seq` Prelude.rnf peeringAttachmentId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableAnnouncementId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
