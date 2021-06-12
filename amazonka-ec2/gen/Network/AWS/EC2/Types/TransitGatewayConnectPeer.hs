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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectPeer where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration
import Network.AWS.EC2.Types.TransitGatewayConnectPeerState
import qualified Network.AWS.Lens as Lens

-- | Describes a transit gateway Connect peer.
--
-- /See:/ 'newTransitGatewayConnectPeer' smart constructor.
data TransitGatewayConnectPeer = TransitGatewayConnectPeer'
  { -- | The Connect peer details.
    connectPeerConfiguration :: Core.Maybe TransitGatewayConnectPeerConfiguration,
    -- | The creation time.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the Connect peer.
    transitGatewayConnectPeerId :: Core.Maybe Core.Text,
    -- | The state of the Connect peer.
    state :: Core.Maybe TransitGatewayConnectPeerState,
    -- | The tags for the Connect peer.
    tags :: Core.Maybe [Tag],
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayConnectPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectPeerConfiguration', 'transitGatewayConnectPeer_connectPeerConfiguration' - The Connect peer details.
--
-- 'creationTime', 'transitGatewayConnectPeer_creationTime' - The creation time.
--
-- 'transitGatewayConnectPeerId', 'transitGatewayConnectPeer_transitGatewayConnectPeerId' - The ID of the Connect peer.
--
-- 'state', 'transitGatewayConnectPeer_state' - The state of the Connect peer.
--
-- 'tags', 'transitGatewayConnectPeer_tags' - The tags for the Connect peer.
--
-- 'transitGatewayAttachmentId', 'transitGatewayConnectPeer_transitGatewayAttachmentId' - The ID of the Connect attachment.
newTransitGatewayConnectPeer ::
  TransitGatewayConnectPeer
newTransitGatewayConnectPeer =
  TransitGatewayConnectPeer'
    { connectPeerConfiguration =
        Core.Nothing,
      creationTime = Core.Nothing,
      transitGatewayConnectPeerId = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing
    }

-- | The Connect peer details.
transitGatewayConnectPeer_connectPeerConfiguration :: Lens.Lens' TransitGatewayConnectPeer (Core.Maybe TransitGatewayConnectPeerConfiguration)
transitGatewayConnectPeer_connectPeerConfiguration = Lens.lens (\TransitGatewayConnectPeer' {connectPeerConfiguration} -> connectPeerConfiguration) (\s@TransitGatewayConnectPeer' {} a -> s {connectPeerConfiguration = a} :: TransitGatewayConnectPeer)

-- | The creation time.
transitGatewayConnectPeer_creationTime :: Lens.Lens' TransitGatewayConnectPeer (Core.Maybe Core.UTCTime)
transitGatewayConnectPeer_creationTime = Lens.lens (\TransitGatewayConnectPeer' {creationTime} -> creationTime) (\s@TransitGatewayConnectPeer' {} a -> s {creationTime = a} :: TransitGatewayConnectPeer) Core.. Lens.mapping Core._Time

-- | The ID of the Connect peer.
transitGatewayConnectPeer_transitGatewayConnectPeerId :: Lens.Lens' TransitGatewayConnectPeer (Core.Maybe Core.Text)
transitGatewayConnectPeer_transitGatewayConnectPeerId = Lens.lens (\TransitGatewayConnectPeer' {transitGatewayConnectPeerId} -> transitGatewayConnectPeerId) (\s@TransitGatewayConnectPeer' {} a -> s {transitGatewayConnectPeerId = a} :: TransitGatewayConnectPeer)

-- | The state of the Connect peer.
transitGatewayConnectPeer_state :: Lens.Lens' TransitGatewayConnectPeer (Core.Maybe TransitGatewayConnectPeerState)
transitGatewayConnectPeer_state = Lens.lens (\TransitGatewayConnectPeer' {state} -> state) (\s@TransitGatewayConnectPeer' {} a -> s {state = a} :: TransitGatewayConnectPeer)

-- | The tags for the Connect peer.
transitGatewayConnectPeer_tags :: Lens.Lens' TransitGatewayConnectPeer (Core.Maybe [Tag])
transitGatewayConnectPeer_tags = Lens.lens (\TransitGatewayConnectPeer' {tags} -> tags) (\s@TransitGatewayConnectPeer' {} a -> s {tags = a} :: TransitGatewayConnectPeer) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Connect attachment.
transitGatewayConnectPeer_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayConnectPeer (Core.Maybe Core.Text)
transitGatewayConnectPeer_transitGatewayAttachmentId = Lens.lens (\TransitGatewayConnectPeer' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayConnectPeer' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayConnectPeer)

instance Core.FromXML TransitGatewayConnectPeer where
  parseXML x =
    TransitGatewayConnectPeer'
      Core.<$> (x Core..@? "connectPeerConfiguration")
      Core.<*> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "transitGatewayConnectPeerId")
      Core.<*> (x Core..@? "state")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "transitGatewayAttachmentId")

instance Core.Hashable TransitGatewayConnectPeer

instance Core.NFData TransitGatewayConnectPeer
