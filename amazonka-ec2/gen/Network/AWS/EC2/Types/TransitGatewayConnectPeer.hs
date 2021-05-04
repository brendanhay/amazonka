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
-- Module      : Network.AWS.EC2.Types.TransitGatewayConnectPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayConnectPeer where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TransitGatewayConnectPeerConfiguration
import Network.AWS.EC2.Types.TransitGatewayConnectPeerState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a transit gateway Connect peer.
--
-- /See:/ 'newTransitGatewayConnectPeer' smart constructor.
data TransitGatewayConnectPeer = TransitGatewayConnectPeer'
  { -- | The Connect peer details.
    connectPeerConfiguration :: Prelude.Maybe TransitGatewayConnectPeerConfiguration,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The ID of the Connect peer.
    transitGatewayConnectPeerId :: Prelude.Maybe Prelude.Text,
    -- | The state of the Connect peer.
    state :: Prelude.Maybe TransitGatewayConnectPeerState,
    -- | The tags for the Connect peer.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the Connect attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      transitGatewayConnectPeerId = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing
    }

-- | The Connect peer details.
transitGatewayConnectPeer_connectPeerConfiguration :: Lens.Lens' TransitGatewayConnectPeer (Prelude.Maybe TransitGatewayConnectPeerConfiguration)
transitGatewayConnectPeer_connectPeerConfiguration = Lens.lens (\TransitGatewayConnectPeer' {connectPeerConfiguration} -> connectPeerConfiguration) (\s@TransitGatewayConnectPeer' {} a -> s {connectPeerConfiguration = a} :: TransitGatewayConnectPeer)

-- | The creation time.
transitGatewayConnectPeer_creationTime :: Lens.Lens' TransitGatewayConnectPeer (Prelude.Maybe Prelude.UTCTime)
transitGatewayConnectPeer_creationTime = Lens.lens (\TransitGatewayConnectPeer' {creationTime} -> creationTime) (\s@TransitGatewayConnectPeer' {} a -> s {creationTime = a} :: TransitGatewayConnectPeer) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the Connect peer.
transitGatewayConnectPeer_transitGatewayConnectPeerId :: Lens.Lens' TransitGatewayConnectPeer (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeer_transitGatewayConnectPeerId = Lens.lens (\TransitGatewayConnectPeer' {transitGatewayConnectPeerId} -> transitGatewayConnectPeerId) (\s@TransitGatewayConnectPeer' {} a -> s {transitGatewayConnectPeerId = a} :: TransitGatewayConnectPeer)

-- | The state of the Connect peer.
transitGatewayConnectPeer_state :: Lens.Lens' TransitGatewayConnectPeer (Prelude.Maybe TransitGatewayConnectPeerState)
transitGatewayConnectPeer_state = Lens.lens (\TransitGatewayConnectPeer' {state} -> state) (\s@TransitGatewayConnectPeer' {} a -> s {state = a} :: TransitGatewayConnectPeer)

-- | The tags for the Connect peer.
transitGatewayConnectPeer_tags :: Lens.Lens' TransitGatewayConnectPeer (Prelude.Maybe [Tag])
transitGatewayConnectPeer_tags = Lens.lens (\TransitGatewayConnectPeer' {tags} -> tags) (\s@TransitGatewayConnectPeer' {} a -> s {tags = a} :: TransitGatewayConnectPeer) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the Connect attachment.
transitGatewayConnectPeer_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayConnectPeer (Prelude.Maybe Prelude.Text)
transitGatewayConnectPeer_transitGatewayAttachmentId = Lens.lens (\TransitGatewayConnectPeer' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayConnectPeer' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayConnectPeer)

instance Prelude.FromXML TransitGatewayConnectPeer where
  parseXML x =
    TransitGatewayConnectPeer'
      Prelude.<$> (x Prelude..@? "connectPeerConfiguration")
      Prelude.<*> (x Prelude..@? "creationTime")
      Prelude.<*> (x Prelude..@? "transitGatewayConnectPeerId")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "transitGatewayAttachmentId")

instance Prelude.Hashable TransitGatewayConnectPeer

instance Prelude.NFData TransitGatewayConnectPeer
