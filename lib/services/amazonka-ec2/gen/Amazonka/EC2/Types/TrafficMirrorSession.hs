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
-- Module      : Amazonka.EC2.Types.TrafficMirrorSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Traffic Mirror session.
--
-- /See:/ 'newTrafficMirrorSession' smart constructor.
data TrafficMirrorSession = TrafficMirrorSession'
  { -- | The tags assigned to the Traffic Mirror session.
    tags :: Prelude.Maybe [Tag],
    -- | The ID for the Traffic Mirror session.
    trafficMirrorSessionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that owns the Traffic Mirror session.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The session number determines the order in which sessions are evaluated
    -- when an interface is used by multiple sessions. The first session with a
    -- matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Maybe Prelude.Text,
    -- | The description of the Traffic Mirror session.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes in each packet to mirror. These are the bytes after
    -- the VXLAN header. To mirror a subset, set this to the length (in bytes)
    -- to mirror. For example, if you set this value to 100, then the first 100
    -- bytes that meet the filter criteria are copied to the target. Do not
    -- specify this parameter when you want to mirror the entire packet
    packetLength :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Traffic Mirror session\'s network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The virtual network ID associated with the Traffic Mirror session.
    virtualNetworkId :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficMirrorSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'trafficMirrorSession_tags' - The tags assigned to the Traffic Mirror session.
--
-- 'trafficMirrorSessionId', 'trafficMirrorSession_trafficMirrorSessionId' - The ID for the Traffic Mirror session.
--
-- 'ownerId', 'trafficMirrorSession_ownerId' - The ID of the account that owns the Traffic Mirror session.
--
-- 'sessionNumber', 'trafficMirrorSession_sessionNumber' - The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- 'trafficMirrorTargetId', 'trafficMirrorSession_trafficMirrorTargetId' - The ID of the Traffic Mirror target.
--
-- 'trafficMirrorFilterId', 'trafficMirrorSession_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'description', 'trafficMirrorSession_description' - The description of the Traffic Mirror session.
--
-- 'packetLength', 'trafficMirrorSession_packetLength' - The number of bytes in each packet to mirror. These are the bytes after
-- the VXLAN header. To mirror a subset, set this to the length (in bytes)
-- to mirror. For example, if you set this value to 100, then the first 100
-- bytes that meet the filter criteria are copied to the target. Do not
-- specify this parameter when you want to mirror the entire packet
--
-- 'networkInterfaceId', 'trafficMirrorSession_networkInterfaceId' - The ID of the Traffic Mirror session\'s network interface.
--
-- 'virtualNetworkId', 'trafficMirrorSession_virtualNetworkId' - The virtual network ID associated with the Traffic Mirror session.
newTrafficMirrorSession ::
  TrafficMirrorSession
newTrafficMirrorSession =
  TrafficMirrorSession'
    { tags = Prelude.Nothing,
      trafficMirrorSessionId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      sessionNumber = Prelude.Nothing,
      trafficMirrorTargetId = Prelude.Nothing,
      trafficMirrorFilterId = Prelude.Nothing,
      description = Prelude.Nothing,
      packetLength = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      virtualNetworkId = Prelude.Nothing
    }

-- | The tags assigned to the Traffic Mirror session.
trafficMirrorSession_tags :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe [Tag])
trafficMirrorSession_tags = Lens.lens (\TrafficMirrorSession' {tags} -> tags) (\s@TrafficMirrorSession' {} a -> s {tags = a} :: TrafficMirrorSession) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the Traffic Mirror session.
trafficMirrorSession_trafficMirrorSessionId :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Text)
trafficMirrorSession_trafficMirrorSessionId = Lens.lens (\TrafficMirrorSession' {trafficMirrorSessionId} -> trafficMirrorSessionId) (\s@TrafficMirrorSession' {} a -> s {trafficMirrorSessionId = a} :: TrafficMirrorSession)

-- | The ID of the account that owns the Traffic Mirror session.
trafficMirrorSession_ownerId :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Text)
trafficMirrorSession_ownerId = Lens.lens (\TrafficMirrorSession' {ownerId} -> ownerId) (\s@TrafficMirrorSession' {} a -> s {ownerId = a} :: TrafficMirrorSession)

-- | The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
trafficMirrorSession_sessionNumber :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Int)
trafficMirrorSession_sessionNumber = Lens.lens (\TrafficMirrorSession' {sessionNumber} -> sessionNumber) (\s@TrafficMirrorSession' {} a -> s {sessionNumber = a} :: TrafficMirrorSession)

-- | The ID of the Traffic Mirror target.
trafficMirrorSession_trafficMirrorTargetId :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Text)
trafficMirrorSession_trafficMirrorTargetId = Lens.lens (\TrafficMirrorSession' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@TrafficMirrorSession' {} a -> s {trafficMirrorTargetId = a} :: TrafficMirrorSession)

-- | The ID of the Traffic Mirror filter.
trafficMirrorSession_trafficMirrorFilterId :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Text)
trafficMirrorSession_trafficMirrorFilterId = Lens.lens (\TrafficMirrorSession' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@TrafficMirrorSession' {} a -> s {trafficMirrorFilterId = a} :: TrafficMirrorSession)

-- | The description of the Traffic Mirror session.
trafficMirrorSession_description :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Text)
trafficMirrorSession_description = Lens.lens (\TrafficMirrorSession' {description} -> description) (\s@TrafficMirrorSession' {} a -> s {description = a} :: TrafficMirrorSession)

-- | The number of bytes in each packet to mirror. These are the bytes after
-- the VXLAN header. To mirror a subset, set this to the length (in bytes)
-- to mirror. For example, if you set this value to 100, then the first 100
-- bytes that meet the filter criteria are copied to the target. Do not
-- specify this parameter when you want to mirror the entire packet
trafficMirrorSession_packetLength :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Int)
trafficMirrorSession_packetLength = Lens.lens (\TrafficMirrorSession' {packetLength} -> packetLength) (\s@TrafficMirrorSession' {} a -> s {packetLength = a} :: TrafficMirrorSession)

-- | The ID of the Traffic Mirror session\'s network interface.
trafficMirrorSession_networkInterfaceId :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Text)
trafficMirrorSession_networkInterfaceId = Lens.lens (\TrafficMirrorSession' {networkInterfaceId} -> networkInterfaceId) (\s@TrafficMirrorSession' {} a -> s {networkInterfaceId = a} :: TrafficMirrorSession)

-- | The virtual network ID associated with the Traffic Mirror session.
trafficMirrorSession_virtualNetworkId :: Lens.Lens' TrafficMirrorSession (Prelude.Maybe Prelude.Int)
trafficMirrorSession_virtualNetworkId = Lens.lens (\TrafficMirrorSession' {virtualNetworkId} -> virtualNetworkId) (\s@TrafficMirrorSession' {} a -> s {virtualNetworkId = a} :: TrafficMirrorSession)

instance Core.FromXML TrafficMirrorSession where
  parseXML x =
    TrafficMirrorSession'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "trafficMirrorSessionId")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "sessionNumber")
      Prelude.<*> (x Core..@? "trafficMirrorTargetId")
      Prelude.<*> (x Core..@? "trafficMirrorFilterId")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "packetLength")
      Prelude.<*> (x Core..@? "networkInterfaceId")
      Prelude.<*> (x Core..@? "virtualNetworkId")

instance Prelude.Hashable TrafficMirrorSession where
  hashWithSalt _salt TrafficMirrorSession' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trafficMirrorSessionId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` sessionNumber
      `Prelude.hashWithSalt` trafficMirrorTargetId
      `Prelude.hashWithSalt` trafficMirrorFilterId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` packetLength
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` virtualNetworkId

instance Prelude.NFData TrafficMirrorSession where
  rnf TrafficMirrorSession' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trafficMirrorSessionId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf sessionNumber
      `Prelude.seq` Prelude.rnf trafficMirrorTargetId
      `Prelude.seq` Prelude.rnf trafficMirrorFilterId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf packetLength
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf virtualNetworkId
