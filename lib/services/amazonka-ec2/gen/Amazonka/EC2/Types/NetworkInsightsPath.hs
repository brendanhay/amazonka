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
-- Module      : Amazonka.EC2.Types.NetworkInsightsPath
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInsightsPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Protocol
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a path.
--
-- /See:/ 'newNetworkInsightsPath' smart constructor.
data NetworkInsightsPath = NetworkInsightsPath'
  { -- | The tags associated with the path.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Web Services resource that is the destination of the path.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the Amazon Web Services resource that is the
    -- destination of the path.
    destinationIp :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the Amazon Web Services resource that is the source of
    -- the path.
    sourceIp :: Prelude.Maybe Prelude.Text,
    -- | The destination port.
    destinationPort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the path.
    networkInsightsPathArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services resource that is the source of the path.
    source :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the path was created.
    createdDate :: Prelude.Maybe Core.ISO8601,
    -- | The protocol.
    protocol :: Prelude.Maybe Protocol,
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInsightsPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'networkInsightsPath_tags' - The tags associated with the path.
--
-- 'destination', 'networkInsightsPath_destination' - The Amazon Web Services resource that is the destination of the path.
--
-- 'destinationIp', 'networkInsightsPath_destinationIp' - The IP address of the Amazon Web Services resource that is the
-- destination of the path.
--
-- 'sourceIp', 'networkInsightsPath_sourceIp' - The IP address of the Amazon Web Services resource that is the source of
-- the path.
--
-- 'destinationPort', 'networkInsightsPath_destinationPort' - The destination port.
--
-- 'networkInsightsPathArn', 'networkInsightsPath_networkInsightsPathArn' - The Amazon Resource Name (ARN) of the path.
--
-- 'source', 'networkInsightsPath_source' - The Amazon Web Services resource that is the source of the path.
--
-- 'createdDate', 'networkInsightsPath_createdDate' - The time stamp when the path was created.
--
-- 'protocol', 'networkInsightsPath_protocol' - The protocol.
--
-- 'networkInsightsPathId', 'networkInsightsPath_networkInsightsPathId' - The ID of the path.
newNetworkInsightsPath ::
  NetworkInsightsPath
newNetworkInsightsPath =
  NetworkInsightsPath'
    { tags = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationIp = Prelude.Nothing,
      sourceIp = Prelude.Nothing,
      destinationPort = Prelude.Nothing,
      networkInsightsPathArn = Prelude.Nothing,
      source = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      protocol = Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing
    }

-- | The tags associated with the path.
networkInsightsPath_tags :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe [Tag])
networkInsightsPath_tags = Lens.lens (\NetworkInsightsPath' {tags} -> tags) (\s@NetworkInsightsPath' {} a -> s {tags = a} :: NetworkInsightsPath) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services resource that is the destination of the path.
networkInsightsPath_destination :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destination = Lens.lens (\NetworkInsightsPath' {destination} -> destination) (\s@NetworkInsightsPath' {} a -> s {destination = a} :: NetworkInsightsPath)

-- | The IP address of the Amazon Web Services resource that is the
-- destination of the path.
networkInsightsPath_destinationIp :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destinationIp = Lens.lens (\NetworkInsightsPath' {destinationIp} -> destinationIp) (\s@NetworkInsightsPath' {} a -> s {destinationIp = a} :: NetworkInsightsPath)

-- | The IP address of the Amazon Web Services resource that is the source of
-- the path.
networkInsightsPath_sourceIp :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_sourceIp = Lens.lens (\NetworkInsightsPath' {sourceIp} -> sourceIp) (\s@NetworkInsightsPath' {} a -> s {sourceIp = a} :: NetworkInsightsPath)

-- | The destination port.
networkInsightsPath_destinationPort :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Int)
networkInsightsPath_destinationPort = Lens.lens (\NetworkInsightsPath' {destinationPort} -> destinationPort) (\s@NetworkInsightsPath' {} a -> s {destinationPort = a} :: NetworkInsightsPath)

-- | The Amazon Resource Name (ARN) of the path.
networkInsightsPath_networkInsightsPathArn :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_networkInsightsPathArn = Lens.lens (\NetworkInsightsPath' {networkInsightsPathArn} -> networkInsightsPathArn) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathArn = a} :: NetworkInsightsPath)

-- | The Amazon Web Services resource that is the source of the path.
networkInsightsPath_source :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_source = Lens.lens (\NetworkInsightsPath' {source} -> source) (\s@NetworkInsightsPath' {} a -> s {source = a} :: NetworkInsightsPath)

-- | The time stamp when the path was created.
networkInsightsPath_createdDate :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.UTCTime)
networkInsightsPath_createdDate = Lens.lens (\NetworkInsightsPath' {createdDate} -> createdDate) (\s@NetworkInsightsPath' {} a -> s {createdDate = a} :: NetworkInsightsPath) Prelude.. Lens.mapping Core._Time

-- | The protocol.
networkInsightsPath_protocol :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Protocol)
networkInsightsPath_protocol = Lens.lens (\NetworkInsightsPath' {protocol} -> protocol) (\s@NetworkInsightsPath' {} a -> s {protocol = a} :: NetworkInsightsPath)

-- | The ID of the path.
networkInsightsPath_networkInsightsPathId :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_networkInsightsPathId = Lens.lens (\NetworkInsightsPath' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsPath)

instance Core.FromXML NetworkInsightsPath where
  parseXML x =
    NetworkInsightsPath'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "destination")
      Prelude.<*> (x Core..@? "destinationIp")
      Prelude.<*> (x Core..@? "sourceIp")
      Prelude.<*> (x Core..@? "destinationPort")
      Prelude.<*> (x Core..@? "networkInsightsPathArn")
      Prelude.<*> (x Core..@? "source")
      Prelude.<*> (x Core..@? "createdDate")
      Prelude.<*> (x Core..@? "protocol")
      Prelude.<*> (x Core..@? "networkInsightsPathId")

instance Prelude.Hashable NetworkInsightsPath where
  hashWithSalt _salt NetworkInsightsPath' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationIp
      `Prelude.hashWithSalt` sourceIp
      `Prelude.hashWithSalt` destinationPort
      `Prelude.hashWithSalt` networkInsightsPathArn
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` networkInsightsPathId

instance Prelude.NFData NetworkInsightsPath where
  rnf NetworkInsightsPath' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationIp
      `Prelude.seq` Prelude.rnf sourceIp
      `Prelude.seq` Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf networkInsightsPathArn
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf networkInsightsPathId
