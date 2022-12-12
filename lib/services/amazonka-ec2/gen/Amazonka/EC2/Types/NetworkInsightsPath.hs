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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Protocol
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a path.
--
-- /See:/ 'newNetworkInsightsPath' smart constructor.
data NetworkInsightsPath = NetworkInsightsPath'
  { -- | The time stamp when the path was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Web Services resource that is the destination of the path.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the Amazon Web Services resource that is the
    -- destination of the path.
    destinationIp :: Prelude.Maybe Prelude.Text,
    -- | The destination port.
    destinationPort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the path.
    networkInsightsPathArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | The protocol.
    protocol :: Prelude.Maybe Protocol,
    -- | The Amazon Web Services resource that is the source of the path.
    source :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the Amazon Web Services resource that is the source of
    -- the path.
    sourceIp :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the path.
    tags :: Prelude.Maybe [Tag]
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
-- 'createdDate', 'networkInsightsPath_createdDate' - The time stamp when the path was created.
--
-- 'destination', 'networkInsightsPath_destination' - The Amazon Web Services resource that is the destination of the path.
--
-- 'destinationArn', 'networkInsightsPath_destinationArn' - The Amazon Resource Name (ARN) of the destination.
--
-- 'destinationIp', 'networkInsightsPath_destinationIp' - The IP address of the Amazon Web Services resource that is the
-- destination of the path.
--
-- 'destinationPort', 'networkInsightsPath_destinationPort' - The destination port.
--
-- 'networkInsightsPathArn', 'networkInsightsPath_networkInsightsPathArn' - The Amazon Resource Name (ARN) of the path.
--
-- 'networkInsightsPathId', 'networkInsightsPath_networkInsightsPathId' - The ID of the path.
--
-- 'protocol', 'networkInsightsPath_protocol' - The protocol.
--
-- 'source', 'networkInsightsPath_source' - The Amazon Web Services resource that is the source of the path.
--
-- 'sourceArn', 'networkInsightsPath_sourceArn' - The Amazon Resource Name (ARN) of the source.
--
-- 'sourceIp', 'networkInsightsPath_sourceIp' - The IP address of the Amazon Web Services resource that is the source of
-- the path.
--
-- 'tags', 'networkInsightsPath_tags' - The tags associated with the path.
newNetworkInsightsPath ::
  NetworkInsightsPath
newNetworkInsightsPath =
  NetworkInsightsPath'
    { createdDate = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      destinationIp = Prelude.Nothing,
      destinationPort = Prelude.Nothing,
      networkInsightsPathArn = Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing,
      protocol = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      sourceIp = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time stamp when the path was created.
networkInsightsPath_createdDate :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.UTCTime)
networkInsightsPath_createdDate = Lens.lens (\NetworkInsightsPath' {createdDate} -> createdDate) (\s@NetworkInsightsPath' {} a -> s {createdDate = a} :: NetworkInsightsPath) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services resource that is the destination of the path.
networkInsightsPath_destination :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destination = Lens.lens (\NetworkInsightsPath' {destination} -> destination) (\s@NetworkInsightsPath' {} a -> s {destination = a} :: NetworkInsightsPath)

-- | The Amazon Resource Name (ARN) of the destination.
networkInsightsPath_destinationArn :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destinationArn = Lens.lens (\NetworkInsightsPath' {destinationArn} -> destinationArn) (\s@NetworkInsightsPath' {} a -> s {destinationArn = a} :: NetworkInsightsPath)

-- | The IP address of the Amazon Web Services resource that is the
-- destination of the path.
networkInsightsPath_destinationIp :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destinationIp = Lens.lens (\NetworkInsightsPath' {destinationIp} -> destinationIp) (\s@NetworkInsightsPath' {} a -> s {destinationIp = a} :: NetworkInsightsPath)

-- | The destination port.
networkInsightsPath_destinationPort :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Int)
networkInsightsPath_destinationPort = Lens.lens (\NetworkInsightsPath' {destinationPort} -> destinationPort) (\s@NetworkInsightsPath' {} a -> s {destinationPort = a} :: NetworkInsightsPath)

-- | The Amazon Resource Name (ARN) of the path.
networkInsightsPath_networkInsightsPathArn :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_networkInsightsPathArn = Lens.lens (\NetworkInsightsPath' {networkInsightsPathArn} -> networkInsightsPathArn) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathArn = a} :: NetworkInsightsPath)

-- | The ID of the path.
networkInsightsPath_networkInsightsPathId :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_networkInsightsPathId = Lens.lens (\NetworkInsightsPath' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsPath)

-- | The protocol.
networkInsightsPath_protocol :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Protocol)
networkInsightsPath_protocol = Lens.lens (\NetworkInsightsPath' {protocol} -> protocol) (\s@NetworkInsightsPath' {} a -> s {protocol = a} :: NetworkInsightsPath)

-- | The Amazon Web Services resource that is the source of the path.
networkInsightsPath_source :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_source = Lens.lens (\NetworkInsightsPath' {source} -> source) (\s@NetworkInsightsPath' {} a -> s {source = a} :: NetworkInsightsPath)

-- | The Amazon Resource Name (ARN) of the source.
networkInsightsPath_sourceArn :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_sourceArn = Lens.lens (\NetworkInsightsPath' {sourceArn} -> sourceArn) (\s@NetworkInsightsPath' {} a -> s {sourceArn = a} :: NetworkInsightsPath)

-- | The IP address of the Amazon Web Services resource that is the source of
-- the path.
networkInsightsPath_sourceIp :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_sourceIp = Lens.lens (\NetworkInsightsPath' {sourceIp} -> sourceIp) (\s@NetworkInsightsPath' {} a -> s {sourceIp = a} :: NetworkInsightsPath)

-- | The tags associated with the path.
networkInsightsPath_tags :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe [Tag])
networkInsightsPath_tags = Lens.lens (\NetworkInsightsPath' {tags} -> tags) (\s@NetworkInsightsPath' {} a -> s {tags = a} :: NetworkInsightsPath) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML NetworkInsightsPath where
  parseXML x =
    NetworkInsightsPath'
      Prelude.<$> (x Data..@? "createdDate")
      Prelude.<*> (x Data..@? "destination")
      Prelude.<*> (x Data..@? "destinationArn")
      Prelude.<*> (x Data..@? "destinationIp")
      Prelude.<*> (x Data..@? "destinationPort")
      Prelude.<*> (x Data..@? "networkInsightsPathArn")
      Prelude.<*> (x Data..@? "networkInsightsPathId")
      Prelude.<*> (x Data..@? "protocol")
      Prelude.<*> (x Data..@? "source")
      Prelude.<*> (x Data..@? "sourceArn")
      Prelude.<*> (x Data..@? "sourceIp")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable NetworkInsightsPath where
  hashWithSalt _salt NetworkInsightsPath' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` destinationIp
      `Prelude.hashWithSalt` destinationPort
      `Prelude.hashWithSalt` networkInsightsPathArn
      `Prelude.hashWithSalt` networkInsightsPathId
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` sourceIp
      `Prelude.hashWithSalt` tags

instance Prelude.NFData NetworkInsightsPath where
  rnf NetworkInsightsPath' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf destinationIp
      `Prelude.seq` Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf networkInsightsPathArn
      `Prelude.seq` Prelude.rnf networkInsightsPathId
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf sourceIp
      `Prelude.seq` Prelude.rnf tags
