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
-- Module      : Network.AWS.EC2.Types.NetworkInsightsPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInsightsPath where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Protocol
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a path.
--
-- /See:/ 'newNetworkInsightsPath' smart constructor.
data NetworkInsightsPath = NetworkInsightsPath'
  { -- | The time stamp when the path was created.
    createdDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The AWS resource that is the source of the path.
    source :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the AWS resource that is the destination of the path.
    destinationIp :: Prelude.Maybe Prelude.Text,
    -- | The AWS resource that is the destination of the path.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the AWS resource that is the source of the path.
    sourceIp :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the path.
    tags :: Prelude.Maybe [Tag],
    -- | The protocol.
    protocol :: Prelude.Maybe Protocol,
    -- | The Amazon Resource Name (ARN) of the path.
    networkInsightsPathArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | The destination port.
    destinationPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'source', 'networkInsightsPath_source' - The AWS resource that is the source of the path.
--
-- 'destinationIp', 'networkInsightsPath_destinationIp' - The IP address of the AWS resource that is the destination of the path.
--
-- 'destination', 'networkInsightsPath_destination' - The AWS resource that is the destination of the path.
--
-- 'sourceIp', 'networkInsightsPath_sourceIp' - The IP address of the AWS resource that is the source of the path.
--
-- 'tags', 'networkInsightsPath_tags' - The tags associated with the path.
--
-- 'protocol', 'networkInsightsPath_protocol' - The protocol.
--
-- 'networkInsightsPathArn', 'networkInsightsPath_networkInsightsPathArn' - The Amazon Resource Name (ARN) of the path.
--
-- 'networkInsightsPathId', 'networkInsightsPath_networkInsightsPathId' - The ID of the path.
--
-- 'destinationPort', 'networkInsightsPath_destinationPort' - The destination port.
newNetworkInsightsPath ::
  NetworkInsightsPath
newNetworkInsightsPath =
  NetworkInsightsPath'
    { createdDate = Prelude.Nothing,
      source = Prelude.Nothing,
      destinationIp = Prelude.Nothing,
      destination = Prelude.Nothing,
      sourceIp = Prelude.Nothing,
      tags = Prelude.Nothing,
      protocol = Prelude.Nothing,
      networkInsightsPathArn = Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing,
      destinationPort = Prelude.Nothing
    }

-- | The time stamp when the path was created.
networkInsightsPath_createdDate :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.UTCTime)
networkInsightsPath_createdDate = Lens.lens (\NetworkInsightsPath' {createdDate} -> createdDate) (\s@NetworkInsightsPath' {} a -> s {createdDate = a} :: NetworkInsightsPath) Prelude.. Lens.mapping Prelude._Time

-- | The AWS resource that is the source of the path.
networkInsightsPath_source :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_source = Lens.lens (\NetworkInsightsPath' {source} -> source) (\s@NetworkInsightsPath' {} a -> s {source = a} :: NetworkInsightsPath)

-- | The IP address of the AWS resource that is the destination of the path.
networkInsightsPath_destinationIp :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destinationIp = Lens.lens (\NetworkInsightsPath' {destinationIp} -> destinationIp) (\s@NetworkInsightsPath' {} a -> s {destinationIp = a} :: NetworkInsightsPath)

-- | The AWS resource that is the destination of the path.
networkInsightsPath_destination :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_destination = Lens.lens (\NetworkInsightsPath' {destination} -> destination) (\s@NetworkInsightsPath' {} a -> s {destination = a} :: NetworkInsightsPath)

-- | The IP address of the AWS resource that is the source of the path.
networkInsightsPath_sourceIp :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_sourceIp = Lens.lens (\NetworkInsightsPath' {sourceIp} -> sourceIp) (\s@NetworkInsightsPath' {} a -> s {sourceIp = a} :: NetworkInsightsPath)

-- | The tags associated with the path.
networkInsightsPath_tags :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe [Tag])
networkInsightsPath_tags = Lens.lens (\NetworkInsightsPath' {tags} -> tags) (\s@NetworkInsightsPath' {} a -> s {tags = a} :: NetworkInsightsPath) Prelude.. Lens.mapping Prelude._Coerce

-- | The protocol.
networkInsightsPath_protocol :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Protocol)
networkInsightsPath_protocol = Lens.lens (\NetworkInsightsPath' {protocol} -> protocol) (\s@NetworkInsightsPath' {} a -> s {protocol = a} :: NetworkInsightsPath)

-- | The Amazon Resource Name (ARN) of the path.
networkInsightsPath_networkInsightsPathArn :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_networkInsightsPathArn = Lens.lens (\NetworkInsightsPath' {networkInsightsPathArn} -> networkInsightsPathArn) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathArn = a} :: NetworkInsightsPath)

-- | The ID of the path.
networkInsightsPath_networkInsightsPathId :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Text)
networkInsightsPath_networkInsightsPathId = Lens.lens (\NetworkInsightsPath' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsPath)

-- | The destination port.
networkInsightsPath_destinationPort :: Lens.Lens' NetworkInsightsPath (Prelude.Maybe Prelude.Int)
networkInsightsPath_destinationPort = Lens.lens (\NetworkInsightsPath' {destinationPort} -> destinationPort) (\s@NetworkInsightsPath' {} a -> s {destinationPort = a} :: NetworkInsightsPath)

instance Prelude.FromXML NetworkInsightsPath where
  parseXML x =
    NetworkInsightsPath'
      Prelude.<$> (x Prelude..@? "createdDate")
      Prelude.<*> (x Prelude..@? "source")
      Prelude.<*> (x Prelude..@? "destinationIp")
      Prelude.<*> (x Prelude..@? "destination")
      Prelude.<*> (x Prelude..@? "sourceIp")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "protocol")
      Prelude.<*> (x Prelude..@? "networkInsightsPathArn")
      Prelude.<*> (x Prelude..@? "networkInsightsPathId")
      Prelude.<*> (x Prelude..@? "destinationPort")

instance Prelude.Hashable NetworkInsightsPath

instance Prelude.NFData NetworkInsightsPath
