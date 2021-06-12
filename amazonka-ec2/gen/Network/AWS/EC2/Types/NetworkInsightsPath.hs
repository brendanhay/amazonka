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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Protocol
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a path.
--
-- /See:/ 'newNetworkInsightsPath' smart constructor.
data NetworkInsightsPath = NetworkInsightsPath'
  { -- | The time stamp when the path was created.
    createdDate :: Core.Maybe Core.ISO8601,
    -- | The AWS resource that is the source of the path.
    source :: Core.Maybe Core.Text,
    -- | The IP address of the AWS resource that is the destination of the path.
    destinationIp :: Core.Maybe Core.Text,
    -- | The AWS resource that is the destination of the path.
    destination :: Core.Maybe Core.Text,
    -- | The IP address of the AWS resource that is the source of the path.
    sourceIp :: Core.Maybe Core.Text,
    -- | The tags associated with the path.
    tags :: Core.Maybe [Tag],
    -- | The protocol.
    protocol :: Core.Maybe Protocol,
    -- | The Amazon Resource Name (ARN) of the path.
    networkInsightsPathArn :: Core.Maybe Core.Text,
    -- | The ID of the path.
    networkInsightsPathId :: Core.Maybe Core.Text,
    -- | The destination port.
    destinationPort :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { createdDate = Core.Nothing,
      source = Core.Nothing,
      destinationIp = Core.Nothing,
      destination = Core.Nothing,
      sourceIp = Core.Nothing,
      tags = Core.Nothing,
      protocol = Core.Nothing,
      networkInsightsPathArn = Core.Nothing,
      networkInsightsPathId = Core.Nothing,
      destinationPort = Core.Nothing
    }

-- | The time stamp when the path was created.
networkInsightsPath_createdDate :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.UTCTime)
networkInsightsPath_createdDate = Lens.lens (\NetworkInsightsPath' {createdDate} -> createdDate) (\s@NetworkInsightsPath' {} a -> s {createdDate = a} :: NetworkInsightsPath) Core.. Lens.mapping Core._Time

-- | The AWS resource that is the source of the path.
networkInsightsPath_source :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Text)
networkInsightsPath_source = Lens.lens (\NetworkInsightsPath' {source} -> source) (\s@NetworkInsightsPath' {} a -> s {source = a} :: NetworkInsightsPath)

-- | The IP address of the AWS resource that is the destination of the path.
networkInsightsPath_destinationIp :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Text)
networkInsightsPath_destinationIp = Lens.lens (\NetworkInsightsPath' {destinationIp} -> destinationIp) (\s@NetworkInsightsPath' {} a -> s {destinationIp = a} :: NetworkInsightsPath)

-- | The AWS resource that is the destination of the path.
networkInsightsPath_destination :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Text)
networkInsightsPath_destination = Lens.lens (\NetworkInsightsPath' {destination} -> destination) (\s@NetworkInsightsPath' {} a -> s {destination = a} :: NetworkInsightsPath)

-- | The IP address of the AWS resource that is the source of the path.
networkInsightsPath_sourceIp :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Text)
networkInsightsPath_sourceIp = Lens.lens (\NetworkInsightsPath' {sourceIp} -> sourceIp) (\s@NetworkInsightsPath' {} a -> s {sourceIp = a} :: NetworkInsightsPath)

-- | The tags associated with the path.
networkInsightsPath_tags :: Lens.Lens' NetworkInsightsPath (Core.Maybe [Tag])
networkInsightsPath_tags = Lens.lens (\NetworkInsightsPath' {tags} -> tags) (\s@NetworkInsightsPath' {} a -> s {tags = a} :: NetworkInsightsPath) Core.. Lens.mapping Lens._Coerce

-- | The protocol.
networkInsightsPath_protocol :: Lens.Lens' NetworkInsightsPath (Core.Maybe Protocol)
networkInsightsPath_protocol = Lens.lens (\NetworkInsightsPath' {protocol} -> protocol) (\s@NetworkInsightsPath' {} a -> s {protocol = a} :: NetworkInsightsPath)

-- | The Amazon Resource Name (ARN) of the path.
networkInsightsPath_networkInsightsPathArn :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Text)
networkInsightsPath_networkInsightsPathArn = Lens.lens (\NetworkInsightsPath' {networkInsightsPathArn} -> networkInsightsPathArn) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathArn = a} :: NetworkInsightsPath)

-- | The ID of the path.
networkInsightsPath_networkInsightsPathId :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Text)
networkInsightsPath_networkInsightsPathId = Lens.lens (\NetworkInsightsPath' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsPath' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsPath)

-- | The destination port.
networkInsightsPath_destinationPort :: Lens.Lens' NetworkInsightsPath (Core.Maybe Core.Int)
networkInsightsPath_destinationPort = Lens.lens (\NetworkInsightsPath' {destinationPort} -> destinationPort) (\s@NetworkInsightsPath' {} a -> s {destinationPort = a} :: NetworkInsightsPath)

instance Core.FromXML NetworkInsightsPath where
  parseXML x =
    NetworkInsightsPath'
      Core.<$> (x Core..@? "createdDate")
      Core.<*> (x Core..@? "source")
      Core.<*> (x Core..@? "destinationIp")
      Core.<*> (x Core..@? "destination")
      Core.<*> (x Core..@? "sourceIp")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "protocol")
      Core.<*> (x Core..@? "networkInsightsPathArn")
      Core.<*> (x Core..@? "networkInsightsPathId")
      Core.<*> (x Core..@? "destinationPort")

instance Core.Hashable NetworkInsightsPath

instance Core.NFData NetworkInsightsPath
