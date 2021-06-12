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
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Mode

-- | A list of node configurations.
--
-- /See:/ 'newNodeConfigurationOption' smart constructor.
data NodeConfigurationOption = NodeConfigurationOption'
  { -- | The category of the node configuration recommendation.
    mode :: Core.Maybe Mode,
    -- | The number of nodes.
    numberOfNodes :: Core.Maybe Core.Int,
    -- | The estimated disk utilizaton percentage.
    estimatedDiskUtilizationPercent :: Core.Maybe Core.Double,
    -- | The node type, such as, \"ds2.8xlarge\".
    nodeType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeConfigurationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'nodeConfigurationOption_mode' - The category of the node configuration recommendation.
--
-- 'numberOfNodes', 'nodeConfigurationOption_numberOfNodes' - The number of nodes.
--
-- 'estimatedDiskUtilizationPercent', 'nodeConfigurationOption_estimatedDiskUtilizationPercent' - The estimated disk utilizaton percentage.
--
-- 'nodeType', 'nodeConfigurationOption_nodeType' - The node type, such as, \"ds2.8xlarge\".
newNodeConfigurationOption ::
  NodeConfigurationOption
newNodeConfigurationOption =
  NodeConfigurationOption'
    { mode = Core.Nothing,
      numberOfNodes = Core.Nothing,
      estimatedDiskUtilizationPercent = Core.Nothing,
      nodeType = Core.Nothing
    }

-- | The category of the node configuration recommendation.
nodeConfigurationOption_mode :: Lens.Lens' NodeConfigurationOption (Core.Maybe Mode)
nodeConfigurationOption_mode = Lens.lens (\NodeConfigurationOption' {mode} -> mode) (\s@NodeConfigurationOption' {} a -> s {mode = a} :: NodeConfigurationOption)

-- | The number of nodes.
nodeConfigurationOption_numberOfNodes :: Lens.Lens' NodeConfigurationOption (Core.Maybe Core.Int)
nodeConfigurationOption_numberOfNodes = Lens.lens (\NodeConfigurationOption' {numberOfNodes} -> numberOfNodes) (\s@NodeConfigurationOption' {} a -> s {numberOfNodes = a} :: NodeConfigurationOption)

-- | The estimated disk utilizaton percentage.
nodeConfigurationOption_estimatedDiskUtilizationPercent :: Lens.Lens' NodeConfigurationOption (Core.Maybe Core.Double)
nodeConfigurationOption_estimatedDiskUtilizationPercent = Lens.lens (\NodeConfigurationOption' {estimatedDiskUtilizationPercent} -> estimatedDiskUtilizationPercent) (\s@NodeConfigurationOption' {} a -> s {estimatedDiskUtilizationPercent = a} :: NodeConfigurationOption)

-- | The node type, such as, \"ds2.8xlarge\".
nodeConfigurationOption_nodeType :: Lens.Lens' NodeConfigurationOption (Core.Maybe Core.Text)
nodeConfigurationOption_nodeType = Lens.lens (\NodeConfigurationOption' {nodeType} -> nodeType) (\s@NodeConfigurationOption' {} a -> s {nodeType = a} :: NodeConfigurationOption)

instance Core.FromXML NodeConfigurationOption where
  parseXML x =
    NodeConfigurationOption'
      Core.<$> (x Core..@? "Mode")
      Core.<*> (x Core..@? "NumberOfNodes")
      Core.<*> (x Core..@? "EstimatedDiskUtilizationPercent")
      Core.<*> (x Core..@? "NodeType")

instance Core.Hashable NodeConfigurationOption

instance Core.NFData NodeConfigurationOption
