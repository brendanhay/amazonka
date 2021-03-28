{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.NodeConfigurationOption
  ( NodeConfigurationOption (..)
  -- * Smart constructor
  , mkNodeConfigurationOption
  -- * Lenses
  , ncoEstimatedDiskUtilizationPercent
  , ncoMode
  , ncoNodeType
  , ncoNumberOfNodes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.Mode as Types

-- | A list of node configurations.
--
-- /See:/ 'mkNodeConfigurationOption' smart constructor.
data NodeConfigurationOption = NodeConfigurationOption'
  { estimatedDiskUtilizationPercent :: Core.Maybe Core.Double
    -- ^ The estimated disk utilizaton percentage.
  , mode :: Core.Maybe Types.Mode
    -- ^ The category of the node configuration recommendation.
  , nodeType :: Core.Maybe Core.Text
    -- ^ The node type, such as, "ds2.8xlarge".
  , numberOfNodes :: Core.Maybe Core.Int
    -- ^ The number of nodes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeConfigurationOption' value with any optional fields omitted.
mkNodeConfigurationOption
    :: NodeConfigurationOption
mkNodeConfigurationOption
  = NodeConfigurationOption'{estimatedDiskUtilizationPercent =
                               Core.Nothing,
                             mode = Core.Nothing, nodeType = Core.Nothing,
                             numberOfNodes = Core.Nothing}

-- | The estimated disk utilizaton percentage.
--
-- /Note:/ Consider using 'estimatedDiskUtilizationPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoEstimatedDiskUtilizationPercent :: Lens.Lens' NodeConfigurationOption (Core.Maybe Core.Double)
ncoEstimatedDiskUtilizationPercent = Lens.field @"estimatedDiskUtilizationPercent"
{-# INLINEABLE ncoEstimatedDiskUtilizationPercent #-}
{-# DEPRECATED estimatedDiskUtilizationPercent "Use generic-lens or generic-optics with 'estimatedDiskUtilizationPercent' instead"  #-}

-- | The category of the node configuration recommendation.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoMode :: Lens.Lens' NodeConfigurationOption (Core.Maybe Types.Mode)
ncoMode = Lens.field @"mode"
{-# INLINEABLE ncoMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The node type, such as, "ds2.8xlarge".
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoNodeType :: Lens.Lens' NodeConfigurationOption (Core.Maybe Core.Text)
ncoNodeType = Lens.field @"nodeType"
{-# INLINEABLE ncoNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The number of nodes.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoNumberOfNodes :: Lens.Lens' NodeConfigurationOption (Core.Maybe Core.Int)
ncoNumberOfNodes = Lens.field @"numberOfNodes"
{-# INLINEABLE ncoNumberOfNodes #-}
{-# DEPRECATED numberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead"  #-}

instance Core.FromXML NodeConfigurationOption where
        parseXML x
          = NodeConfigurationOption' Core.<$>
              (x Core..@? "EstimatedDiskUtilizationPercent") Core.<*>
                x Core..@? "Mode"
                Core.<*> x Core..@? "NodeType"
                Core.<*> x Core..@? "NumberOfNodes"
