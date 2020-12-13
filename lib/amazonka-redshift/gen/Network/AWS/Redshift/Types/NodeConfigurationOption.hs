{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOption
  ( NodeConfigurationOption (..),

    -- * Smart constructor
    mkNodeConfigurationOption,

    -- * Lenses
    ncoMode,
    ncoNumberOfNodes,
    ncoNodeType,
    ncoEstimatedDiskUtilizationPercent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Mode

-- | A list of node configurations.
--
-- /See:/ 'mkNodeConfigurationOption' smart constructor.
data NodeConfigurationOption = NodeConfigurationOption'
  { -- | The category of the node configuration recommendation.
    mode :: Lude.Maybe Mode,
    -- | The number of nodes.
    numberOfNodes :: Lude.Maybe Lude.Int,
    -- | The node type, such as, "ds2.8xlarge".
    nodeType :: Lude.Maybe Lude.Text,
    -- | The estimated disk utilizaton percentage.
    estimatedDiskUtilizationPercent :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeConfigurationOption' with the minimum fields required to make a request.
--
-- * 'mode' - The category of the node configuration recommendation.
-- * 'numberOfNodes' - The number of nodes.
-- * 'nodeType' - The node type, such as, "ds2.8xlarge".
-- * 'estimatedDiskUtilizationPercent' - The estimated disk utilizaton percentage.
mkNodeConfigurationOption ::
  NodeConfigurationOption
mkNodeConfigurationOption =
  NodeConfigurationOption'
    { mode = Lude.Nothing,
      numberOfNodes = Lude.Nothing,
      nodeType = Lude.Nothing,
      estimatedDiskUtilizationPercent = Lude.Nothing
    }

-- | The category of the node configuration recommendation.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoMode :: Lens.Lens' NodeConfigurationOption (Lude.Maybe Mode)
ncoMode = Lens.lens (mode :: NodeConfigurationOption -> Lude.Maybe Mode) (\s a -> s {mode = a} :: NodeConfigurationOption)
{-# DEPRECATED ncoMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The number of nodes.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoNumberOfNodes :: Lens.Lens' NodeConfigurationOption (Lude.Maybe Lude.Int)
ncoNumberOfNodes = Lens.lens (numberOfNodes :: NodeConfigurationOption -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: NodeConfigurationOption)
{-# DEPRECATED ncoNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | The node type, such as, "ds2.8xlarge".
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoNodeType :: Lens.Lens' NodeConfigurationOption (Lude.Maybe Lude.Text)
ncoNodeType = Lens.lens (nodeType :: NodeConfigurationOption -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: NodeConfigurationOption)
{-# DEPRECATED ncoNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The estimated disk utilizaton percentage.
--
-- /Note:/ Consider using 'estimatedDiskUtilizationPercent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncoEstimatedDiskUtilizationPercent :: Lens.Lens' NodeConfigurationOption (Lude.Maybe Lude.Double)
ncoEstimatedDiskUtilizationPercent = Lens.lens (estimatedDiskUtilizationPercent :: NodeConfigurationOption -> Lude.Maybe Lude.Double) (\s a -> s {estimatedDiskUtilizationPercent = a} :: NodeConfigurationOption)
{-# DEPRECATED ncoEstimatedDiskUtilizationPercent "Use generic-lens or generic-optics with 'estimatedDiskUtilizationPercent' instead." #-}

instance Lude.FromXML NodeConfigurationOption where
  parseXML x =
    NodeConfigurationOption'
      Lude.<$> (x Lude..@? "Mode")
      Lude.<*> (x Lude..@? "NumberOfNodes")
      Lude.<*> (x Lude..@? "NodeType")
      Lude.<*> (x Lude..@? "EstimatedDiskUtilizationPercent")
