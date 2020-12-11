-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeOverrides
  ( NodeOverrides (..),

    -- * Smart constructor
    mkNodeOverrides,

    -- * Lenses
    noNumNodes,
    noNodePropertyOverrides,
  )
where

import Network.AWS.Batch.Types.NodePropertyOverride
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Object representing any node overrides to a job definition that is used in a 'SubmitJob' API operation.
--
-- /See:/ 'mkNodeOverrides' smart constructor.
data NodeOverrides = NodeOverrides'
  { numNodes ::
      Lude.Maybe Lude.Int,
    nodePropertyOverrides :: Lude.Maybe [NodePropertyOverride]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeOverrides' with the minimum fields required to make a request.
--
-- * 'nodePropertyOverrides' - The node property overrides for the job.
-- * 'numNodes' - The number of nodes to use with a multi-node parallel job. This value overrides the number of nodes that are specified in the job definition. To use this override:
--
--
--     * There must be at least one node range in your job definition that has an open upper boundary (such as @:@ or @n:@ ).
--
--
--     * The lower boundary of the node range specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
--     * The main node index specified in the job definition must be fewer than the number of nodes specified in the override.
mkNodeOverrides ::
  NodeOverrides
mkNodeOverrides =
  NodeOverrides'
    { numNodes = Lude.Nothing,
      nodePropertyOverrides = Lude.Nothing
    }

-- | The number of nodes to use with a multi-node parallel job. This value overrides the number of nodes that are specified in the job definition. To use this override:
--
--
--     * There must be at least one node range in your job definition that has an open upper boundary (such as @:@ or @n:@ ).
--
--
--     * The lower boundary of the node range specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
--     * The main node index specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
--
-- /Note:/ Consider using 'numNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noNumNodes :: Lens.Lens' NodeOverrides (Lude.Maybe Lude.Int)
noNumNodes = Lens.lens (numNodes :: NodeOverrides -> Lude.Maybe Lude.Int) (\s a -> s {numNodes = a} :: NodeOverrides)
{-# DEPRECATED noNumNodes "Use generic-lens or generic-optics with 'numNodes' instead." #-}

-- | The node property overrides for the job.
--
-- /Note:/ Consider using 'nodePropertyOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noNodePropertyOverrides :: Lens.Lens' NodeOverrides (Lude.Maybe [NodePropertyOverride])
noNodePropertyOverrides = Lens.lens (nodePropertyOverrides :: NodeOverrides -> Lude.Maybe [NodePropertyOverride]) (\s a -> s {nodePropertyOverrides = a} :: NodeOverrides)
{-# DEPRECATED noNodePropertyOverrides "Use generic-lens or generic-optics with 'nodePropertyOverrides' instead." #-}

instance Lude.ToJSON NodeOverrides where
  toJSON NodeOverrides' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("numNodes" Lude..=) Lude.<$> numNodes,
            ("nodePropertyOverrides" Lude..=) Lude.<$> nodePropertyOverrides
          ]
      )
