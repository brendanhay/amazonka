{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeProperties
  ( NodeProperties (..),

    -- * Smart constructor
    mkNodeProperties,

    -- * Lenses
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,
  )
where

import Network.AWS.Batch.Types.NodeRangeProperty
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the node properties of a multi-node parallel job.
--
-- /See:/ 'mkNodeProperties' smart constructor.
data NodeProperties = NodeProperties'
  { -- | The number of nodes associated with a multi-node parallel job.
    numNodes :: Lude.Int,
    -- | Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
    mainNode :: Lude.Int,
    -- | A list of node ranges and their properties associated with a multi-node parallel job.
    nodeRangeProperties :: [NodeRangeProperty]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeProperties' with the minimum fields required to make a request.
--
-- * 'numNodes' - The number of nodes associated with a multi-node parallel job.
-- * 'mainNode' - Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
-- * 'nodeRangeProperties' - A list of node ranges and their properties associated with a multi-node parallel job.
mkNodeProperties ::
  -- | 'numNodes'
  Lude.Int ->
  -- | 'mainNode'
  Lude.Int ->
  NodeProperties
mkNodeProperties pNumNodes_ pMainNode_ =
  NodeProperties'
    { numNodes = pNumNodes_,
      mainNode = pMainNode_,
      nodeRangeProperties = Lude.mempty
    }

-- | The number of nodes associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'numNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNumNodes :: Lens.Lens' NodeProperties Lude.Int
npNumNodes = Lens.lens (numNodes :: NodeProperties -> Lude.Int) (\s a -> s {numNodes = a} :: NodeProperties)
{-# DEPRECATED npNumNodes "Use generic-lens or generic-optics with 'numNodes' instead." #-}

-- | Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
--
-- /Note:/ Consider using 'mainNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npMainNode :: Lens.Lens' NodeProperties Lude.Int
npMainNode = Lens.lens (mainNode :: NodeProperties -> Lude.Int) (\s a -> s {mainNode = a} :: NodeProperties)
{-# DEPRECATED npMainNode "Use generic-lens or generic-optics with 'mainNode' instead." #-}

-- | A list of node ranges and their properties associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeRangeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNodeRangeProperties :: Lens.Lens' NodeProperties [NodeRangeProperty]
npNodeRangeProperties = Lens.lens (nodeRangeProperties :: NodeProperties -> [NodeRangeProperty]) (\s a -> s {nodeRangeProperties = a} :: NodeProperties)
{-# DEPRECATED npNodeRangeProperties "Use generic-lens or generic-optics with 'nodeRangeProperties' instead." #-}

instance Lude.FromJSON NodeProperties where
  parseJSON =
    Lude.withObject
      "NodeProperties"
      ( \x ->
          NodeProperties'
            Lude.<$> (x Lude..: "numNodes")
            Lude.<*> (x Lude..: "mainNode")
            Lude.<*> (x Lude..:? "nodeRangeProperties" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON NodeProperties where
  toJSON NodeProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("numNodes" Lude..= numNodes),
            Lude.Just ("mainNode" Lude..= mainNode),
            Lude.Just ("nodeRangeProperties" Lude..= nodeRangeProperties)
          ]
      )
