{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeRangeProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeRangeProperty
  ( NodeRangeProperty (..),

    -- * Smart constructor
    mkNodeRangeProperty,

    -- * Lenses
    nrpContainer,
    nrpTargetNodes,
  )
where

import Network.AWS.Batch.Types.ContainerProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the properties of the node range for a multi-node parallel job.
--
-- /See:/ 'mkNodeRangeProperty' smart constructor.
data NodeRangeProperty = NodeRangeProperty'
  { container ::
      Lude.Maybe ContainerProperties,
    targetNodes :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeRangeProperty' with the minimum fields required to make a request.
--
-- * 'container' - The container details for the node range.
-- * 'targetNodes' - The range of nodes, using node index values. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range. Your accumulative node ranges must account for all nodes (0:n). You may nest node ranges, for example 0:10 and 4:5, in which case the 4:5 range properties override the 0:10 properties.
mkNodeRangeProperty ::
  -- | 'targetNodes'
  Lude.Text ->
  NodeRangeProperty
mkNodeRangeProperty pTargetNodes_ =
  NodeRangeProperty'
    { container = Lude.Nothing,
      targetNodes = pTargetNodes_
    }

-- | The container details for the node range.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrpContainer :: Lens.Lens' NodeRangeProperty (Lude.Maybe ContainerProperties)
nrpContainer = Lens.lens (container :: NodeRangeProperty -> Lude.Maybe ContainerProperties) (\s a -> s {container = a} :: NodeRangeProperty)
{-# DEPRECATED nrpContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The range of nodes, using node index values. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range. Your accumulative node ranges must account for all nodes (0:n). You may nest node ranges, for example 0:10 and 4:5, in which case the 4:5 range properties override the 0:10 properties.
--
-- /Note:/ Consider using 'targetNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrpTargetNodes :: Lens.Lens' NodeRangeProperty Lude.Text
nrpTargetNodes = Lens.lens (targetNodes :: NodeRangeProperty -> Lude.Text) (\s a -> s {targetNodes = a} :: NodeRangeProperty)
{-# DEPRECATED nrpTargetNodes "Use generic-lens or generic-optics with 'targetNodes' instead." #-}

instance Lude.FromJSON NodeRangeProperty where
  parseJSON =
    Lude.withObject
      "NodeRangeProperty"
      ( \x ->
          NodeRangeProperty'
            Lude.<$> (x Lude..:? "container") Lude.<*> (x Lude..: "targetNodes")
      )

instance Lude.ToJSON NodeRangeProperty where
  toJSON NodeRangeProperty' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("container" Lude..=) Lude.<$> container,
            Lude.Just ("targetNodes" Lude..= targetNodes)
          ]
      )
