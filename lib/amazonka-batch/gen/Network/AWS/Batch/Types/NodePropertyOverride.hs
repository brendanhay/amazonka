{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodePropertyOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodePropertyOverride
  ( NodePropertyOverride (..),

    -- * Smart constructor
    mkNodePropertyOverride,

    -- * Lenses
    npoContainerOverrides,
    npoTargetNodes,
  )
where

import Network.AWS.Batch.Types.ContainerOverrides
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Object representing any node overrides to a job definition that is used in a 'SubmitJob' API operation.
--
-- /See:/ 'mkNodePropertyOverride' smart constructor.
data NodePropertyOverride = NodePropertyOverride'
  { -- | The overrides that should be sent to a node range.
    containerOverrides :: Lude.Maybe ContainerOverrides,
    -- | The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
    targetNodes :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodePropertyOverride' with the minimum fields required to make a request.
--
-- * 'containerOverrides' - The overrides that should be sent to a node range.
-- * 'targetNodes' - The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
mkNodePropertyOverride ::
  -- | 'targetNodes'
  Lude.Text ->
  NodePropertyOverride
mkNodePropertyOverride pTargetNodes_ =
  NodePropertyOverride'
    { containerOverrides = Lude.Nothing,
      targetNodes = pTargetNodes_
    }

-- | The overrides that should be sent to a node range.
--
-- /Note:/ Consider using 'containerOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npoContainerOverrides :: Lens.Lens' NodePropertyOverride (Lude.Maybe ContainerOverrides)
npoContainerOverrides = Lens.lens (containerOverrides :: NodePropertyOverride -> Lude.Maybe ContainerOverrides) (\s a -> s {containerOverrides = a} :: NodePropertyOverride)
{-# DEPRECATED npoContainerOverrides "Use generic-lens or generic-optics with 'containerOverrides' instead." #-}

-- | The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
--
-- /Note:/ Consider using 'targetNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npoTargetNodes :: Lens.Lens' NodePropertyOverride Lude.Text
npoTargetNodes = Lens.lens (targetNodes :: NodePropertyOverride -> Lude.Text) (\s a -> s {targetNodes = a} :: NodePropertyOverride)
{-# DEPRECATED npoTargetNodes "Use generic-lens or generic-optics with 'targetNodes' instead." #-}

instance Lude.ToJSON NodePropertyOverride where
  toJSON NodePropertyOverride' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("containerOverrides" Lude..=) Lude.<$> containerOverrides,
            Lude.Just ("targetNodes" Lude..= targetNodes)
          ]
      )
