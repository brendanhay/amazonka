{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeEnvironmentOrder
  ( ComputeEnvironmentOrder (..),

    -- * Smart constructor
    mkComputeEnvironmentOrder,

    -- * Lenses
    ceoComputeEnvironment,
    ceoOrder,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The order in which compute environments are tried for job placement within a queue. Compute environments are tried in ascending order. For example, if two compute environments are associated with a job queue, the compute environment with a lower order integer value is tried for job placement first.
--
-- /See:/ 'mkComputeEnvironmentOrder' smart constructor.
data ComputeEnvironmentOrder = ComputeEnvironmentOrder'
  { -- | The Amazon Resource Name (ARN) of the compute environment.
    computeEnvironment :: Lude.Text,
    -- | The order of the compute environment.
    order :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeEnvironmentOrder' with the minimum fields required to make a request.
--
-- * 'computeEnvironment' - The Amazon Resource Name (ARN) of the compute environment.
-- * 'order' - The order of the compute environment.
mkComputeEnvironmentOrder ::
  -- | 'computeEnvironment'
  Lude.Text ->
  -- | 'order'
  Lude.Int ->
  ComputeEnvironmentOrder
mkComputeEnvironmentOrder pComputeEnvironment_ pOrder_ =
  ComputeEnvironmentOrder'
    { computeEnvironment =
        pComputeEnvironment_,
      order = pOrder_
    }

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoComputeEnvironment :: Lens.Lens' ComputeEnvironmentOrder Lude.Text
ceoComputeEnvironment = Lens.lens (computeEnvironment :: ComputeEnvironmentOrder -> Lude.Text) (\s a -> s {computeEnvironment = a} :: ComputeEnvironmentOrder)
{-# DEPRECATED ceoComputeEnvironment "Use generic-lens or generic-optics with 'computeEnvironment' instead." #-}

-- | The order of the compute environment.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoOrder :: Lens.Lens' ComputeEnvironmentOrder Lude.Int
ceoOrder = Lens.lens (order :: ComputeEnvironmentOrder -> Lude.Int) (\s a -> s {order = a} :: ComputeEnvironmentOrder)
{-# DEPRECATED ceoOrder "Use generic-lens or generic-optics with 'order' instead." #-}

instance Lude.FromJSON ComputeEnvironmentOrder where
  parseJSON =
    Lude.withObject
      "ComputeEnvironmentOrder"
      ( \x ->
          ComputeEnvironmentOrder'
            Lude.<$> (x Lude..: "computeEnvironment") Lude.<*> (x Lude..: "order")
      )

instance Lude.ToJSON ComputeEnvironmentOrder where
  toJSON ComputeEnvironmentOrder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("computeEnvironment" Lude..= computeEnvironment),
            Lude.Just ("order" Lude..= order)
          ]
      )
