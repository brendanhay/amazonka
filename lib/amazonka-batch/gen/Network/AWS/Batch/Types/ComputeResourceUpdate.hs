{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeResourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeResourceUpdate
  ( ComputeResourceUpdate (..),

    -- * Smart constructor
    mkComputeResourceUpdate,

    -- * Lenses
    cruMinvCPUs,
    cruMaxvCPUs,
    cruDesiredvCPUs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the attributes of a compute environment that can be updated.
--
-- /See:/ 'mkComputeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
  { -- | The minimum number of Amazon EC2 vCPUs that an environment should maintain.
    minvCPUs :: Lude.Maybe Lude.Int,
    -- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
    maxvCPUs :: Lude.Maybe Lude.Int,
    -- | The desired number of Amazon EC2 vCPUS in the compute environment.
    desiredvCPUs :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeResourceUpdate' with the minimum fields required to make a request.
--
-- * 'minvCPUs' - The minimum number of Amazon EC2 vCPUs that an environment should maintain.
-- * 'maxvCPUs' - The maximum number of Amazon EC2 vCPUs that an environment can reach.
-- * 'desiredvCPUs' - The desired number of Amazon EC2 vCPUS in the compute environment.
mkComputeResourceUpdate ::
  ComputeResourceUpdate
mkComputeResourceUpdate =
  ComputeResourceUpdate'
    { minvCPUs = Lude.Nothing,
      maxvCPUs = Lude.Nothing,
      desiredvCPUs = Lude.Nothing
    }

-- | The minimum number of Amazon EC2 vCPUs that an environment should maintain.
--
-- /Note:/ Consider using 'minvCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cruMinvCPUs :: Lens.Lens' ComputeResourceUpdate (Lude.Maybe Lude.Int)
cruMinvCPUs = Lens.lens (minvCPUs :: ComputeResourceUpdate -> Lude.Maybe Lude.Int) (\s a -> s {minvCPUs = a} :: ComputeResourceUpdate)
{-# DEPRECATED cruMinvCPUs "Use generic-lens or generic-optics with 'minvCPUs' instead." #-}

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- /Note:/ Consider using 'maxvCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cruMaxvCPUs :: Lens.Lens' ComputeResourceUpdate (Lude.Maybe Lude.Int)
cruMaxvCPUs = Lens.lens (maxvCPUs :: ComputeResourceUpdate -> Lude.Maybe Lude.Int) (\s a -> s {maxvCPUs = a} :: ComputeResourceUpdate)
{-# DEPRECATED cruMaxvCPUs "Use generic-lens or generic-optics with 'maxvCPUs' instead." #-}

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- /Note:/ Consider using 'desiredvCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cruDesiredvCPUs :: Lens.Lens' ComputeResourceUpdate (Lude.Maybe Lude.Int)
cruDesiredvCPUs = Lens.lens (desiredvCPUs :: ComputeResourceUpdate -> Lude.Maybe Lude.Int) (\s a -> s {desiredvCPUs = a} :: ComputeResourceUpdate)
{-# DEPRECATED cruDesiredvCPUs "Use generic-lens or generic-optics with 'desiredvCPUs' instead." #-}

instance Lude.ToJSON ComputeResourceUpdate where
  toJSON ComputeResourceUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("minvCpus" Lude..=) Lude.<$> minvCPUs,
            ("maxvCpus" Lude..=) Lude.<$> maxvCPUs,
            ("desiredvCpus" Lude..=) Lude.<$> desiredvCPUs
          ]
      )
