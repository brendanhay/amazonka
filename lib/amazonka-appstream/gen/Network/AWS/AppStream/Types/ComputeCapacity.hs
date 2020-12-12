{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ComputeCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ComputeCapacity
  ( ComputeCapacity (..),

    -- * Smart constructor
    mkComputeCapacity,

    -- * Lenses
    ccDesiredInstances,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the capacity for a fleet.
--
-- /See:/ 'mkComputeCapacity' smart constructor.
newtype ComputeCapacity = ComputeCapacity'
  { desiredInstances ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeCapacity' with the minimum fields required to make a request.
--
-- * 'desiredInstances' - The desired number of streaming instances.
mkComputeCapacity ::
  -- | 'desiredInstances'
  Lude.Int ->
  ComputeCapacity
mkComputeCapacity pDesiredInstances_ =
  ComputeCapacity' {desiredInstances = pDesiredInstances_}

-- | The desired number of streaming instances.
--
-- /Note:/ Consider using 'desiredInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDesiredInstances :: Lens.Lens' ComputeCapacity Lude.Int
ccDesiredInstances = Lens.lens (desiredInstances :: ComputeCapacity -> Lude.Int) (\s a -> s {desiredInstances = a} :: ComputeCapacity)
{-# DEPRECATED ccDesiredInstances "Use generic-lens or generic-optics with 'desiredInstances' instead." #-}

instance Lude.ToJSON ComputeCapacity where
  toJSON ComputeCapacity' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("DesiredInstances" Lude..= desiredInstances)]
      )
