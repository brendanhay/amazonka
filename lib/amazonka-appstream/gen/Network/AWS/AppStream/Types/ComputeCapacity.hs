{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ComputeCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ComputeCapacity
  ( ComputeCapacity (..)
  -- * Smart constructor
  , mkComputeCapacity
  -- * Lenses
  , ccDesiredInstances
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the capacity for a fleet.
--
-- /See:/ 'mkComputeCapacity' smart constructor.
newtype ComputeCapacity = ComputeCapacity'
  { desiredInstances :: Core.Int
    -- ^ The desired number of streaming instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeCapacity' value with any optional fields omitted.
mkComputeCapacity
    :: Core.Int -- ^ 'desiredInstances'
    -> ComputeCapacity
mkComputeCapacity desiredInstances
  = ComputeCapacity'{desiredInstances}

-- | The desired number of streaming instances.
--
-- /Note:/ Consider using 'desiredInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDesiredInstances :: Lens.Lens' ComputeCapacity Core.Int
ccDesiredInstances = Lens.field @"desiredInstances"
{-# INLINEABLE ccDesiredInstances #-}
{-# DEPRECATED desiredInstances "Use generic-lens or generic-optics with 'desiredInstances' instead"  #-}

instance Core.FromJSON ComputeCapacity where
        toJSON ComputeCapacity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DesiredInstances" Core..= desiredInstances)])
