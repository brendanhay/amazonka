{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ManagedScalingPolicy
  ( ManagedScalingPolicy (..)
  -- * Smart constructor
  , mkManagedScalingPolicy
  -- * Lenses
  , mspComputeLimits
  ) where

import qualified Network.AWS.EMR.Types.ComputeLimits as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Managed scaling policy for an Amazon EMR cluster. The policy specifies the limits for resources that can be added or terminated from a cluster. The policy only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
-- /See:/ 'mkManagedScalingPolicy' smart constructor.
newtype ManagedScalingPolicy = ManagedScalingPolicy'
  { computeLimits :: Core.Maybe Types.ComputeLimits
    -- ^ The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster is not allowed to go above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ManagedScalingPolicy' value with any optional fields omitted.
mkManagedScalingPolicy
    :: ManagedScalingPolicy
mkManagedScalingPolicy
  = ManagedScalingPolicy'{computeLimits = Core.Nothing}

-- | The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster is not allowed to go above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
-- /Note:/ Consider using 'computeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mspComputeLimits :: Lens.Lens' ManagedScalingPolicy (Core.Maybe Types.ComputeLimits)
mspComputeLimits = Lens.field @"computeLimits"
{-# INLINEABLE mspComputeLimits #-}
{-# DEPRECATED computeLimits "Use generic-lens or generic-optics with 'computeLimits' instead"  #-}

instance Core.FromJSON ManagedScalingPolicy where
        toJSON ManagedScalingPolicy{..}
          = Core.object
              (Core.catMaybes [("ComputeLimits" Core..=) Core.<$> computeLimits])

instance Core.FromJSON ManagedScalingPolicy where
        parseJSON
          = Core.withObject "ManagedScalingPolicy" Core.$
              \ x -> ManagedScalingPolicy' Core.<$> (x Core..:? "ComputeLimits")
