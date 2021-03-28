{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifecyclePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.LifecyclePolicy
  ( LifecyclePolicy (..)
  -- * Smart constructor
  , mkLifecyclePolicy
  -- * Lenses
  , lpTransitionToIA
  ) where

import qualified Network.AWS.EFS.Types.TransitionToIARules as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a policy used by EFS lifecycle management to transition files to the Infrequent Access (IA) storage class.
--
-- /See:/ 'mkLifecyclePolicy' smart constructor.
newtype LifecyclePolicy = LifecyclePolicy'
  { transitionToIA :: Core.Maybe Types.TransitionToIARules
    -- ^ A value that describes the period of time that a file is not accessed, after which it transitions to the IA storage class. Metadata operations such as listing the contents of a directory don't count as file access events.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LifecyclePolicy' value with any optional fields omitted.
mkLifecyclePolicy
    :: LifecyclePolicy
mkLifecyclePolicy = LifecyclePolicy'{transitionToIA = Core.Nothing}

-- | A value that describes the period of time that a file is not accessed, after which it transitions to the IA storage class. Metadata operations such as listing the contents of a directory don't count as file access events.
--
-- /Note:/ Consider using 'transitionToIA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpTransitionToIA :: Lens.Lens' LifecyclePolicy (Core.Maybe Types.TransitionToIARules)
lpTransitionToIA = Lens.field @"transitionToIA"
{-# INLINEABLE lpTransitionToIA #-}
{-# DEPRECATED transitionToIA "Use generic-lens or generic-optics with 'transitionToIA' instead"  #-}

instance Core.FromJSON LifecyclePolicy where
        toJSON LifecyclePolicy{..}
          = Core.object
              (Core.catMaybes
                 [("TransitionToIA" Core..=) Core.<$> transitionToIA])

instance Core.FromJSON LifecyclePolicy where
        parseJSON
          = Core.withObject "LifecyclePolicy" Core.$
              \ x -> LifecyclePolicy' Core.<$> (x Core..:? "TransitionToIA")
