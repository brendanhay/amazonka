{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ProjectStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.ProjectStatus
  ( ProjectStatus (..)
  -- * Smart constructor
  , mkProjectStatus
  -- * Lenses
  , psState
  , psReason
  ) where

import qualified Network.AWS.CodeStar.Types.Reason as Types
import qualified Network.AWS.CodeStar.Types.State as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An indication of whether a project creation or deletion is failed or successful.
--
-- /See:/ 'mkProjectStatus' smart constructor.
data ProjectStatus = ProjectStatus'
  { state :: Types.State
    -- ^ The phase of completion for a project creation or deletion.
  , reason :: Core.Maybe Types.Reason
    -- ^ In the case of a project creation or deletion failure, a reason for the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectStatus' value with any optional fields omitted.
mkProjectStatus
    :: Types.State -- ^ 'state'
    -> ProjectStatus
mkProjectStatus state
  = ProjectStatus'{state, reason = Core.Nothing}

-- | The phase of completion for a project creation or deletion.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psState :: Lens.Lens' ProjectStatus Types.State
psState = Lens.field @"state"
{-# INLINEABLE psState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | In the case of a project creation or deletion failure, a reason for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psReason :: Lens.Lens' ProjectStatus (Core.Maybe Types.Reason)
psReason = Lens.field @"reason"
{-# INLINEABLE psReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.FromJSON ProjectStatus where
        parseJSON
          = Core.withObject "ProjectStatus" Core.$
              \ x ->
                ProjectStatus' Core.<$>
                  (x Core..: "state") Core.<*> x Core..:? "reason"
