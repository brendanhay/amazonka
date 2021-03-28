{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UniqueProblem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.UniqueProblem
  ( UniqueProblem (..)
  -- * Smart constructor
  , mkUniqueProblem
  -- * Lenses
  , upMessage
  , upProblems
  ) where

import qualified Network.AWS.DeviceFarm.Types.Message as Types
import qualified Network.AWS.DeviceFarm.Types.Problem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of one or more problems, grouped by their result.
--
-- /See:/ 'mkUniqueProblem' smart constructor.
data UniqueProblem = UniqueProblem'
  { message :: Core.Maybe Types.Message
    -- ^ A message about the unique problems' result.
  , problems :: Core.Maybe [Types.Problem]
    -- ^ Information about the problems.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UniqueProblem' value with any optional fields omitted.
mkUniqueProblem
    :: UniqueProblem
mkUniqueProblem
  = UniqueProblem'{message = Core.Nothing, problems = Core.Nothing}

-- | A message about the unique problems' result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMessage :: Lens.Lens' UniqueProblem (Core.Maybe Types.Message)
upMessage = Lens.field @"message"
{-# INLINEABLE upMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | Information about the problems.
--
-- /Note:/ Consider using 'problems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProblems :: Lens.Lens' UniqueProblem (Core.Maybe [Types.Problem])
upProblems = Lens.field @"problems"
{-# INLINEABLE upProblems #-}
{-# DEPRECATED problems "Use generic-lens or generic-optics with 'problems' instead"  #-}

instance Core.FromJSON UniqueProblem where
        parseJSON
          = Core.withObject "UniqueProblem" Core.$
              \ x ->
                UniqueProblem' Core.<$>
                  (x Core..:? "message") Core.<*> x Core..:? "problems"
