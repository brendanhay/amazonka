{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.FindingCriteria
  ( FindingCriteria (..)
  -- * Smart constructor
  , mkFindingCriteria
  -- * Lenses
  , fcCriterion
  ) where

import qualified Network.AWS.GuardDuty.Types.Condition as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the criteria used for querying findings.
--
-- /See:/ 'mkFindingCriteria' smart constructor.
newtype FindingCriteria = FindingCriteria'
  { criterion :: Core.Maybe (Core.HashMap Core.Text Types.Condition)
    -- ^ Represents a map of finding properties that match specified conditions and values when querying findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FindingCriteria' value with any optional fields omitted.
mkFindingCriteria
    :: FindingCriteria
mkFindingCriteria = FindingCriteria'{criterion = Core.Nothing}

-- | Represents a map of finding properties that match specified conditions and values when querying findings.
--
-- /Note:/ Consider using 'criterion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcCriterion :: Lens.Lens' FindingCriteria (Core.Maybe (Core.HashMap Core.Text Types.Condition))
fcCriterion = Lens.field @"criterion"
{-# INLINEABLE fcCriterion #-}
{-# DEPRECATED criterion "Use generic-lens or generic-optics with 'criterion' instead"  #-}

instance Core.FromJSON FindingCriteria where
        toJSON FindingCriteria{..}
          = Core.object
              (Core.catMaybes [("criterion" Core..=) Core.<$> criterion])

instance Core.FromJSON FindingCriteria where
        parseJSON
          = Core.withObject "FindingCriteria" Core.$
              \ x -> FindingCriteria' Core.<$> (x Core..:? "criterion")
