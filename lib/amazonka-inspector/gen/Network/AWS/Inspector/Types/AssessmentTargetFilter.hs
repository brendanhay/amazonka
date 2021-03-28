{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTargetFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentTargetFilter
  ( AssessmentTargetFilter (..)
  -- * Smart constructor
  , mkAssessmentTargetFilter
  -- * Lenses
  , atfAssessmentTargetNamePattern
  ) where

import qualified Network.AWS.Inspector.Types.NamePattern as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used as the request parameter in the 'ListAssessmentTargets' action.
--
-- /See:/ 'mkAssessmentTargetFilter' smart constructor.
newtype AssessmentTargetFilter = AssessmentTargetFilter'
  { assessmentTargetNamePattern :: Core.Maybe Types.NamePattern
    -- ^ For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssessmentTargetFilter' value with any optional fields omitted.
mkAssessmentTargetFilter
    :: AssessmentTargetFilter
mkAssessmentTargetFilter
  = AssessmentTargetFilter'{assessmentTargetNamePattern =
                              Core.Nothing}

-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
--
-- /Note:/ Consider using 'assessmentTargetNamePattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfAssessmentTargetNamePattern :: Lens.Lens' AssessmentTargetFilter (Core.Maybe Types.NamePattern)
atfAssessmentTargetNamePattern = Lens.field @"assessmentTargetNamePattern"
{-# INLINEABLE atfAssessmentTargetNamePattern #-}
{-# DEPRECATED assessmentTargetNamePattern "Use generic-lens or generic-optics with 'assessmentTargetNamePattern' instead"  #-}

instance Core.FromJSON AssessmentTargetFilter where
        toJSON AssessmentTargetFilter{..}
          = Core.object
              (Core.catMaybes
                 [("assessmentTargetNamePattern" Core..=) Core.<$>
                    assessmentTargetNamePattern])
