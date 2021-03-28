{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTemplateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentTemplateFilter
  ( AssessmentTemplateFilter (..)
  -- * Smart constructor
  , mkAssessmentTemplateFilter
  -- * Lenses
  , atfDurationRange
  , atfNamePattern
  , atfRulesPackageArns
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.DurationRange as Types
import qualified Network.AWS.Inspector.Types.NamePattern as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used as the request parameter in the 'ListAssessmentTemplates' action.
--
-- /See:/ 'mkAssessmentTemplateFilter' smart constructor.
data AssessmentTemplateFilter = AssessmentTemplateFilter'
  { durationRange :: Core.Maybe Types.DurationRange
    -- ^ For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
  , namePattern :: Core.Maybe Types.NamePattern
    -- ^ For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
  , rulesPackageArns :: Core.Maybe [Types.Arn]
    -- ^ For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssessmentTemplateFilter' value with any optional fields omitted.
mkAssessmentTemplateFilter
    :: AssessmentTemplateFilter
mkAssessmentTemplateFilter
  = AssessmentTemplateFilter'{durationRange = Core.Nothing,
                              namePattern = Core.Nothing, rulesPackageArns = Core.Nothing}

-- | For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
--
-- /Note:/ Consider using 'durationRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfDurationRange :: Lens.Lens' AssessmentTemplateFilter (Core.Maybe Types.DurationRange)
atfDurationRange = Lens.field @"durationRange"
{-# INLINEABLE atfDurationRange #-}
{-# DEPRECATED durationRange "Use generic-lens or generic-optics with 'durationRange' instead"  #-}

-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
--
-- /Note:/ Consider using 'namePattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfNamePattern :: Lens.Lens' AssessmentTemplateFilter (Core.Maybe Types.NamePattern)
atfNamePattern = Lens.field @"namePattern"
{-# INLINEABLE atfNamePattern #-}
{-# DEPRECATED namePattern "Use generic-lens or generic-optics with 'namePattern' instead"  #-}

-- | For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfRulesPackageArns :: Lens.Lens' AssessmentTemplateFilter (Core.Maybe [Types.Arn])
atfRulesPackageArns = Lens.field @"rulesPackageArns"
{-# INLINEABLE atfRulesPackageArns #-}
{-# DEPRECATED rulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead"  #-}

instance Core.FromJSON AssessmentTemplateFilter where
        toJSON AssessmentTemplateFilter{..}
          = Core.object
              (Core.catMaybes
                 [("durationRange" Core..=) Core.<$> durationRange,
                  ("namePattern" Core..=) Core.<$> namePattern,
                  ("rulesPackageArns" Core..=) Core.<$> rulesPackageArns])
