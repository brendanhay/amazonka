{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTemplateFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTemplateFilter
  ( AssessmentTemplateFilter (..),

    -- * Smart constructor
    mkAssessmentTemplateFilter,

    -- * Lenses
    atfNamePattern,
    atfRulesPackageARNs,
    atfDurationRange,
  )
where

import Network.AWS.Inspector.Types.DurationRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used as the request parameter in the 'ListAssessmentTemplates' action.
--
-- /See:/ 'mkAssessmentTemplateFilter' smart constructor.
data AssessmentTemplateFilter = AssessmentTemplateFilter'
  { namePattern ::
      Lude.Maybe Lude.Text,
    rulesPackageARNs ::
      Lude.Maybe [Lude.Text],
    durationRange :: Lude.Maybe DurationRange
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentTemplateFilter' with the minimum fields required to make a request.
--
-- * 'durationRange' - For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
-- * 'namePattern' - For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
-- * 'rulesPackageARNs' - For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
mkAssessmentTemplateFilter ::
  AssessmentTemplateFilter
mkAssessmentTemplateFilter =
  AssessmentTemplateFilter'
    { namePattern = Lude.Nothing,
      rulesPackageARNs = Lude.Nothing,
      durationRange = Lude.Nothing
    }

-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTemplateName__ property of the 'AssessmentTemplate' data type.
--
-- /Note:/ Consider using 'namePattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfNamePattern :: Lens.Lens' AssessmentTemplateFilter (Lude.Maybe Lude.Text)
atfNamePattern = Lens.lens (namePattern :: AssessmentTemplateFilter -> Lude.Maybe Lude.Text) (\s a -> s {namePattern = a} :: AssessmentTemplateFilter)
{-# DEPRECATED atfNamePattern "Use generic-lens or generic-optics with 'namePattern' instead." #-}

-- | For a record to match a filter, the values that are specified for this data type property must be contained in the list of values of the __rulesPackageArns__ property of the 'AssessmentTemplate' data type.
--
-- /Note:/ Consider using 'rulesPackageARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfRulesPackageARNs :: Lens.Lens' AssessmentTemplateFilter (Lude.Maybe [Lude.Text])
atfRulesPackageARNs = Lens.lens (rulesPackageARNs :: AssessmentTemplateFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {rulesPackageARNs = a} :: AssessmentTemplateFilter)
{-# DEPRECATED atfRulesPackageARNs "Use generic-lens or generic-optics with 'rulesPackageARNs' instead." #-}

-- | For a record to match a filter, the value specified for this data type property must inclusively match any value between the specified minimum and maximum values of the __durationInSeconds__ property of the 'AssessmentTemplate' data type.
--
-- /Note:/ Consider using 'durationRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfDurationRange :: Lens.Lens' AssessmentTemplateFilter (Lude.Maybe DurationRange)
atfDurationRange = Lens.lens (durationRange :: AssessmentTemplateFilter -> Lude.Maybe DurationRange) (\s a -> s {durationRange = a} :: AssessmentTemplateFilter)
{-# DEPRECATED atfDurationRange "Use generic-lens or generic-optics with 'durationRange' instead." #-}

instance Lude.ToJSON AssessmentTemplateFilter where
  toJSON AssessmentTemplateFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("namePattern" Lude..=) Lude.<$> namePattern,
            ("rulesPackageArns" Lude..=) Lude.<$> rulesPackageARNs,
            ("durationRange" Lude..=) Lude.<$> durationRange
          ]
      )
