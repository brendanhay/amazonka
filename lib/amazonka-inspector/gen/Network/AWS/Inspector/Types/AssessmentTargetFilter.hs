-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentTargetFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentTargetFilter
  ( AssessmentTargetFilter (..),

    -- * Smart constructor
    mkAssessmentTargetFilter,

    -- * Lenses
    atfAssessmentTargetNamePattern,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used as the request parameter in the 'ListAssessmentTargets' action.
--
-- /See:/ 'mkAssessmentTargetFilter' smart constructor.
newtype AssessmentTargetFilter = AssessmentTargetFilter'
  { assessmentTargetNamePattern ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssessmentTargetFilter' with the minimum fields required to make a request.
--
-- * 'assessmentTargetNamePattern' - For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
mkAssessmentTargetFilter ::
  AssessmentTargetFilter
mkAssessmentTargetFilter =
  AssessmentTargetFilter'
    { assessmentTargetNamePattern =
        Lude.Nothing
    }

-- | For a record to match a filter, an explicit value or a string that contains a wildcard that is specified for this data type property must match the value of the __assessmentTargetName__ property of the 'AssessmentTarget' data type.
--
-- /Note:/ Consider using 'assessmentTargetNamePattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atfAssessmentTargetNamePattern :: Lens.Lens' AssessmentTargetFilter (Lude.Maybe Lude.Text)
atfAssessmentTargetNamePattern = Lens.lens (assessmentTargetNamePattern :: AssessmentTargetFilter -> Lude.Maybe Lude.Text) (\s a -> s {assessmentTargetNamePattern = a} :: AssessmentTargetFilter)
{-# DEPRECATED atfAssessmentTargetNamePattern "Use generic-lens or generic-optics with 'assessmentTargetNamePattern' instead." #-}

instance Lude.ToJSON AssessmentTargetFilter where
  toJSON AssessmentTargetFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("assessmentTargetNamePattern" Lude..=)
              Lude.<$> assessmentTargetNamePattern
          ]
      )
