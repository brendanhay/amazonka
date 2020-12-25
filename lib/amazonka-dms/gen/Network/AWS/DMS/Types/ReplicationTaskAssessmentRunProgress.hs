{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
  ( ReplicationTaskAssessmentRunProgress (..),

    -- * Smart constructor
    mkReplicationTaskAssessmentRunProgress,

    -- * Lenses
    rtarpIndividualAssessmentCompletedCount,
    rtarpIndividualAssessmentCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The progress values reported by the @AssessmentProgress@ response element.
--
-- /See:/ 'mkReplicationTaskAssessmentRunProgress' smart constructor.
data ReplicationTaskAssessmentRunProgress = ReplicationTaskAssessmentRunProgress'
  { -- | The number of individual assessments that have completed, successfully or not.
    individualAssessmentCompletedCount :: Core.Maybe Core.Int,
    -- | The number of individual assessments that are specified to run.
    individualAssessmentCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationTaskAssessmentRunProgress' value with any optional fields omitted.
mkReplicationTaskAssessmentRunProgress ::
  ReplicationTaskAssessmentRunProgress
mkReplicationTaskAssessmentRunProgress =
  ReplicationTaskAssessmentRunProgress'
    { individualAssessmentCompletedCount =
        Core.Nothing,
      individualAssessmentCount = Core.Nothing
    }

-- | The number of individual assessments that have completed, successfully or not.
--
-- /Note:/ Consider using 'individualAssessmentCompletedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarpIndividualAssessmentCompletedCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Core.Maybe Core.Int)
rtarpIndividualAssessmentCompletedCount = Lens.field @"individualAssessmentCompletedCount"
{-# DEPRECATED rtarpIndividualAssessmentCompletedCount "Use generic-lens or generic-optics with 'individualAssessmentCompletedCount' instead." #-}

-- | The number of individual assessments that are specified to run.
--
-- /Note:/ Consider using 'individualAssessmentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarpIndividualAssessmentCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Core.Maybe Core.Int)
rtarpIndividualAssessmentCount = Lens.field @"individualAssessmentCount"
{-# DEPRECATED rtarpIndividualAssessmentCount "Use generic-lens or generic-optics with 'individualAssessmentCount' instead." #-}

instance Core.FromJSON ReplicationTaskAssessmentRunProgress where
  parseJSON =
    Core.withObject "ReplicationTaskAssessmentRunProgress" Core.$
      \x ->
        ReplicationTaskAssessmentRunProgress'
          Core.<$> (x Core..:? "IndividualAssessmentCompletedCount")
          Core.<*> (x Core..:? "IndividualAssessmentCount")
