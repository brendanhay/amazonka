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
    rtarpIndividualAssessmentCount,
    rtarpIndividualAssessmentCompletedCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The progress values reported by the @AssessmentProgress@ response element.
--
-- /See:/ 'mkReplicationTaskAssessmentRunProgress' smart constructor.
data ReplicationTaskAssessmentRunProgress = ReplicationTaskAssessmentRunProgress'
  { -- | The number of individual assessments that are specified to run.
    individualAssessmentCount :: Lude.Maybe Lude.Int,
    -- | The number of individual assessments that have completed, successfully or not.
    individualAssessmentCompletedCount :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTaskAssessmentRunProgress' with the minimum fields required to make a request.
--
-- * 'individualAssessmentCount' - The number of individual assessments that are specified to run.
-- * 'individualAssessmentCompletedCount' - The number of individual assessments that have completed, successfully or not.
mkReplicationTaskAssessmentRunProgress ::
  ReplicationTaskAssessmentRunProgress
mkReplicationTaskAssessmentRunProgress =
  ReplicationTaskAssessmentRunProgress'
    { individualAssessmentCount =
        Lude.Nothing,
      individualAssessmentCompletedCount = Lude.Nothing
    }

-- | The number of individual assessments that are specified to run.
--
-- /Note:/ Consider using 'individualAssessmentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarpIndividualAssessmentCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Lude.Maybe Lude.Int)
rtarpIndividualAssessmentCount = Lens.lens (individualAssessmentCount :: ReplicationTaskAssessmentRunProgress -> Lude.Maybe Lude.Int) (\s a -> s {individualAssessmentCount = a} :: ReplicationTaskAssessmentRunProgress)
{-# DEPRECATED rtarpIndividualAssessmentCount "Use generic-lens or generic-optics with 'individualAssessmentCount' instead." #-}

-- | The number of individual assessments that have completed, successfully or not.
--
-- /Note:/ Consider using 'individualAssessmentCompletedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtarpIndividualAssessmentCompletedCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Lude.Maybe Lude.Int)
rtarpIndividualAssessmentCompletedCount = Lens.lens (individualAssessmentCompletedCount :: ReplicationTaskAssessmentRunProgress -> Lude.Maybe Lude.Int) (\s a -> s {individualAssessmentCompletedCount = a} :: ReplicationTaskAssessmentRunProgress)
{-# DEPRECATED rtarpIndividualAssessmentCompletedCount "Use generic-lens or generic-optics with 'individualAssessmentCompletedCount' instead." #-}

instance Lude.FromJSON ReplicationTaskAssessmentRunProgress where
  parseJSON =
    Lude.withObject
      "ReplicationTaskAssessmentRunProgress"
      ( \x ->
          ReplicationTaskAssessmentRunProgress'
            Lude.<$> (x Lude..:? "IndividualAssessmentCount")
            Lude.<*> (x Lude..:? "IndividualAssessmentCompletedCount")
      )
