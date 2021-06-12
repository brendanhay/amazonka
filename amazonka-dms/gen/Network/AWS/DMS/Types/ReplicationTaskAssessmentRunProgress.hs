{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The progress values reported by the @AssessmentProgress@ response
-- element.
--
-- /See:/ 'newReplicationTaskAssessmentRunProgress' smart constructor.
data ReplicationTaskAssessmentRunProgress = ReplicationTaskAssessmentRunProgress'
  { -- | The number of individual assessments that are specified to run.
    individualAssessmentCount :: Core.Maybe Core.Int,
    -- | The number of individual assessments that have completed, successfully
    -- or not.
    individualAssessmentCompletedCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationTaskAssessmentRunProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'individualAssessmentCount', 'replicationTaskAssessmentRunProgress_individualAssessmentCount' - The number of individual assessments that are specified to run.
--
-- 'individualAssessmentCompletedCount', 'replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount' - The number of individual assessments that have completed, successfully
-- or not.
newReplicationTaskAssessmentRunProgress ::
  ReplicationTaskAssessmentRunProgress
newReplicationTaskAssessmentRunProgress =
  ReplicationTaskAssessmentRunProgress'
    { individualAssessmentCount =
        Core.Nothing,
      individualAssessmentCompletedCount =
        Core.Nothing
    }

-- | The number of individual assessments that are specified to run.
replicationTaskAssessmentRunProgress_individualAssessmentCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Core.Maybe Core.Int)
replicationTaskAssessmentRunProgress_individualAssessmentCount = Lens.lens (\ReplicationTaskAssessmentRunProgress' {individualAssessmentCount} -> individualAssessmentCount) (\s@ReplicationTaskAssessmentRunProgress' {} a -> s {individualAssessmentCount = a} :: ReplicationTaskAssessmentRunProgress)

-- | The number of individual assessments that have completed, successfully
-- or not.
replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Core.Maybe Core.Int)
replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount = Lens.lens (\ReplicationTaskAssessmentRunProgress' {individualAssessmentCompletedCount} -> individualAssessmentCompletedCount) (\s@ReplicationTaskAssessmentRunProgress' {} a -> s {individualAssessmentCompletedCount = a} :: ReplicationTaskAssessmentRunProgress)

instance
  Core.FromJSON
    ReplicationTaskAssessmentRunProgress
  where
  parseJSON =
    Core.withObject
      "ReplicationTaskAssessmentRunProgress"
      ( \x ->
          ReplicationTaskAssessmentRunProgress'
            Core.<$> (x Core..:? "IndividualAssessmentCount")
            Core.<*> (x Core..:? "IndividualAssessmentCompletedCount")
      )

instance
  Core.Hashable
    ReplicationTaskAssessmentRunProgress

instance
  Core.NFData
    ReplicationTaskAssessmentRunProgress
