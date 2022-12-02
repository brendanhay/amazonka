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
-- Module      : Amazonka.DMS.Types.ReplicationTaskAssessmentRunProgress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationTaskAssessmentRunProgress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The progress values reported by the @AssessmentProgress@ response
-- element.
--
-- /See:/ 'newReplicationTaskAssessmentRunProgress' smart constructor.
data ReplicationTaskAssessmentRunProgress = ReplicationTaskAssessmentRunProgress'
  { -- | The number of individual assessments that have completed, successfully
    -- or not.
    individualAssessmentCompletedCount :: Prelude.Maybe Prelude.Int,
    -- | The number of individual assessments that are specified to run.
    individualAssessmentCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTaskAssessmentRunProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'individualAssessmentCompletedCount', 'replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount' - The number of individual assessments that have completed, successfully
-- or not.
--
-- 'individualAssessmentCount', 'replicationTaskAssessmentRunProgress_individualAssessmentCount' - The number of individual assessments that are specified to run.
newReplicationTaskAssessmentRunProgress ::
  ReplicationTaskAssessmentRunProgress
newReplicationTaskAssessmentRunProgress =
  ReplicationTaskAssessmentRunProgress'
    { individualAssessmentCompletedCount =
        Prelude.Nothing,
      individualAssessmentCount =
        Prelude.Nothing
    }

-- | The number of individual assessments that have completed, successfully
-- or not.
replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Prelude.Maybe Prelude.Int)
replicationTaskAssessmentRunProgress_individualAssessmentCompletedCount = Lens.lens (\ReplicationTaskAssessmentRunProgress' {individualAssessmentCompletedCount} -> individualAssessmentCompletedCount) (\s@ReplicationTaskAssessmentRunProgress' {} a -> s {individualAssessmentCompletedCount = a} :: ReplicationTaskAssessmentRunProgress)

-- | The number of individual assessments that are specified to run.
replicationTaskAssessmentRunProgress_individualAssessmentCount :: Lens.Lens' ReplicationTaskAssessmentRunProgress (Prelude.Maybe Prelude.Int)
replicationTaskAssessmentRunProgress_individualAssessmentCount = Lens.lens (\ReplicationTaskAssessmentRunProgress' {individualAssessmentCount} -> individualAssessmentCount) (\s@ReplicationTaskAssessmentRunProgress' {} a -> s {individualAssessmentCount = a} :: ReplicationTaskAssessmentRunProgress)

instance
  Data.FromJSON
    ReplicationTaskAssessmentRunProgress
  where
  parseJSON =
    Data.withObject
      "ReplicationTaskAssessmentRunProgress"
      ( \x ->
          ReplicationTaskAssessmentRunProgress'
            Prelude.<$> (x Data..:? "IndividualAssessmentCompletedCount")
            Prelude.<*> (x Data..:? "IndividualAssessmentCount")
      )

instance
  Prelude.Hashable
    ReplicationTaskAssessmentRunProgress
  where
  hashWithSalt
    _salt
    ReplicationTaskAssessmentRunProgress' {..} =
      _salt
        `Prelude.hashWithSalt` individualAssessmentCompletedCount
        `Prelude.hashWithSalt` individualAssessmentCount

instance
  Prelude.NFData
    ReplicationTaskAssessmentRunProgress
  where
  rnf ReplicationTaskAssessmentRunProgress' {..} =
    Prelude.rnf individualAssessmentCompletedCount
      `Prelude.seq` Prelude.rnf individualAssessmentCount
