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
-- Module      : Amazonka.VoiceId.Types.JobProgress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.JobProgress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates the completion progress for a batch job.
--
-- /See:/ 'newJobProgress' smart constructor.
data JobProgress = JobProgress'
  { -- | Shows the completed percentage of enrollment or registration requests
    -- listed in the input file.
    percentComplete :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobProgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentComplete', 'jobProgress_percentComplete' - Shows the completed percentage of enrollment or registration requests
-- listed in the input file.
newJobProgress ::
  JobProgress
newJobProgress =
  JobProgress' {percentComplete = Prelude.Nothing}

-- | Shows the completed percentage of enrollment or registration requests
-- listed in the input file.
jobProgress_percentComplete :: Lens.Lens' JobProgress (Prelude.Maybe Prelude.Natural)
jobProgress_percentComplete = Lens.lens (\JobProgress' {percentComplete} -> percentComplete) (\s@JobProgress' {} a -> s {percentComplete = a} :: JobProgress)

instance Data.FromJSON JobProgress where
  parseJSON =
    Data.withObject
      "JobProgress"
      ( \x ->
          JobProgress'
            Prelude.<$> (x Data..:? "PercentComplete")
      )

instance Prelude.Hashable JobProgress where
  hashWithSalt _salt JobProgress' {..} =
    _salt `Prelude.hashWithSalt` percentComplete

instance Prelude.NFData JobProgress where
  rnf JobProgress' {..} = Prelude.rnf percentComplete
