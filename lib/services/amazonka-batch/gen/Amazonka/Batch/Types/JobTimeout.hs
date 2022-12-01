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
-- Module      : Amazonka.Batch.Types.JobTimeout
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JobTimeout where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a job timeout configuration.
--
-- /See:/ 'newJobTimeout' smart constructor.
data JobTimeout = JobTimeout'
  { -- | The job timeout time (in seconds) that\'s measured from the job
    -- attempt\'s @startedAt@ timestamp. After this time passes, Batch
    -- terminates your jobs if they aren\'t finished. The minimum value for the
    -- timeout is 60 seconds.
    attemptDurationSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobTimeout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attemptDurationSeconds', 'jobTimeout_attemptDurationSeconds' - The job timeout time (in seconds) that\'s measured from the job
-- attempt\'s @startedAt@ timestamp. After this time passes, Batch
-- terminates your jobs if they aren\'t finished. The minimum value for the
-- timeout is 60 seconds.
newJobTimeout ::
  JobTimeout
newJobTimeout =
  JobTimeout'
    { attemptDurationSeconds =
        Prelude.Nothing
    }

-- | The job timeout time (in seconds) that\'s measured from the job
-- attempt\'s @startedAt@ timestamp. After this time passes, Batch
-- terminates your jobs if they aren\'t finished. The minimum value for the
-- timeout is 60 seconds.
jobTimeout_attemptDurationSeconds :: Lens.Lens' JobTimeout (Prelude.Maybe Prelude.Int)
jobTimeout_attemptDurationSeconds = Lens.lens (\JobTimeout' {attemptDurationSeconds} -> attemptDurationSeconds) (\s@JobTimeout' {} a -> s {attemptDurationSeconds = a} :: JobTimeout)

instance Core.FromJSON JobTimeout where
  parseJSON =
    Core.withObject
      "JobTimeout"
      ( \x ->
          JobTimeout'
            Prelude.<$> (x Core..:? "attemptDurationSeconds")
      )

instance Prelude.Hashable JobTimeout where
  hashWithSalt _salt JobTimeout' {..} =
    _salt `Prelude.hashWithSalt` attemptDurationSeconds

instance Prelude.NFData JobTimeout where
  rnf JobTimeout' {..} =
    Prelude.rnf attemptDurationSeconds

instance Core.ToJSON JobTimeout where
  toJSON JobTimeout' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("attemptDurationSeconds" Core..=)
              Prelude.<$> attemptDurationSeconds
          ]
      )
