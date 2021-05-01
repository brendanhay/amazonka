{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.Types.JobTimeout
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobTimeout where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a job timeout configuration.
--
-- /See:/ 'newJobTimeout' smart constructor.
data JobTimeout = JobTimeout'
  { -- | The time duration in seconds (measured from the job attempt\'s
    -- @startedAt@ timestamp) after which AWS Batch terminates your jobs if
    -- they have not finished. The minimum value for the timeout is 60 seconds.
    attemptDurationSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobTimeout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attemptDurationSeconds', 'jobTimeout_attemptDurationSeconds' - The time duration in seconds (measured from the job attempt\'s
-- @startedAt@ timestamp) after which AWS Batch terminates your jobs if
-- they have not finished. The minimum value for the timeout is 60 seconds.
newJobTimeout ::
  JobTimeout
newJobTimeout =
  JobTimeout'
    { attemptDurationSeconds =
        Prelude.Nothing
    }

-- | The time duration in seconds (measured from the job attempt\'s
-- @startedAt@ timestamp) after which AWS Batch terminates your jobs if
-- they have not finished. The minimum value for the timeout is 60 seconds.
jobTimeout_attemptDurationSeconds :: Lens.Lens' JobTimeout (Prelude.Maybe Prelude.Int)
jobTimeout_attemptDurationSeconds = Lens.lens (\JobTimeout' {attemptDurationSeconds} -> attemptDurationSeconds) (\s@JobTimeout' {} a -> s {attemptDurationSeconds = a} :: JobTimeout)

instance Prelude.FromJSON JobTimeout where
  parseJSON =
    Prelude.withObject
      "JobTimeout"
      ( \x ->
          JobTimeout'
            Prelude.<$> (x Prelude..:? "attemptDurationSeconds")
      )

instance Prelude.Hashable JobTimeout

instance Prelude.NFData JobTimeout

instance Prelude.ToJSON JobTimeout where
  toJSON JobTimeout' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("attemptDurationSeconds" Prelude..=)
              Prelude.<$> attemptDurationSeconds
          ]
      )
