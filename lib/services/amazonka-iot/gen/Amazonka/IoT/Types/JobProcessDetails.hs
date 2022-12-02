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
-- Module      : Amazonka.IoT.Types.JobProcessDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobProcessDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The job process details.
--
-- /See:/ 'newJobProcessDetails' smart constructor.
data JobProcessDetails = JobProcessDetails'
  { -- | The number of things which successfully completed the job.
    numberOfSucceededThings :: Prelude.Maybe Prelude.Int,
    -- | The number of things that failed executing the job.
    numberOfFailedThings :: Prelude.Maybe Prelude.Int,
    -- | The number of things that rejected the job.
    numberOfRejectedThings :: Prelude.Maybe Prelude.Int,
    -- | The number of things whose job execution status is @TIMED_OUT@.
    numberOfTimedOutThings :: Prelude.Maybe Prelude.Int,
    -- | The number of things that cancelled the job.
    numberOfCanceledThings :: Prelude.Maybe Prelude.Int,
    -- | The number of things currently executing the job.
    numberOfInProgressThings :: Prelude.Maybe Prelude.Int,
    -- | The target devices to which the job execution is being rolled out. This
    -- value will be null after the job execution has finished rolling out to
    -- all the target devices.
    processingTargets :: Prelude.Maybe [Prelude.Text],
    -- | The number of things that are no longer scheduled to execute the job
    -- because they have been deleted or have been removed from the group that
    -- was a target of the job.
    numberOfRemovedThings :: Prelude.Maybe Prelude.Int,
    -- | The number of things that are awaiting execution of the job.
    numberOfQueuedThings :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobProcessDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfSucceededThings', 'jobProcessDetails_numberOfSucceededThings' - The number of things which successfully completed the job.
--
-- 'numberOfFailedThings', 'jobProcessDetails_numberOfFailedThings' - The number of things that failed executing the job.
--
-- 'numberOfRejectedThings', 'jobProcessDetails_numberOfRejectedThings' - The number of things that rejected the job.
--
-- 'numberOfTimedOutThings', 'jobProcessDetails_numberOfTimedOutThings' - The number of things whose job execution status is @TIMED_OUT@.
--
-- 'numberOfCanceledThings', 'jobProcessDetails_numberOfCanceledThings' - The number of things that cancelled the job.
--
-- 'numberOfInProgressThings', 'jobProcessDetails_numberOfInProgressThings' - The number of things currently executing the job.
--
-- 'processingTargets', 'jobProcessDetails_processingTargets' - The target devices to which the job execution is being rolled out. This
-- value will be null after the job execution has finished rolling out to
-- all the target devices.
--
-- 'numberOfRemovedThings', 'jobProcessDetails_numberOfRemovedThings' - The number of things that are no longer scheduled to execute the job
-- because they have been deleted or have been removed from the group that
-- was a target of the job.
--
-- 'numberOfQueuedThings', 'jobProcessDetails_numberOfQueuedThings' - The number of things that are awaiting execution of the job.
newJobProcessDetails ::
  JobProcessDetails
newJobProcessDetails =
  JobProcessDetails'
    { numberOfSucceededThings =
        Prelude.Nothing,
      numberOfFailedThings = Prelude.Nothing,
      numberOfRejectedThings = Prelude.Nothing,
      numberOfTimedOutThings = Prelude.Nothing,
      numberOfCanceledThings = Prelude.Nothing,
      numberOfInProgressThings = Prelude.Nothing,
      processingTargets = Prelude.Nothing,
      numberOfRemovedThings = Prelude.Nothing,
      numberOfQueuedThings = Prelude.Nothing
    }

-- | The number of things which successfully completed the job.
jobProcessDetails_numberOfSucceededThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfSucceededThings = Lens.lens (\JobProcessDetails' {numberOfSucceededThings} -> numberOfSucceededThings) (\s@JobProcessDetails' {} a -> s {numberOfSucceededThings = a} :: JobProcessDetails)

-- | The number of things that failed executing the job.
jobProcessDetails_numberOfFailedThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfFailedThings = Lens.lens (\JobProcessDetails' {numberOfFailedThings} -> numberOfFailedThings) (\s@JobProcessDetails' {} a -> s {numberOfFailedThings = a} :: JobProcessDetails)

-- | The number of things that rejected the job.
jobProcessDetails_numberOfRejectedThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfRejectedThings = Lens.lens (\JobProcessDetails' {numberOfRejectedThings} -> numberOfRejectedThings) (\s@JobProcessDetails' {} a -> s {numberOfRejectedThings = a} :: JobProcessDetails)

-- | The number of things whose job execution status is @TIMED_OUT@.
jobProcessDetails_numberOfTimedOutThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfTimedOutThings = Lens.lens (\JobProcessDetails' {numberOfTimedOutThings} -> numberOfTimedOutThings) (\s@JobProcessDetails' {} a -> s {numberOfTimedOutThings = a} :: JobProcessDetails)

-- | The number of things that cancelled the job.
jobProcessDetails_numberOfCanceledThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfCanceledThings = Lens.lens (\JobProcessDetails' {numberOfCanceledThings} -> numberOfCanceledThings) (\s@JobProcessDetails' {} a -> s {numberOfCanceledThings = a} :: JobProcessDetails)

-- | The number of things currently executing the job.
jobProcessDetails_numberOfInProgressThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfInProgressThings = Lens.lens (\JobProcessDetails' {numberOfInProgressThings} -> numberOfInProgressThings) (\s@JobProcessDetails' {} a -> s {numberOfInProgressThings = a} :: JobProcessDetails)

-- | The target devices to which the job execution is being rolled out. This
-- value will be null after the job execution has finished rolling out to
-- all the target devices.
jobProcessDetails_processingTargets :: Lens.Lens' JobProcessDetails (Prelude.Maybe [Prelude.Text])
jobProcessDetails_processingTargets = Lens.lens (\JobProcessDetails' {processingTargets} -> processingTargets) (\s@JobProcessDetails' {} a -> s {processingTargets = a} :: JobProcessDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of things that are no longer scheduled to execute the job
-- because they have been deleted or have been removed from the group that
-- was a target of the job.
jobProcessDetails_numberOfRemovedThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfRemovedThings = Lens.lens (\JobProcessDetails' {numberOfRemovedThings} -> numberOfRemovedThings) (\s@JobProcessDetails' {} a -> s {numberOfRemovedThings = a} :: JobProcessDetails)

-- | The number of things that are awaiting execution of the job.
jobProcessDetails_numberOfQueuedThings :: Lens.Lens' JobProcessDetails (Prelude.Maybe Prelude.Int)
jobProcessDetails_numberOfQueuedThings = Lens.lens (\JobProcessDetails' {numberOfQueuedThings} -> numberOfQueuedThings) (\s@JobProcessDetails' {} a -> s {numberOfQueuedThings = a} :: JobProcessDetails)

instance Data.FromJSON JobProcessDetails where
  parseJSON =
    Data.withObject
      "JobProcessDetails"
      ( \x ->
          JobProcessDetails'
            Prelude.<$> (x Data..:? "numberOfSucceededThings")
            Prelude.<*> (x Data..:? "numberOfFailedThings")
            Prelude.<*> (x Data..:? "numberOfRejectedThings")
            Prelude.<*> (x Data..:? "numberOfTimedOutThings")
            Prelude.<*> (x Data..:? "numberOfCanceledThings")
            Prelude.<*> (x Data..:? "numberOfInProgressThings")
            Prelude.<*> ( x Data..:? "processingTargets"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "numberOfRemovedThings")
            Prelude.<*> (x Data..:? "numberOfQueuedThings")
      )

instance Prelude.Hashable JobProcessDetails where
  hashWithSalt _salt JobProcessDetails' {..} =
    _salt
      `Prelude.hashWithSalt` numberOfSucceededThings
      `Prelude.hashWithSalt` numberOfFailedThings
      `Prelude.hashWithSalt` numberOfRejectedThings
      `Prelude.hashWithSalt` numberOfTimedOutThings
      `Prelude.hashWithSalt` numberOfCanceledThings
      `Prelude.hashWithSalt` numberOfInProgressThings
      `Prelude.hashWithSalt` processingTargets
      `Prelude.hashWithSalt` numberOfRemovedThings
      `Prelude.hashWithSalt` numberOfQueuedThings

instance Prelude.NFData JobProcessDetails where
  rnf JobProcessDetails' {..} =
    Prelude.rnf numberOfSucceededThings
      `Prelude.seq` Prelude.rnf numberOfFailedThings
      `Prelude.seq` Prelude.rnf numberOfRejectedThings
      `Prelude.seq` Prelude.rnf numberOfTimedOutThings
      `Prelude.seq` Prelude.rnf numberOfCanceledThings
      `Prelude.seq` Prelude.rnf numberOfInProgressThings
      `Prelude.seq` Prelude.rnf processingTargets
      `Prelude.seq` Prelude.rnf numberOfRemovedThings
      `Prelude.seq` Prelude.rnf numberOfQueuedThings
