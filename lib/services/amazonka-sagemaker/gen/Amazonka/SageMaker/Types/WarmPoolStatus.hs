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
-- Module      : Amazonka.SageMaker.Types.WarmPoolStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.WarmPoolStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.WarmPoolResourceStatus

-- | Status and billing information about the warm pool.
--
-- /See:/ 'newWarmPoolStatus' smart constructor.
data WarmPoolStatus = WarmPoolStatus'
  { -- | The billable time in seconds used by the warm pool. Billable time refers
    -- to the absolute wall-clock time.
    --
    -- Multiply @ResourceRetainedBillableTimeInSeconds@ by the number of
    -- instances (@InstanceCount@) in your training cluster to get the total
    -- compute time SageMaker bills you if you run warm pool training. The
    -- formula is as follows:
    -- @ResourceRetainedBillableTimeInSeconds * InstanceCount@.
    resourceRetainedBillableTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the matching training job that reused the warm pool.
    reusedByJob :: Prelude.Maybe Prelude.Text,
    -- | The status of the warm pool.
    --
    -- -   @InUse@: The warm pool is in use for the training job.
    --
    -- -   @Available@: The warm pool is available to reuse for a matching
    --     training job.
    --
    -- -   @Reused@: The warm pool moved to a matching training job for reuse.
    --
    -- -   @Terminated@: The warm pool is no longer available. Warm pools are
    --     unavailable if they are terminated by a user, terminated for a patch
    --     update, or terminated for exceeding the specified
    --     @KeepAlivePeriodInSeconds@.
    status :: WarmPoolResourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WarmPoolStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceRetainedBillableTimeInSeconds', 'warmPoolStatus_resourceRetainedBillableTimeInSeconds' - The billable time in seconds used by the warm pool. Billable time refers
-- to the absolute wall-clock time.
--
-- Multiply @ResourceRetainedBillableTimeInSeconds@ by the number of
-- instances (@InstanceCount@) in your training cluster to get the total
-- compute time SageMaker bills you if you run warm pool training. The
-- formula is as follows:
-- @ResourceRetainedBillableTimeInSeconds * InstanceCount@.
--
-- 'reusedByJob', 'warmPoolStatus_reusedByJob' - The name of the matching training job that reused the warm pool.
--
-- 'status', 'warmPoolStatus_status' - The status of the warm pool.
--
-- -   @InUse@: The warm pool is in use for the training job.
--
-- -   @Available@: The warm pool is available to reuse for a matching
--     training job.
--
-- -   @Reused@: The warm pool moved to a matching training job for reuse.
--
-- -   @Terminated@: The warm pool is no longer available. Warm pools are
--     unavailable if they are terminated by a user, terminated for a patch
--     update, or terminated for exceeding the specified
--     @KeepAlivePeriodInSeconds@.
newWarmPoolStatus ::
  -- | 'status'
  WarmPoolResourceStatus ->
  WarmPoolStatus
newWarmPoolStatus pStatus_ =
  WarmPoolStatus'
    { resourceRetainedBillableTimeInSeconds =
        Prelude.Nothing,
      reusedByJob = Prelude.Nothing,
      status = pStatus_
    }

-- | The billable time in seconds used by the warm pool. Billable time refers
-- to the absolute wall-clock time.
--
-- Multiply @ResourceRetainedBillableTimeInSeconds@ by the number of
-- instances (@InstanceCount@) in your training cluster to get the total
-- compute time SageMaker bills you if you run warm pool training. The
-- formula is as follows:
-- @ResourceRetainedBillableTimeInSeconds * InstanceCount@.
warmPoolStatus_resourceRetainedBillableTimeInSeconds :: Lens.Lens' WarmPoolStatus (Prelude.Maybe Prelude.Natural)
warmPoolStatus_resourceRetainedBillableTimeInSeconds = Lens.lens (\WarmPoolStatus' {resourceRetainedBillableTimeInSeconds} -> resourceRetainedBillableTimeInSeconds) (\s@WarmPoolStatus' {} a -> s {resourceRetainedBillableTimeInSeconds = a} :: WarmPoolStatus)

-- | The name of the matching training job that reused the warm pool.
warmPoolStatus_reusedByJob :: Lens.Lens' WarmPoolStatus (Prelude.Maybe Prelude.Text)
warmPoolStatus_reusedByJob = Lens.lens (\WarmPoolStatus' {reusedByJob} -> reusedByJob) (\s@WarmPoolStatus' {} a -> s {reusedByJob = a} :: WarmPoolStatus)

-- | The status of the warm pool.
--
-- -   @InUse@: The warm pool is in use for the training job.
--
-- -   @Available@: The warm pool is available to reuse for a matching
--     training job.
--
-- -   @Reused@: The warm pool moved to a matching training job for reuse.
--
-- -   @Terminated@: The warm pool is no longer available. Warm pools are
--     unavailable if they are terminated by a user, terminated for a patch
--     update, or terminated for exceeding the specified
--     @KeepAlivePeriodInSeconds@.
warmPoolStatus_status :: Lens.Lens' WarmPoolStatus WarmPoolResourceStatus
warmPoolStatus_status = Lens.lens (\WarmPoolStatus' {status} -> status) (\s@WarmPoolStatus' {} a -> s {status = a} :: WarmPoolStatus)

instance Data.FromJSON WarmPoolStatus where
  parseJSON =
    Data.withObject
      "WarmPoolStatus"
      ( \x ->
          WarmPoolStatus'
            Prelude.<$> (x Data..:? "ResourceRetainedBillableTimeInSeconds")
            Prelude.<*> (x Data..:? "ReusedByJob")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable WarmPoolStatus where
  hashWithSalt _salt WarmPoolStatus' {..} =
    _salt
      `Prelude.hashWithSalt` resourceRetainedBillableTimeInSeconds
      `Prelude.hashWithSalt` reusedByJob
      `Prelude.hashWithSalt` status

instance Prelude.NFData WarmPoolStatus where
  rnf WarmPoolStatus' {..} =
    Prelude.rnf resourceRetainedBillableTimeInSeconds
      `Prelude.seq` Prelude.rnf reusedByJob
      `Prelude.seq` Prelude.rnf status
