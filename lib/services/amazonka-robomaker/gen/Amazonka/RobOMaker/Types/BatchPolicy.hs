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
-- Module      : Amazonka.RobOMaker.Types.BatchPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.BatchPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the batch policy.
--
-- /See:/ 'newBatchPolicy' smart constructor.
data BatchPolicy = BatchPolicy'
  { -- | The number of active simulation jobs create as part of the batch that
    -- can be in an active state at the same time.
    --
    -- Active states include: @Pending@,@Preparing@, @Running@, @Restarting@,
    -- @RunningFailed@ and @Terminating@. All other states are terminal states.
    maxConcurrency :: Prelude.Maybe Prelude.Int,
    -- | The amount of time, in seconds, to wait for the batch to complete.
    --
    -- If a batch times out, and there are pending requests that were failing
    -- due to an internal failure (like @InternalServiceError@), they will be
    -- moved to the failed list and the batch status will be @Failed@. If the
    -- pending requests were failing for any other reason, the failed pending
    -- requests will be moved to the failed list and the batch status will be
    -- @TimedOut@.
    timeoutInSeconds :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrency', 'batchPolicy_maxConcurrency' - The number of active simulation jobs create as part of the batch that
-- can be in an active state at the same time.
--
-- Active states include: @Pending@,@Preparing@, @Running@, @Restarting@,
-- @RunningFailed@ and @Terminating@. All other states are terminal states.
--
-- 'timeoutInSeconds', 'batchPolicy_timeoutInSeconds' - The amount of time, in seconds, to wait for the batch to complete.
--
-- If a batch times out, and there are pending requests that were failing
-- due to an internal failure (like @InternalServiceError@), they will be
-- moved to the failed list and the batch status will be @Failed@. If the
-- pending requests were failing for any other reason, the failed pending
-- requests will be moved to the failed list and the batch status will be
-- @TimedOut@.
newBatchPolicy ::
  BatchPolicy
newBatchPolicy =
  BatchPolicy'
    { maxConcurrency = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing
    }

-- | The number of active simulation jobs create as part of the batch that
-- can be in an active state at the same time.
--
-- Active states include: @Pending@,@Preparing@, @Running@, @Restarting@,
-- @RunningFailed@ and @Terminating@. All other states are terminal states.
batchPolicy_maxConcurrency :: Lens.Lens' BatchPolicy (Prelude.Maybe Prelude.Int)
batchPolicy_maxConcurrency = Lens.lens (\BatchPolicy' {maxConcurrency} -> maxConcurrency) (\s@BatchPolicy' {} a -> s {maxConcurrency = a} :: BatchPolicy)

-- | The amount of time, in seconds, to wait for the batch to complete.
--
-- If a batch times out, and there are pending requests that were failing
-- due to an internal failure (like @InternalServiceError@), they will be
-- moved to the failed list and the batch status will be @Failed@. If the
-- pending requests were failing for any other reason, the failed pending
-- requests will be moved to the failed list and the batch status will be
-- @TimedOut@.
batchPolicy_timeoutInSeconds :: Lens.Lens' BatchPolicy (Prelude.Maybe Prelude.Integer)
batchPolicy_timeoutInSeconds = Lens.lens (\BatchPolicy' {timeoutInSeconds} -> timeoutInSeconds) (\s@BatchPolicy' {} a -> s {timeoutInSeconds = a} :: BatchPolicy)

instance Data.FromJSON BatchPolicy where
  parseJSON =
    Data.withObject
      "BatchPolicy"
      ( \x ->
          BatchPolicy'
            Prelude.<$> (x Data..:? "maxConcurrency")
            Prelude.<*> (x Data..:? "timeoutInSeconds")
      )

instance Prelude.Hashable BatchPolicy where
  hashWithSalt _salt BatchPolicy' {..} =
    _salt `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData BatchPolicy where
  rnf BatchPolicy' {..} =
    Prelude.rnf maxConcurrency
      `Prelude.seq` Prelude.rnf timeoutInSeconds

instance Data.ToJSON BatchPolicy where
  toJSON BatchPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxConcurrency" Data..=)
              Prelude.<$> maxConcurrency,
            ("timeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds
          ]
      )
