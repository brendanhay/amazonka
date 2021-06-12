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
-- Module      : Network.AWS.Batch.Types.RetryStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.RetryStrategy where

import Network.AWS.Batch.Types.EvaluateOnExit
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The retry strategy associated with a job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_retries.html Automated job retries>
-- in the /AWS Batch User Guide/.
--
-- /See:/ 'newRetryStrategy' smart constructor.
data RetryStrategy = RetryStrategy'
  { -- | Array of up to 5 objects that specify conditions under which the job
    -- should be retried or failed. If this parameter is specified, then the
    -- @attempts@ parameter must also be specified.
    evaluateOnExit :: Core.Maybe [EvaluateOnExit],
    -- | The number of times to move a job to the @RUNNABLE@ status. You can
    -- specify between 1 and 10 attempts. If the value of @attempts@ is greater
    -- than one, the job is retried on failure the same number of attempts as
    -- the value.
    attempts :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetryStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluateOnExit', 'retryStrategy_evaluateOnExit' - Array of up to 5 objects that specify conditions under which the job
-- should be retried or failed. If this parameter is specified, then the
-- @attempts@ parameter must also be specified.
--
-- 'attempts', 'retryStrategy_attempts' - The number of times to move a job to the @RUNNABLE@ status. You can
-- specify between 1 and 10 attempts. If the value of @attempts@ is greater
-- than one, the job is retried on failure the same number of attempts as
-- the value.
newRetryStrategy ::
  RetryStrategy
newRetryStrategy =
  RetryStrategy'
    { evaluateOnExit = Core.Nothing,
      attempts = Core.Nothing
    }

-- | Array of up to 5 objects that specify conditions under which the job
-- should be retried or failed. If this parameter is specified, then the
-- @attempts@ parameter must also be specified.
retryStrategy_evaluateOnExit :: Lens.Lens' RetryStrategy (Core.Maybe [EvaluateOnExit])
retryStrategy_evaluateOnExit = Lens.lens (\RetryStrategy' {evaluateOnExit} -> evaluateOnExit) (\s@RetryStrategy' {} a -> s {evaluateOnExit = a} :: RetryStrategy) Core.. Lens.mapping Lens._Coerce

-- | The number of times to move a job to the @RUNNABLE@ status. You can
-- specify between 1 and 10 attempts. If the value of @attempts@ is greater
-- than one, the job is retried on failure the same number of attempts as
-- the value.
retryStrategy_attempts :: Lens.Lens' RetryStrategy (Core.Maybe Core.Int)
retryStrategy_attempts = Lens.lens (\RetryStrategy' {attempts} -> attempts) (\s@RetryStrategy' {} a -> s {attempts = a} :: RetryStrategy)

instance Core.FromJSON RetryStrategy where
  parseJSON =
    Core.withObject
      "RetryStrategy"
      ( \x ->
          RetryStrategy'
            Core.<$> (x Core..:? "evaluateOnExit" Core..!= Core.mempty)
            Core.<*> (x Core..:? "attempts")
      )

instance Core.Hashable RetryStrategy

instance Core.NFData RetryStrategy

instance Core.ToJSON RetryStrategy where
  toJSON RetryStrategy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("evaluateOnExit" Core..=) Core.<$> evaluateOnExit,
            ("attempts" Core..=) Core.<$> attempts
          ]
      )
