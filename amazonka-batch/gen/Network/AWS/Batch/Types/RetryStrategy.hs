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
-- Module      : Network.AWS.Batch.Types.RetryStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.RetryStrategy where

import Network.AWS.Batch.Types.EvaluateOnExit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The retry strategy associated with a job. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/job_retries.html Automated job retries>
-- in the /AWS Batch User Guide/.
--
-- /See:/ 'newRetryStrategy' smart constructor.
data RetryStrategy = RetryStrategy'
  { -- | Array of up to 5 objects that specify conditions under which the job
    -- should be retried or failed. If this parameter is specified, then the
    -- @attempts@ parameter must also be specified.
    evaluateOnExit :: Prelude.Maybe [EvaluateOnExit],
    -- | The number of times to move a job to the @RUNNABLE@ status. You can
    -- specify between 1 and 10 attempts. If the value of @attempts@ is greater
    -- than one, the job is retried on failure the same number of attempts as
    -- the value.
    attempts :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { evaluateOnExit = Prelude.Nothing,
      attempts = Prelude.Nothing
    }

-- | Array of up to 5 objects that specify conditions under which the job
-- should be retried or failed. If this parameter is specified, then the
-- @attempts@ parameter must also be specified.
retryStrategy_evaluateOnExit :: Lens.Lens' RetryStrategy (Prelude.Maybe [EvaluateOnExit])
retryStrategy_evaluateOnExit = Lens.lens (\RetryStrategy' {evaluateOnExit} -> evaluateOnExit) (\s@RetryStrategy' {} a -> s {evaluateOnExit = a} :: RetryStrategy) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of times to move a job to the @RUNNABLE@ status. You can
-- specify between 1 and 10 attempts. If the value of @attempts@ is greater
-- than one, the job is retried on failure the same number of attempts as
-- the value.
retryStrategy_attempts :: Lens.Lens' RetryStrategy (Prelude.Maybe Prelude.Int)
retryStrategy_attempts = Lens.lens (\RetryStrategy' {attempts} -> attempts) (\s@RetryStrategy' {} a -> s {attempts = a} :: RetryStrategy)

instance Prelude.FromJSON RetryStrategy where
  parseJSON =
    Prelude.withObject
      "RetryStrategy"
      ( \x ->
          RetryStrategy'
            Prelude.<$> ( x Prelude..:? "evaluateOnExit"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "attempts")
      )

instance Prelude.Hashable RetryStrategy

instance Prelude.NFData RetryStrategy

instance Prelude.ToJSON RetryStrategy where
  toJSON RetryStrategy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("evaluateOnExit" Prelude..=)
              Prelude.<$> evaluateOnExit,
            ("attempts" Prelude..=) Prelude.<$> attempts
          ]
      )
