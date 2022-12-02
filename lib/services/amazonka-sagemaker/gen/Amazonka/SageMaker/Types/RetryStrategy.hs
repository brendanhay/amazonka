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
-- Module      : Amazonka.SageMaker.Types.RetryStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RetryStrategy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The retry strategy to use when a training job fails due to an
-- @InternalServerError@. @RetryStrategy@ is specified as part of the
-- @CreateTrainingJob@ and @CreateHyperParameterTuningJob@ requests. You
-- can add the @StoppingCondition@ parameter to the request to limit the
-- training time for the complete job.
--
-- /See:/ 'newRetryStrategy' smart constructor.
data RetryStrategy = RetryStrategy'
  { -- | The number of times to retry the job. When the job is retried, it\'s
    -- @SecondaryStatus@ is changed to @STARTING@.
    maximumRetryAttempts :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumRetryAttempts', 'retryStrategy_maximumRetryAttempts' - The number of times to retry the job. When the job is retried, it\'s
-- @SecondaryStatus@ is changed to @STARTING@.
newRetryStrategy ::
  -- | 'maximumRetryAttempts'
  Prelude.Natural ->
  RetryStrategy
newRetryStrategy pMaximumRetryAttempts_ =
  RetryStrategy'
    { maximumRetryAttempts =
        pMaximumRetryAttempts_
    }

-- | The number of times to retry the job. When the job is retried, it\'s
-- @SecondaryStatus@ is changed to @STARTING@.
retryStrategy_maximumRetryAttempts :: Lens.Lens' RetryStrategy Prelude.Natural
retryStrategy_maximumRetryAttempts = Lens.lens (\RetryStrategy' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@RetryStrategy' {} a -> s {maximumRetryAttempts = a} :: RetryStrategy)

instance Data.FromJSON RetryStrategy where
  parseJSON =
    Data.withObject
      "RetryStrategy"
      ( \x ->
          RetryStrategy'
            Prelude.<$> (x Data..: "MaximumRetryAttempts")
      )

instance Prelude.Hashable RetryStrategy where
  hashWithSalt _salt RetryStrategy' {..} =
    _salt `Prelude.hashWithSalt` maximumRetryAttempts

instance Prelude.NFData RetryStrategy where
  rnf RetryStrategy' {..} =
    Prelude.rnf maximumRetryAttempts

instance Data.ToJSON RetryStrategy where
  toJSON RetryStrategy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MaximumRetryAttempts"
                  Data..= maximumRetryAttempts
              )
          ]
      )
