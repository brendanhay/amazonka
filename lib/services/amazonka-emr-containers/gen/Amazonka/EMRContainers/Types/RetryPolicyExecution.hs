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
-- Module      : Amazonka.EMRContainers.Types.RetryPolicyExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.RetryPolicyExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of the retry policy executed on the job.
--
-- /See:/ 'newRetryPolicyExecution' smart constructor.
data RetryPolicyExecution = RetryPolicyExecution'
  { -- | The current number of attempts made on the driver of the job.
    currentAttemptCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryPolicyExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentAttemptCount', 'retryPolicyExecution_currentAttemptCount' - The current number of attempts made on the driver of the job.
newRetryPolicyExecution ::
  -- | 'currentAttemptCount'
  Prelude.Int ->
  RetryPolicyExecution
newRetryPolicyExecution pCurrentAttemptCount_ =
  RetryPolicyExecution'
    { currentAttemptCount =
        pCurrentAttemptCount_
    }

-- | The current number of attempts made on the driver of the job.
retryPolicyExecution_currentAttemptCount :: Lens.Lens' RetryPolicyExecution Prelude.Int
retryPolicyExecution_currentAttemptCount = Lens.lens (\RetryPolicyExecution' {currentAttemptCount} -> currentAttemptCount) (\s@RetryPolicyExecution' {} a -> s {currentAttemptCount = a} :: RetryPolicyExecution)

instance Data.FromJSON RetryPolicyExecution where
  parseJSON =
    Data.withObject
      "RetryPolicyExecution"
      ( \x ->
          RetryPolicyExecution'
            Prelude.<$> (x Data..: "currentAttemptCount")
      )

instance Prelude.Hashable RetryPolicyExecution where
  hashWithSalt _salt RetryPolicyExecution' {..} =
    _salt `Prelude.hashWithSalt` currentAttemptCount

instance Prelude.NFData RetryPolicyExecution where
  rnf RetryPolicyExecution' {..} =
    Prelude.rnf currentAttemptCount
