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
-- Module      : Amazonka.EMRContainers.Types.RetryPolicyConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.RetryPolicyConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the retry policy that the job runs on.
--
-- /See:/ 'newRetryPolicyConfiguration' smart constructor.
data RetryPolicyConfiguration = RetryPolicyConfiguration'
  { -- | The maximum number of attempts on the job\'s driver.
    maxAttempts :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryPolicyConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAttempts', 'retryPolicyConfiguration_maxAttempts' - The maximum number of attempts on the job\'s driver.
newRetryPolicyConfiguration ::
  -- | 'maxAttempts'
  Prelude.Int ->
  RetryPolicyConfiguration
newRetryPolicyConfiguration pMaxAttempts_ =
  RetryPolicyConfiguration'
    { maxAttempts =
        pMaxAttempts_
    }

-- | The maximum number of attempts on the job\'s driver.
retryPolicyConfiguration_maxAttempts :: Lens.Lens' RetryPolicyConfiguration Prelude.Int
retryPolicyConfiguration_maxAttempts = Lens.lens (\RetryPolicyConfiguration' {maxAttempts} -> maxAttempts) (\s@RetryPolicyConfiguration' {} a -> s {maxAttempts = a} :: RetryPolicyConfiguration)

instance Data.FromJSON RetryPolicyConfiguration where
  parseJSON =
    Data.withObject
      "RetryPolicyConfiguration"
      ( \x ->
          RetryPolicyConfiguration'
            Prelude.<$> (x Data..: "maxAttempts")
      )

instance Prelude.Hashable RetryPolicyConfiguration where
  hashWithSalt _salt RetryPolicyConfiguration' {..} =
    _salt `Prelude.hashWithSalt` maxAttempts

instance Prelude.NFData RetryPolicyConfiguration where
  rnf RetryPolicyConfiguration' {..} =
    Prelude.rnf maxAttempts

instance Data.ToJSON RetryPolicyConfiguration where
  toJSON RetryPolicyConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("maxAttempts" Data..= maxAttempts)]
      )
