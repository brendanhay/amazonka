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
-- Module      : Amazonka.Scheduler.Types.RetryPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.RetryPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A @RetryPolicy@ object that includes information about the retry policy
-- settings, including the maximum age of an event, and the maximum number
-- of times EventBridge Scheduler will try to deliver the event to a
-- target.
--
-- /See:/ 'newRetryPolicy' smart constructor.
data RetryPolicy = RetryPolicy'
  { -- | The maximum amount of time, in seconds, to continue to make retry
    -- attempts.
    maximumEventAgeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of retry attempts to make before the request fails.
    -- Retry attempts with exponential backoff continue until either the
    -- maximum number of attempts is made or until the duration of the
    -- @MaximumEventAgeInSeconds@ is reached.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumEventAgeInSeconds', 'retryPolicy_maximumEventAgeInSeconds' - The maximum amount of time, in seconds, to continue to make retry
-- attempts.
--
-- 'maximumRetryAttempts', 'retryPolicy_maximumRetryAttempts' - The maximum number of retry attempts to make before the request fails.
-- Retry attempts with exponential backoff continue until either the
-- maximum number of attempts is made or until the duration of the
-- @MaximumEventAgeInSeconds@ is reached.
newRetryPolicy ::
  RetryPolicy
newRetryPolicy =
  RetryPolicy'
    { maximumEventAgeInSeconds =
        Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing
    }

-- | The maximum amount of time, in seconds, to continue to make retry
-- attempts.
retryPolicy_maximumEventAgeInSeconds :: Lens.Lens' RetryPolicy (Prelude.Maybe Prelude.Natural)
retryPolicy_maximumEventAgeInSeconds = Lens.lens (\RetryPolicy' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@RetryPolicy' {} a -> s {maximumEventAgeInSeconds = a} :: RetryPolicy)

-- | The maximum number of retry attempts to make before the request fails.
-- Retry attempts with exponential backoff continue until either the
-- maximum number of attempts is made or until the duration of the
-- @MaximumEventAgeInSeconds@ is reached.
retryPolicy_maximumRetryAttempts :: Lens.Lens' RetryPolicy (Prelude.Maybe Prelude.Natural)
retryPolicy_maximumRetryAttempts = Lens.lens (\RetryPolicy' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@RetryPolicy' {} a -> s {maximumRetryAttempts = a} :: RetryPolicy)

instance Data.FromJSON RetryPolicy where
  parseJSON =
    Data.withObject
      "RetryPolicy"
      ( \x ->
          RetryPolicy'
            Prelude.<$> (x Data..:? "MaximumEventAgeInSeconds")
            Prelude.<*> (x Data..:? "MaximumRetryAttempts")
      )

instance Prelude.Hashable RetryPolicy where
  hashWithSalt _salt RetryPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` maximumEventAgeInSeconds
      `Prelude.hashWithSalt` maximumRetryAttempts

instance Prelude.NFData RetryPolicy where
  rnf RetryPolicy' {..} =
    Prelude.rnf maximumEventAgeInSeconds `Prelude.seq`
      Prelude.rnf maximumRetryAttempts

instance Data.ToJSON RetryPolicy where
  toJSON RetryPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaximumEventAgeInSeconds" Data..=)
              Prelude.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Data..=)
              Prelude.<$> maximumRetryAttempts
          ]
      )
