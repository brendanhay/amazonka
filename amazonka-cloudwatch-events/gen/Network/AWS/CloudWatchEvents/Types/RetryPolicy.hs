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
-- Module      : Network.AWS.CloudWatchEvents.Types.RetryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RetryPolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A @RetryPolicy@ object that includes information about the retry policy
-- settings.
--
-- /See:/ 'newRetryPolicy' smart constructor.
data RetryPolicy = RetryPolicy'
  { -- | The maximum amount of time, in seconds, to continue to make retry
    -- attempts.
    maximumEventAgeInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of retry attempts to make before the request fails.
    -- Retry attempts continue until either the maximum number of attempts is
    -- made or until the duration of the @MaximumEventAgeInSeconds@ is met.
    maximumRetryAttempts :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- Retry attempts continue until either the maximum number of attempts is
-- made or until the duration of the @MaximumEventAgeInSeconds@ is met.
newRetryPolicy ::
  RetryPolicy
newRetryPolicy =
  RetryPolicy'
    { maximumEventAgeInSeconds =
        Core.Nothing,
      maximumRetryAttempts = Core.Nothing
    }

-- | The maximum amount of time, in seconds, to continue to make retry
-- attempts.
retryPolicy_maximumEventAgeInSeconds :: Lens.Lens' RetryPolicy (Core.Maybe Core.Natural)
retryPolicy_maximumEventAgeInSeconds = Lens.lens (\RetryPolicy' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@RetryPolicy' {} a -> s {maximumEventAgeInSeconds = a} :: RetryPolicy)

-- | The maximum number of retry attempts to make before the request fails.
-- Retry attempts continue until either the maximum number of attempts is
-- made or until the duration of the @MaximumEventAgeInSeconds@ is met.
retryPolicy_maximumRetryAttempts :: Lens.Lens' RetryPolicy (Core.Maybe Core.Natural)
retryPolicy_maximumRetryAttempts = Lens.lens (\RetryPolicy' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@RetryPolicy' {} a -> s {maximumRetryAttempts = a} :: RetryPolicy)

instance Core.FromJSON RetryPolicy where
  parseJSON =
    Core.withObject
      "RetryPolicy"
      ( \x ->
          RetryPolicy'
            Core.<$> (x Core..:? "MaximumEventAgeInSeconds")
            Core.<*> (x Core..:? "MaximumRetryAttempts")
      )

instance Core.Hashable RetryPolicy

instance Core.NFData RetryPolicy

instance Core.ToJSON RetryPolicy where
  toJSON RetryPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaximumEventAgeInSeconds" Core..=)
              Core.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Core..=)
              Core.<$> maximumRetryAttempts
          ]
      )
