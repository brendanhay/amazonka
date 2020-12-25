{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RetryPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RetryPolicy
  ( RetryPolicy (..),

    -- * Smart constructor
    mkRetryPolicy,

    -- * Lenses
    rpMaximumEventAgeInSeconds,
    rpMaximumRetryAttempts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A @RetryPolicy@ object that includes information about the retry policy settings.
--
-- /See:/ 'mkRetryPolicy' smart constructor.
data RetryPolicy = RetryPolicy'
  { -- | The maximum amount of time, in seconds, to continue to make retry attempts.
    maximumEventAgeInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of retry attempts to make before the request fails. Retry attempts continue until either the maximum number of attempts is made or until the duration of the @MaximumEventAgeInSeconds@ is met.
    maximumRetryAttempts :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RetryPolicy' value with any optional fields omitted.
mkRetryPolicy ::
  RetryPolicy
mkRetryPolicy =
  RetryPolicy'
    { maximumEventAgeInSeconds = Core.Nothing,
      maximumRetryAttempts = Core.Nothing
    }

-- | The maximum amount of time, in seconds, to continue to make retry attempts.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMaximumEventAgeInSeconds :: Lens.Lens' RetryPolicy (Core.Maybe Core.Natural)
rpMaximumEventAgeInSeconds = Lens.field @"maximumEventAgeInSeconds"
{-# DEPRECATED rpMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of retry attempts to make before the request fails. Retry attempts continue until either the maximum number of attempts is made or until the duration of the @MaximumEventAgeInSeconds@ is met.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMaximumRetryAttempts :: Lens.Lens' RetryPolicy (Core.Maybe Core.Natural)
rpMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# DEPRECATED rpMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

instance Core.FromJSON RetryPolicy where
  toJSON RetryPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaximumEventAgeInSeconds" Core..=)
              Core.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Core..=) Core.<$> maximumRetryAttempts
          ]
      )

instance Core.FromJSON RetryPolicy where
  parseJSON =
    Core.withObject "RetryPolicy" Core.$
      \x ->
        RetryPolicy'
          Core.<$> (x Core..:? "MaximumEventAgeInSeconds")
          Core.<*> (x Core..:? "MaximumRetryAttempts")
