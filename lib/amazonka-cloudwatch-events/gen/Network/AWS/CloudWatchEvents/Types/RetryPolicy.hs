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
import qualified Network.AWS.Prelude as Lude

-- | A @RetryPolicy@ object that includes information about the retry policy settings.
--
-- /See:/ 'mkRetryPolicy' smart constructor.
data RetryPolicy = RetryPolicy'
  { maximumEventAgeInSeconds ::
      Lude.Maybe Lude.Natural,
    maximumRetryAttempts :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetryPolicy' with the minimum fields required to make a request.
--
-- * 'maximumEventAgeInSeconds' - The maximum amount of time, in seconds, to continue to make retry attempts.
-- * 'maximumRetryAttempts' - The maximum number of retry attempts to make before the request fails. Retry attempts continue until either the maximum number of attempts is made or until the duration of the @MaximumEventAgeInSeconds@ is met.
mkRetryPolicy ::
  RetryPolicy
mkRetryPolicy =
  RetryPolicy'
    { maximumEventAgeInSeconds = Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing
    }

-- | The maximum amount of time, in seconds, to continue to make retry attempts.
--
-- /Note:/ Consider using 'maximumEventAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMaximumEventAgeInSeconds :: Lens.Lens' RetryPolicy (Lude.Maybe Lude.Natural)
rpMaximumEventAgeInSeconds = Lens.lens (maximumEventAgeInSeconds :: RetryPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maximumEventAgeInSeconds = a} :: RetryPolicy)
{-# DEPRECATED rpMaximumEventAgeInSeconds "Use generic-lens or generic-optics with 'maximumEventAgeInSeconds' instead." #-}

-- | The maximum number of retry attempts to make before the request fails. Retry attempts continue until either the maximum number of attempts is made or until the duration of the @MaximumEventAgeInSeconds@ is met.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpMaximumRetryAttempts :: Lens.Lens' RetryPolicy (Lude.Maybe Lude.Natural)
rpMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: RetryPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maximumRetryAttempts = a} :: RetryPolicy)
{-# DEPRECATED rpMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

instance Lude.FromJSON RetryPolicy where
  parseJSON =
    Lude.withObject
      "RetryPolicy"
      ( \x ->
          RetryPolicy'
            Lude.<$> (x Lude..:? "MaximumEventAgeInSeconds")
            Lude.<*> (x Lude..:? "MaximumRetryAttempts")
      )

instance Lude.ToJSON RetryPolicy where
  toJSON RetryPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumEventAgeInSeconds" Lude..=)
              Lude.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Lude..=) Lude.<$> maximumRetryAttempts
          ]
      )
