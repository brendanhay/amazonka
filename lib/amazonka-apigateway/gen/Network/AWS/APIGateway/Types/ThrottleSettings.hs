{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.ThrottleSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ThrottleSettings
  ( ThrottleSettings (..),

    -- * Smart constructor
    mkThrottleSettings,

    -- * Lenses
    tsBurstLimit,
    tsRateLimit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The API request rate limits.
--
-- /See:/ 'mkThrottleSettings' smart constructor.
data ThrottleSettings = ThrottleSettings'
  { burstLimit ::
      Lude.Maybe Lude.Int,
    rateLimit :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThrottleSettings' with the minimum fields required to make a request.
--
-- * 'burstLimit' - The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
-- * 'rateLimit' - The API request steady-state rate limit.
mkThrottleSettings ::
  ThrottleSettings
mkThrottleSettings =
  ThrottleSettings'
    { burstLimit = Lude.Nothing,
      rateLimit = Lude.Nothing
    }

-- | The API request burst limit, the maximum rate limit over a time ranging from one to a few seconds, depending upon whether the underlying token bucket is at its full capacity.
--
-- /Note:/ Consider using 'burstLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsBurstLimit :: Lens.Lens' ThrottleSettings (Lude.Maybe Lude.Int)
tsBurstLimit = Lens.lens (burstLimit :: ThrottleSettings -> Lude.Maybe Lude.Int) (\s a -> s {burstLimit = a} :: ThrottleSettings)
{-# DEPRECATED tsBurstLimit "Use generic-lens or generic-optics with 'burstLimit' instead." #-}

-- | The API request steady-state rate limit.
--
-- /Note:/ Consider using 'rateLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsRateLimit :: Lens.Lens' ThrottleSettings (Lude.Maybe Lude.Double)
tsRateLimit = Lens.lens (rateLimit :: ThrottleSettings -> Lude.Maybe Lude.Double) (\s a -> s {rateLimit = a} :: ThrottleSettings)
{-# DEPRECATED tsRateLimit "Use generic-lens or generic-optics with 'rateLimit' instead." #-}

instance Lude.FromJSON ThrottleSettings where
  parseJSON =
    Lude.withObject
      "ThrottleSettings"
      ( \x ->
          ThrottleSettings'
            Lude.<$> (x Lude..:? "burstLimit") Lude.<*> (x Lude..:? "rateLimit")
      )

instance Lude.ToJSON ThrottleSettings where
  toJSON ThrottleSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("burstLimit" Lude..=) Lude.<$> burstLimit,
            ("rateLimit" Lude..=) Lude.<$> rateLimit
          ]
      )
