-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.QuotaSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.QuotaSettings
  ( QuotaSettings (..),

    -- * Smart constructor
    mkQuotaSettings,

    -- * Lenses
    qsOffset,
    qsPeriod,
    qsLimit,
  )
where

import Network.AWS.APIGateway.Types.QuotaPeriodType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Quotas configured for a usage plan.
--
-- /See:/ 'mkQuotaSettings' smart constructor.
data QuotaSettings = QuotaSettings'
  { offset :: Lude.Maybe Lude.Int,
    period :: Lude.Maybe QuotaPeriodType,
    limit :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QuotaSettings' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of requests that can be made in a given time period.
-- * 'offset' - The number of requests subtracted from the given limit in the initial time period.
-- * 'period' - The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
mkQuotaSettings ::
  QuotaSettings
mkQuotaSettings =
  QuotaSettings'
    { offset = Lude.Nothing,
      period = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The number of requests subtracted from the given limit in the initial time period.
--
-- /Note:/ Consider using 'offset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsOffset :: Lens.Lens' QuotaSettings (Lude.Maybe Lude.Int)
qsOffset = Lens.lens (offset :: QuotaSettings -> Lude.Maybe Lude.Int) (\s a -> s {offset = a} :: QuotaSettings)
{-# DEPRECATED qsOffset "Use generic-lens or generic-optics with 'offset' instead." #-}

-- | The time period in which the limit applies. Valid values are "DAY", "WEEK" or "MONTH".
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsPeriod :: Lens.Lens' QuotaSettings (Lude.Maybe QuotaPeriodType)
qsPeriod = Lens.lens (period :: QuotaSettings -> Lude.Maybe QuotaPeriodType) (\s a -> s {period = a} :: QuotaSettings)
{-# DEPRECATED qsPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The maximum number of requests that can be made in a given time period.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsLimit :: Lens.Lens' QuotaSettings (Lude.Maybe Lude.Int)
qsLimit = Lens.lens (limit :: QuotaSettings -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: QuotaSettings)
{-# DEPRECATED qsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Lude.FromJSON QuotaSettings where
  parseJSON =
    Lude.withObject
      "QuotaSettings"
      ( \x ->
          QuotaSettings'
            Lude.<$> (x Lude..:? "offset")
            Lude.<*> (x Lude..:? "period")
            Lude.<*> (x Lude..:? "limit")
      )

instance Lude.ToJSON QuotaSettings where
  toJSON QuotaSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("offset" Lude..=) Lude.<$> offset,
            ("period" Lude..=) Lude.<$> period,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )
