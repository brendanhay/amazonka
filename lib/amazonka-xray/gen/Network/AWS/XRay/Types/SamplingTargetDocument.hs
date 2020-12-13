{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingTargetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingTargetDocument
  ( SamplingTargetDocument (..),

    -- * Smart constructor
    mkSamplingTargetDocument,

    -- * Lenses
    stdReservoirQuota,
    stdRuleName,
    stdFixedRate,
    stdInterval,
    stdReservoirQuotaTTL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Temporary changes to a sampling rule configuration. To meet the global sampling target for a rule, X-Ray calculates a new reservoir for each service based on the recent sampling results of all services that called 'GetSamplingTargets' .
--
-- /See:/ 'mkSamplingTargetDocument' smart constructor.
data SamplingTargetDocument = SamplingTargetDocument'
  { -- | The number of requests per second that X-Ray allocated for this service.
    reservoirQuota :: Lude.Maybe Lude.Int,
    -- | The name of the sampling rule.
    ruleName :: Lude.Maybe Lude.Text,
    -- | The percentage of matching requests to instrument, after the reservoir is exhausted.
    fixedRate :: Lude.Maybe Lude.Double,
    -- | The number of seconds for the service to wait before getting sampling targets again.
    interval :: Lude.Maybe Lude.Int,
    -- | When the reservoir quota expires.
    reservoirQuotaTTL :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingTargetDocument' with the minimum fields required to make a request.
--
-- * 'reservoirQuota' - The number of requests per second that X-Ray allocated for this service.
-- * 'ruleName' - The name of the sampling rule.
-- * 'fixedRate' - The percentage of matching requests to instrument, after the reservoir is exhausted.
-- * 'interval' - The number of seconds for the service to wait before getting sampling targets again.
-- * 'reservoirQuotaTTL' - When the reservoir quota expires.
mkSamplingTargetDocument ::
  SamplingTargetDocument
mkSamplingTargetDocument =
  SamplingTargetDocument'
    { reservoirQuota = Lude.Nothing,
      ruleName = Lude.Nothing,
      fixedRate = Lude.Nothing,
      interval = Lude.Nothing,
      reservoirQuotaTTL = Lude.Nothing
    }

-- | The number of requests per second that X-Ray allocated for this service.
--
-- /Note:/ Consider using 'reservoirQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdReservoirQuota :: Lens.Lens' SamplingTargetDocument (Lude.Maybe Lude.Int)
stdReservoirQuota = Lens.lens (reservoirQuota :: SamplingTargetDocument -> Lude.Maybe Lude.Int) (\s a -> s {reservoirQuota = a} :: SamplingTargetDocument)
{-# DEPRECATED stdReservoirQuota "Use generic-lens or generic-optics with 'reservoirQuota' instead." #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdRuleName :: Lens.Lens' SamplingTargetDocument (Lude.Maybe Lude.Text)
stdRuleName = Lens.lens (ruleName :: SamplingTargetDocument -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: SamplingTargetDocument)
{-# DEPRECATED stdRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFixedRate :: Lens.Lens' SamplingTargetDocument (Lude.Maybe Lude.Double)
stdFixedRate = Lens.lens (fixedRate :: SamplingTargetDocument -> Lude.Maybe Lude.Double) (\s a -> s {fixedRate = a} :: SamplingTargetDocument)
{-# DEPRECATED stdFixedRate "Use generic-lens or generic-optics with 'fixedRate' instead." #-}

-- | The number of seconds for the service to wait before getting sampling targets again.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdInterval :: Lens.Lens' SamplingTargetDocument (Lude.Maybe Lude.Int)
stdInterval = Lens.lens (interval :: SamplingTargetDocument -> Lude.Maybe Lude.Int) (\s a -> s {interval = a} :: SamplingTargetDocument)
{-# DEPRECATED stdInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | When the reservoir quota expires.
--
-- /Note:/ Consider using 'reservoirQuotaTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdReservoirQuotaTTL :: Lens.Lens' SamplingTargetDocument (Lude.Maybe Lude.Timestamp)
stdReservoirQuotaTTL = Lens.lens (reservoirQuotaTTL :: SamplingTargetDocument -> Lude.Maybe Lude.Timestamp) (\s a -> s {reservoirQuotaTTL = a} :: SamplingTargetDocument)
{-# DEPRECATED stdReservoirQuotaTTL "Use generic-lens or generic-optics with 'reservoirQuotaTTL' instead." #-}

instance Lude.FromJSON SamplingTargetDocument where
  parseJSON =
    Lude.withObject
      "SamplingTargetDocument"
      ( \x ->
          SamplingTargetDocument'
            Lude.<$> (x Lude..:? "ReservoirQuota")
            Lude.<*> (x Lude..:? "RuleName")
            Lude.<*> (x Lude..:? "FixedRate")
            Lude.<*> (x Lude..:? "Interval")
            Lude.<*> (x Lude..:? "ReservoirQuotaTTL")
      )
