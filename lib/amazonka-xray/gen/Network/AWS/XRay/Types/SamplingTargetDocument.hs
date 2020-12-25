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
    stdFixedRate,
    stdInterval,
    stdReservoirQuota,
    stdReservoirQuotaTTL,
    stdRuleName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.String as Types

-- | Temporary changes to a sampling rule configuration. To meet the global sampling target for a rule, X-Ray calculates a new reservoir for each service based on the recent sampling results of all services that called 'GetSamplingTargets' .
--
-- /See:/ 'mkSamplingTargetDocument' smart constructor.
data SamplingTargetDocument = SamplingTargetDocument'
  { -- | The percentage of matching requests to instrument, after the reservoir is exhausted.
    fixedRate :: Core.Maybe Core.Double,
    -- | The number of seconds for the service to wait before getting sampling targets again.
    interval :: Core.Maybe Core.Int,
    -- | The number of requests per second that X-Ray allocated for this service.
    reservoirQuota :: Core.Maybe Core.Int,
    -- | When the reservoir quota expires.
    reservoirQuotaTTL :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the sampling rule.
    ruleName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SamplingTargetDocument' value with any optional fields omitted.
mkSamplingTargetDocument ::
  SamplingTargetDocument
mkSamplingTargetDocument =
  SamplingTargetDocument'
    { fixedRate = Core.Nothing,
      interval = Core.Nothing,
      reservoirQuota = Core.Nothing,
      reservoirQuotaTTL = Core.Nothing,
      ruleName = Core.Nothing
    }

-- | The percentage of matching requests to instrument, after the reservoir is exhausted.
--
-- /Note:/ Consider using 'fixedRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdFixedRate :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Double)
stdFixedRate = Lens.field @"fixedRate"
{-# DEPRECATED stdFixedRate "Use generic-lens or generic-optics with 'fixedRate' instead." #-}

-- | The number of seconds for the service to wait before getting sampling targets again.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdInterval :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Int)
stdInterval = Lens.field @"interval"
{-# DEPRECATED stdInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

-- | The number of requests per second that X-Ray allocated for this service.
--
-- /Note:/ Consider using 'reservoirQuota' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdReservoirQuota :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.Int)
stdReservoirQuota = Lens.field @"reservoirQuota"
{-# DEPRECATED stdReservoirQuota "Use generic-lens or generic-optics with 'reservoirQuota' instead." #-}

-- | When the reservoir quota expires.
--
-- /Note:/ Consider using 'reservoirQuotaTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdReservoirQuotaTTL :: Lens.Lens' SamplingTargetDocument (Core.Maybe Core.NominalDiffTime)
stdReservoirQuotaTTL = Lens.field @"reservoirQuotaTTL"
{-# DEPRECATED stdReservoirQuotaTTL "Use generic-lens or generic-optics with 'reservoirQuotaTTL' instead." #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdRuleName :: Lens.Lens' SamplingTargetDocument (Core.Maybe Types.String)
stdRuleName = Lens.field @"ruleName"
{-# DEPRECATED stdRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

instance Core.FromJSON SamplingTargetDocument where
  parseJSON =
    Core.withObject "SamplingTargetDocument" Core.$
      \x ->
        SamplingTargetDocument'
          Core.<$> (x Core..:? "FixedRate")
          Core.<*> (x Core..:? "Interval")
          Core.<*> (x Core..:? "ReservoirQuota")
          Core.<*> (x Core..:? "ReservoirQuotaTTL")
          Core.<*> (x Core..:? "RuleName")
