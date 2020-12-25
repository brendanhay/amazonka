{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStatisticSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStatisticSummary
  ( SamplingStatisticSummary (..),

    -- * Smart constructor
    mkSamplingStatisticSummary,

    -- * Lenses
    sssBorrowCount,
    sssRequestCount,
    sssRuleName,
    sssSampledCount,
    sssTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.String as Types

-- | Aggregated request sampling data for a sampling rule across all services for a 10-second window.
--
-- /See:/ 'mkSamplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Core.Maybe Core.Int,
    -- | The number of requests that matched the rule.
    requestCount :: Core.Maybe Core.Int,
    -- | The name of the sampling rule.
    ruleName :: Core.Maybe Types.String,
    -- | The number of requests recorded.
    sampledCount :: Core.Maybe Core.Int,
    -- | The start time of the reporting window.
    timestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SamplingStatisticSummary' value with any optional fields omitted.
mkSamplingStatisticSummary ::
  SamplingStatisticSummary
mkSamplingStatisticSummary =
  SamplingStatisticSummary'
    { borrowCount = Core.Nothing,
      requestCount = Core.Nothing,
      ruleName = Core.Nothing,
      sampledCount = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The number of requests recorded with borrowed reservoir quota.
--
-- /Note:/ Consider using 'borrowCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssBorrowCount :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Int)
sssBorrowCount = Lens.field @"borrowCount"
{-# DEPRECATED sssBorrowCount "Use generic-lens or generic-optics with 'borrowCount' instead." #-}

-- | The number of requests that matched the rule.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssRequestCount :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Int)
sssRequestCount = Lens.field @"requestCount"
{-# DEPRECATED sssRequestCount "Use generic-lens or generic-optics with 'requestCount' instead." #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssRuleName :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Types.String)
sssRuleName = Lens.field @"ruleName"
{-# DEPRECATED sssRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The number of requests recorded.
--
-- /Note:/ Consider using 'sampledCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSampledCount :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.Int)
sssSampledCount = Lens.field @"sampledCount"
{-# DEPRECATED sssSampledCount "Use generic-lens or generic-optics with 'sampledCount' instead." #-}

-- | The start time of the reporting window.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssTimestamp :: Lens.Lens' SamplingStatisticSummary (Core.Maybe Core.NominalDiffTime)
sssTimestamp = Lens.field @"timestamp"
{-# DEPRECATED sssTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON SamplingStatisticSummary where
  parseJSON =
    Core.withObject "SamplingStatisticSummary" Core.$
      \x ->
        SamplingStatisticSummary'
          Core.<$> (x Core..:? "BorrowCount")
          Core.<*> (x Core..:? "RequestCount")
          Core.<*> (x Core..:? "RuleName")
          Core.<*> (x Core..:? "SampledCount")
          Core.<*> (x Core..:? "Timestamp")
