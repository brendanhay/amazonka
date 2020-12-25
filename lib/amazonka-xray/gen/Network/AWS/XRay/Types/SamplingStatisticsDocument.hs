{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStatisticsDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStatisticsDocument
  ( SamplingStatisticsDocument (..),

    -- * Smart constructor
    mkSamplingStatisticsDocument,

    -- * Lenses
    ssdRuleName,
    ssdClientID,
    ssdTimestamp,
    ssdRequestCount,
    ssdSampledCount,
    ssdBorrowCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ClientID as Types
import qualified Network.AWS.XRay.Types.RuleName as Types

-- | Request sampling results for a single rule from a service. Results are for the last 10 seconds unless the service has been assigned a longer reporting interval after a previous call to 'GetSamplingTargets' .
--
-- /See:/ 'mkSamplingStatisticsDocument' smart constructor.
data SamplingStatisticsDocument = SamplingStatisticsDocument'
  { -- | The name of the sampling rule.
    ruleName :: Types.RuleName,
    -- | A unique identifier for the service in hexadecimal.
    clientID :: Types.ClientID,
    -- | The current time.
    timestamp :: Core.NominalDiffTime,
    -- | The number of requests that matched the rule.
    requestCount :: Core.Natural,
    -- | The number of requests recorded.
    sampledCount :: Core.Natural,
    -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SamplingStatisticsDocument' value with any optional fields omitted.
mkSamplingStatisticsDocument ::
  -- | 'ruleName'
  Types.RuleName ->
  -- | 'clientID'
  Types.ClientID ->
  -- | 'timestamp'
  Core.NominalDiffTime ->
  -- | 'requestCount'
  Core.Natural ->
  -- | 'sampledCount'
  Core.Natural ->
  SamplingStatisticsDocument
mkSamplingStatisticsDocument
  ruleName
  clientID
  timestamp
  requestCount
  sampledCount =
    SamplingStatisticsDocument'
      { ruleName,
        clientID,
        timestamp,
        requestCount,
        sampledCount,
        borrowCount = Core.Nothing
      }

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdRuleName :: Lens.Lens' SamplingStatisticsDocument Types.RuleName
ssdRuleName = Lens.field @"ruleName"
{-# DEPRECATED ssdRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | A unique identifier for the service in hexadecimal.
--
-- /Note:/ Consider using 'clientID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdClientID :: Lens.Lens' SamplingStatisticsDocument Types.ClientID
ssdClientID = Lens.field @"clientID"
{-# DEPRECATED ssdClientID "Use generic-lens or generic-optics with 'clientID' instead." #-}

-- | The current time.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdTimestamp :: Lens.Lens' SamplingStatisticsDocument Core.NominalDiffTime
ssdTimestamp = Lens.field @"timestamp"
{-# DEPRECATED ssdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The number of requests that matched the rule.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdRequestCount :: Lens.Lens' SamplingStatisticsDocument Core.Natural
ssdRequestCount = Lens.field @"requestCount"
{-# DEPRECATED ssdRequestCount "Use generic-lens or generic-optics with 'requestCount' instead." #-}

-- | The number of requests recorded.
--
-- /Note:/ Consider using 'sampledCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSampledCount :: Lens.Lens' SamplingStatisticsDocument Core.Natural
ssdSampledCount = Lens.field @"sampledCount"
{-# DEPRECATED ssdSampledCount "Use generic-lens or generic-optics with 'sampledCount' instead." #-}

-- | The number of requests recorded with borrowed reservoir quota.
--
-- /Note:/ Consider using 'borrowCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdBorrowCount :: Lens.Lens' SamplingStatisticsDocument (Core.Maybe Core.Natural)
ssdBorrowCount = Lens.field @"borrowCount"
{-# DEPRECATED ssdBorrowCount "Use generic-lens or generic-optics with 'borrowCount' instead." #-}

instance Core.FromJSON SamplingStatisticsDocument where
  toJSON SamplingStatisticsDocument {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RuleName" Core..= ruleName),
            Core.Just ("ClientID" Core..= clientID),
            Core.Just ("Timestamp" Core..= timestamp),
            Core.Just ("RequestCount" Core..= requestCount),
            Core.Just ("SampledCount" Core..= sampledCount),
            ("BorrowCount" Core..=) Core.<$> borrowCount
          ]
      )
