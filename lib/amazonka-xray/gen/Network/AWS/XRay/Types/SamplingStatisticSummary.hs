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
    sssRequestCount,
    sssBorrowCount,
    sssRuleName,
    sssTimestamp,
    sssSampledCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Aggregated request sampling data for a sampling rule across all services for a 10-second window.
--
-- /See:/ 'mkSamplingStatisticSummary' smart constructor.
data SamplingStatisticSummary = SamplingStatisticSummary'
  { -- | The number of requests that matched the rule.
    requestCount :: Lude.Maybe Lude.Int,
    -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Lude.Maybe Lude.Int,
    -- | The name of the sampling rule.
    ruleName :: Lude.Maybe Lude.Text,
    -- | The start time of the reporting window.
    timestamp :: Lude.Maybe Lude.Timestamp,
    -- | The number of requests recorded.
    sampledCount :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingStatisticSummary' with the minimum fields required to make a request.
--
-- * 'requestCount' - The number of requests that matched the rule.
-- * 'borrowCount' - The number of requests recorded with borrowed reservoir quota.
-- * 'ruleName' - The name of the sampling rule.
-- * 'timestamp' - The start time of the reporting window.
-- * 'sampledCount' - The number of requests recorded.
mkSamplingStatisticSummary ::
  SamplingStatisticSummary
mkSamplingStatisticSummary =
  SamplingStatisticSummary'
    { requestCount = Lude.Nothing,
      borrowCount = Lude.Nothing,
      ruleName = Lude.Nothing,
      timestamp = Lude.Nothing,
      sampledCount = Lude.Nothing
    }

-- | The number of requests that matched the rule.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssRequestCount :: Lens.Lens' SamplingStatisticSummary (Lude.Maybe Lude.Int)
sssRequestCount = Lens.lens (requestCount :: SamplingStatisticSummary -> Lude.Maybe Lude.Int) (\s a -> s {requestCount = a} :: SamplingStatisticSummary)
{-# DEPRECATED sssRequestCount "Use generic-lens or generic-optics with 'requestCount' instead." #-}

-- | The number of requests recorded with borrowed reservoir quota.
--
-- /Note:/ Consider using 'borrowCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssBorrowCount :: Lens.Lens' SamplingStatisticSummary (Lude.Maybe Lude.Int)
sssBorrowCount = Lens.lens (borrowCount :: SamplingStatisticSummary -> Lude.Maybe Lude.Int) (\s a -> s {borrowCount = a} :: SamplingStatisticSummary)
{-# DEPRECATED sssBorrowCount "Use generic-lens or generic-optics with 'borrowCount' instead." #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssRuleName :: Lens.Lens' SamplingStatisticSummary (Lude.Maybe Lude.Text)
sssRuleName = Lens.lens (ruleName :: SamplingStatisticSummary -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: SamplingStatisticSummary)
{-# DEPRECATED sssRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The start time of the reporting window.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssTimestamp :: Lens.Lens' SamplingStatisticSummary (Lude.Maybe Lude.Timestamp)
sssTimestamp = Lens.lens (timestamp :: SamplingStatisticSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: SamplingStatisticSummary)
{-# DEPRECATED sssTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The number of requests recorded.
--
-- /Note:/ Consider using 'sampledCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSampledCount :: Lens.Lens' SamplingStatisticSummary (Lude.Maybe Lude.Int)
sssSampledCount = Lens.lens (sampledCount :: SamplingStatisticSummary -> Lude.Maybe Lude.Int) (\s a -> s {sampledCount = a} :: SamplingStatisticSummary)
{-# DEPRECATED sssSampledCount "Use generic-lens or generic-optics with 'sampledCount' instead." #-}

instance Lude.FromJSON SamplingStatisticSummary where
  parseJSON =
    Lude.withObject
      "SamplingStatisticSummary"
      ( \x ->
          SamplingStatisticSummary'
            Lude.<$> (x Lude..:? "RequestCount")
            Lude.<*> (x Lude..:? "BorrowCount")
            Lude.<*> (x Lude..:? "RuleName")
            Lude.<*> (x Lude..:? "Timestamp")
            Lude.<*> (x Lude..:? "SampledCount")
      )
