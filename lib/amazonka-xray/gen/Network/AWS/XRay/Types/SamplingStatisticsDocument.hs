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
    ssdBorrowCount,
    ssdRuleName,
    ssdClientId,
    ssdTimestamp,
    ssdRequestCount,
    ssdSampledCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Request sampling results for a single rule from a service. Results are for the last 10 seconds unless the service has been assigned a longer reporting interval after a previous call to 'GetSamplingTargets' .
--
-- /See:/ 'mkSamplingStatisticsDocument' smart constructor.
data SamplingStatisticsDocument = SamplingStatisticsDocument'
  { borrowCount ::
      Lude.Maybe Lude.Natural,
    ruleName :: Lude.Text,
    clientId :: Lude.Text,
    timestamp :: Lude.Timestamp,
    requestCount :: Lude.Natural,
    sampledCount :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingStatisticsDocument' with the minimum fields required to make a request.
--
-- * 'borrowCount' - The number of requests recorded with borrowed reservoir quota.
-- * 'clientId' - A unique identifier for the service in hexadecimal.
-- * 'requestCount' - The number of requests that matched the rule.
-- * 'ruleName' - The name of the sampling rule.
-- * 'sampledCount' - The number of requests recorded.
-- * 'timestamp' - The current time.
mkSamplingStatisticsDocument ::
  -- | 'ruleName'
  Lude.Text ->
  -- | 'clientId'
  Lude.Text ->
  -- | 'timestamp'
  Lude.Timestamp ->
  -- | 'requestCount'
  Lude.Natural ->
  -- | 'sampledCount'
  Lude.Natural ->
  SamplingStatisticsDocument
mkSamplingStatisticsDocument
  pRuleName_
  pClientId_
  pTimestamp_
  pRequestCount_
  pSampledCount_ =
    SamplingStatisticsDocument'
      { borrowCount = Lude.Nothing,
        ruleName = pRuleName_,
        clientId = pClientId_,
        timestamp = pTimestamp_,
        requestCount = pRequestCount_,
        sampledCount = pSampledCount_
      }

-- | The number of requests recorded with borrowed reservoir quota.
--
-- /Note:/ Consider using 'borrowCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdBorrowCount :: Lens.Lens' SamplingStatisticsDocument (Lude.Maybe Lude.Natural)
ssdBorrowCount = Lens.lens (borrowCount :: SamplingStatisticsDocument -> Lude.Maybe Lude.Natural) (\s a -> s {borrowCount = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdBorrowCount "Use generic-lens or generic-optics with 'borrowCount' instead." #-}

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdRuleName :: Lens.Lens' SamplingStatisticsDocument Lude.Text
ssdRuleName = Lens.lens (ruleName :: SamplingStatisticsDocument -> Lude.Text) (\s a -> s {ruleName = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | A unique identifier for the service in hexadecimal.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdClientId :: Lens.Lens' SamplingStatisticsDocument Lude.Text
ssdClientId = Lens.lens (clientId :: SamplingStatisticsDocument -> Lude.Text) (\s a -> s {clientId = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The current time.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdTimestamp :: Lens.Lens' SamplingStatisticsDocument Lude.Timestamp
ssdTimestamp = Lens.lens (timestamp :: SamplingStatisticsDocument -> Lude.Timestamp) (\s a -> s {timestamp = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The number of requests that matched the rule.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdRequestCount :: Lens.Lens' SamplingStatisticsDocument Lude.Natural
ssdRequestCount = Lens.lens (requestCount :: SamplingStatisticsDocument -> Lude.Natural) (\s a -> s {requestCount = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdRequestCount "Use generic-lens or generic-optics with 'requestCount' instead." #-}

-- | The number of requests recorded.
--
-- /Note:/ Consider using 'sampledCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdSampledCount :: Lens.Lens' SamplingStatisticsDocument Lude.Natural
ssdSampledCount = Lens.lens (sampledCount :: SamplingStatisticsDocument -> Lude.Natural) (\s a -> s {sampledCount = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdSampledCount "Use generic-lens or generic-optics with 'sampledCount' instead." #-}

instance Lude.ToJSON SamplingStatisticsDocument where
  toJSON SamplingStatisticsDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BorrowCount" Lude..=) Lude.<$> borrowCount,
            Lude.Just ("RuleName" Lude..= ruleName),
            Lude.Just ("ClientID" Lude..= clientId),
            Lude.Just ("Timestamp" Lude..= timestamp),
            Lude.Just ("RequestCount" Lude..= requestCount),
            Lude.Just ("SampledCount" Lude..= sampledCount)
          ]
      )
