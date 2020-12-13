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
    ssdClientId,
    ssdRequestCount,
    ssdBorrowCount,
    ssdRuleName,
    ssdTimestamp,
    ssdSampledCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Request sampling results for a single rule from a service. Results are for the last 10 seconds unless the service has been assigned a longer reporting interval after a previous call to 'GetSamplingTargets' .
--
-- /See:/ 'mkSamplingStatisticsDocument' smart constructor.
data SamplingStatisticsDocument = SamplingStatisticsDocument'
  { -- | A unique identifier for the service in hexadecimal.
    clientId :: Lude.Text,
    -- | The number of requests that matched the rule.
    requestCount :: Lude.Natural,
    -- | The number of requests recorded with borrowed reservoir quota.
    borrowCount :: Lude.Maybe Lude.Natural,
    -- | The name of the sampling rule.
    ruleName :: Lude.Text,
    -- | The current time.
    timestamp :: Lude.Timestamp,
    -- | The number of requests recorded.
    sampledCount :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingStatisticsDocument' with the minimum fields required to make a request.
--
-- * 'clientId' - A unique identifier for the service in hexadecimal.
-- * 'requestCount' - The number of requests that matched the rule.
-- * 'borrowCount' - The number of requests recorded with borrowed reservoir quota.
-- * 'ruleName' - The name of the sampling rule.
-- * 'timestamp' - The current time.
-- * 'sampledCount' - The number of requests recorded.
mkSamplingStatisticsDocument ::
  -- | 'clientId'
  Lude.Text ->
  -- | 'requestCount'
  Lude.Natural ->
  -- | 'ruleName'
  Lude.Text ->
  -- | 'timestamp'
  Lude.Timestamp ->
  -- | 'sampledCount'
  Lude.Natural ->
  SamplingStatisticsDocument
mkSamplingStatisticsDocument
  pClientId_
  pRequestCount_
  pRuleName_
  pTimestamp_
  pSampledCount_ =
    SamplingStatisticsDocument'
      { clientId = pClientId_,
        requestCount = pRequestCount_,
        borrowCount = Lude.Nothing,
        ruleName = pRuleName_,
        timestamp = pTimestamp_,
        sampledCount = pSampledCount_
      }

-- | A unique identifier for the service in hexadecimal.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdClientId :: Lens.Lens' SamplingStatisticsDocument Lude.Text
ssdClientId = Lens.lens (clientId :: SamplingStatisticsDocument -> Lude.Text) (\s a -> s {clientId = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The number of requests that matched the rule.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdRequestCount :: Lens.Lens' SamplingStatisticsDocument Lude.Natural
ssdRequestCount = Lens.lens (requestCount :: SamplingStatisticsDocument -> Lude.Natural) (\s a -> s {requestCount = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdRequestCount "Use generic-lens or generic-optics with 'requestCount' instead." #-}

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

-- | The current time.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssdTimestamp :: Lens.Lens' SamplingStatisticsDocument Lude.Timestamp
ssdTimestamp = Lens.lens (timestamp :: SamplingStatisticsDocument -> Lude.Timestamp) (\s a -> s {timestamp = a} :: SamplingStatisticsDocument)
{-# DEPRECATED ssdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

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
          [ Lude.Just ("ClientID" Lude..= clientId),
            Lude.Just ("RequestCount" Lude..= requestCount),
            ("BorrowCount" Lude..=) Lude.<$> borrowCount,
            Lude.Just ("RuleName" Lude..= ruleName),
            Lude.Just ("Timestamp" Lude..= timestamp),
            Lude.Just ("SampledCount" Lude..= sampledCount)
          ]
      )
