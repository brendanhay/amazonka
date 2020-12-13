{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EdgeStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EdgeStatistics
  ( EdgeStatistics (..),

    -- * Smart constructor
    mkEdgeStatistics,

    -- * Lenses
    esFaultStatistics,
    esOKCount,
    esTotalResponseTime,
    esErrorStatistics,
    esTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ErrorStatistics
import Network.AWS.XRay.Types.FaultStatistics

-- | Response statistics for an edge.
--
-- /See:/ 'mkEdgeStatistics' smart constructor.
data EdgeStatistics = EdgeStatistics'
  { -- | Information about requests that failed with a 5xx Server Error status code.
    faultStatistics :: Lude.Maybe FaultStatistics,
    -- | The number of requests that completed with a 2xx Success status code.
    okCount :: Lude.Maybe Lude.Integer,
    -- | The aggregate response time of completed requests.
    totalResponseTime :: Lude.Maybe Lude.Double,
    -- | Information about requests that failed with a 4xx Client Error status code.
    errorStatistics :: Lude.Maybe ErrorStatistics,
    -- | The total number of completed requests.
    totalCount :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EdgeStatistics' with the minimum fields required to make a request.
--
-- * 'faultStatistics' - Information about requests that failed with a 5xx Server Error status code.
-- * 'okCount' - The number of requests that completed with a 2xx Success status code.
-- * 'totalResponseTime' - The aggregate response time of completed requests.
-- * 'errorStatistics' - Information about requests that failed with a 4xx Client Error status code.
-- * 'totalCount' - The total number of completed requests.
mkEdgeStatistics ::
  EdgeStatistics
mkEdgeStatistics =
  EdgeStatistics'
    { faultStatistics = Lude.Nothing,
      okCount = Lude.Nothing,
      totalResponseTime = Lude.Nothing,
      errorStatistics = Lude.Nothing,
      totalCount = Lude.Nothing
    }

-- | Information about requests that failed with a 5xx Server Error status code.
--
-- /Note:/ Consider using 'faultStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esFaultStatistics :: Lens.Lens' EdgeStatistics (Lude.Maybe FaultStatistics)
esFaultStatistics = Lens.lens (faultStatistics :: EdgeStatistics -> Lude.Maybe FaultStatistics) (\s a -> s {faultStatistics = a} :: EdgeStatistics)
{-# DEPRECATED esFaultStatistics "Use generic-lens or generic-optics with 'faultStatistics' instead." #-}

-- | The number of requests that completed with a 2xx Success status code.
--
-- /Note:/ Consider using 'okCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esOKCount :: Lens.Lens' EdgeStatistics (Lude.Maybe Lude.Integer)
esOKCount = Lens.lens (okCount :: EdgeStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {okCount = a} :: EdgeStatistics)
{-# DEPRECATED esOKCount "Use generic-lens or generic-optics with 'okCount' instead." #-}

-- | The aggregate response time of completed requests.
--
-- /Note:/ Consider using 'totalResponseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esTotalResponseTime :: Lens.Lens' EdgeStatistics (Lude.Maybe Lude.Double)
esTotalResponseTime = Lens.lens (totalResponseTime :: EdgeStatistics -> Lude.Maybe Lude.Double) (\s a -> s {totalResponseTime = a} :: EdgeStatistics)
{-# DEPRECATED esTotalResponseTime "Use generic-lens or generic-optics with 'totalResponseTime' instead." #-}

-- | Information about requests that failed with a 4xx Client Error status code.
--
-- /Note:/ Consider using 'errorStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esErrorStatistics :: Lens.Lens' EdgeStatistics (Lude.Maybe ErrorStatistics)
esErrorStatistics = Lens.lens (errorStatistics :: EdgeStatistics -> Lude.Maybe ErrorStatistics) (\s a -> s {errorStatistics = a} :: EdgeStatistics)
{-# DEPRECATED esErrorStatistics "Use generic-lens or generic-optics with 'errorStatistics' instead." #-}

-- | The total number of completed requests.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esTotalCount :: Lens.Lens' EdgeStatistics (Lude.Maybe Lude.Integer)
esTotalCount = Lens.lens (totalCount :: EdgeStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {totalCount = a} :: EdgeStatistics)
{-# DEPRECATED esTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Lude.FromJSON EdgeStatistics where
  parseJSON =
    Lude.withObject
      "EdgeStatistics"
      ( \x ->
          EdgeStatistics'
            Lude.<$> (x Lude..:? "FaultStatistics")
            Lude.<*> (x Lude..:? "OkCount")
            Lude.<*> (x Lude..:? "TotalResponseTime")
            Lude.<*> (x Lude..:? "ErrorStatistics")
            Lude.<*> (x Lude..:? "TotalCount")
      )
