{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a sampling quota for rules that the service is using to sample requests.
module Network.AWS.XRay.GetSamplingTargets
  ( -- * Creating a request
    GetSamplingTargets (..),
    mkGetSamplingTargets,

    -- ** Request lenses
    gstSamplingStatisticsDocuments,

    -- * Destructuring the response
    GetSamplingTargetsResponse (..),
    mkGetSamplingTargetsResponse,

    -- ** Response lenses
    gstrsUnprocessedStatistics,
    gstrsLastRuleModification,
    gstrsSamplingTargetDocuments,
    gstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetSamplingTargets' smart constructor.
newtype GetSamplingTargets = GetSamplingTargets'
  { samplingStatisticsDocuments ::
      [SamplingStatisticsDocument]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSamplingTargets' with the minimum fields required to make a request.
--
-- * 'samplingStatisticsDocuments' - Information about rules that the service is using to sample requests.
mkGetSamplingTargets ::
  GetSamplingTargets
mkGetSamplingTargets =
  GetSamplingTargets' {samplingStatisticsDocuments = Lude.mempty}

-- | Information about rules that the service is using to sample requests.
--
-- /Note:/ Consider using 'samplingStatisticsDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstSamplingStatisticsDocuments :: Lens.Lens' GetSamplingTargets [SamplingStatisticsDocument]
gstSamplingStatisticsDocuments = Lens.lens (samplingStatisticsDocuments :: GetSamplingTargets -> [SamplingStatisticsDocument]) (\s a -> s {samplingStatisticsDocuments = a} :: GetSamplingTargets)
{-# DEPRECATED gstSamplingStatisticsDocuments "Use generic-lens or generic-optics with 'samplingStatisticsDocuments' instead." #-}

instance Lude.AWSRequest GetSamplingTargets where
  type Rs GetSamplingTargets = GetSamplingTargetsResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSamplingTargetsResponse'
            Lude.<$> (x Lude..?> "UnprocessedStatistics" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "LastRuleModification")
            Lude.<*> (x Lude..?> "SamplingTargetDocuments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSamplingTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetSamplingTargets where
  toJSON GetSamplingTargets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "SamplingStatisticsDocuments"
                  Lude..= samplingStatisticsDocuments
              )
          ]
      )

instance Lude.ToPath GetSamplingTargets where
  toPath = Lude.const "/SamplingTargets"

instance Lude.ToQuery GetSamplingTargets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSamplingTargetsResponse' smart constructor.
data GetSamplingTargetsResponse = GetSamplingTargetsResponse'
  { unprocessedStatistics ::
      Lude.Maybe [UnprocessedStatistics],
    lastRuleModification ::
      Lude.Maybe Lude.Timestamp,
    samplingTargetDocuments ::
      Lude.Maybe [SamplingTargetDocument],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSamplingTargetsResponse' with the minimum fields required to make a request.
--
-- * 'lastRuleModification' - The last time a user changed the sampling rule configuration. If the sampling rule configuration changed since the service last retrieved it, the service should call 'GetSamplingRules' to get the latest version.
-- * 'responseStatus' - The response status code.
-- * 'samplingTargetDocuments' - Updated rules that the service should use to sample requests.
-- * 'unprocessedStatistics' - Information about 'SamplingStatisticsDocument' that X-Ray could not process.
mkGetSamplingTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSamplingTargetsResponse
mkGetSamplingTargetsResponse pResponseStatus_ =
  GetSamplingTargetsResponse'
    { unprocessedStatistics = Lude.Nothing,
      lastRuleModification = Lude.Nothing,
      samplingTargetDocuments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about 'SamplingStatisticsDocument' that X-Ray could not process.
--
-- /Note:/ Consider using 'unprocessedStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsUnprocessedStatistics :: Lens.Lens' GetSamplingTargetsResponse (Lude.Maybe [UnprocessedStatistics])
gstrsUnprocessedStatistics = Lens.lens (unprocessedStatistics :: GetSamplingTargetsResponse -> Lude.Maybe [UnprocessedStatistics]) (\s a -> s {unprocessedStatistics = a} :: GetSamplingTargetsResponse)
{-# DEPRECATED gstrsUnprocessedStatistics "Use generic-lens or generic-optics with 'unprocessedStatistics' instead." #-}

-- | The last time a user changed the sampling rule configuration. If the sampling rule configuration changed since the service last retrieved it, the service should call 'GetSamplingRules' to get the latest version.
--
-- /Note:/ Consider using 'lastRuleModification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsLastRuleModification :: Lens.Lens' GetSamplingTargetsResponse (Lude.Maybe Lude.Timestamp)
gstrsLastRuleModification = Lens.lens (lastRuleModification :: GetSamplingTargetsResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastRuleModification = a} :: GetSamplingTargetsResponse)
{-# DEPRECATED gstrsLastRuleModification "Use generic-lens or generic-optics with 'lastRuleModification' instead." #-}

-- | Updated rules that the service should use to sample requests.
--
-- /Note:/ Consider using 'samplingTargetDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsSamplingTargetDocuments :: Lens.Lens' GetSamplingTargetsResponse (Lude.Maybe [SamplingTargetDocument])
gstrsSamplingTargetDocuments = Lens.lens (samplingTargetDocuments :: GetSamplingTargetsResponse -> Lude.Maybe [SamplingTargetDocument]) (\s a -> s {samplingTargetDocuments = a} :: GetSamplingTargetsResponse)
{-# DEPRECATED gstrsSamplingTargetDocuments "Use generic-lens or generic-optics with 'samplingTargetDocuments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrsResponseStatus :: Lens.Lens' GetSamplingTargetsResponse Lude.Int
gstrsResponseStatus = Lens.lens (responseStatus :: GetSamplingTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSamplingTargetsResponse)
{-# DEPRECATED gstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
