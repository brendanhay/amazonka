{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether additional CloudWatch metrics are enabled for the specified CloudFront distribution.
module Network.AWS.CloudFront.GetMonitoringSubscription
  ( -- * Creating a request
    GetMonitoringSubscription (..),
    mkGetMonitoringSubscription,

    -- ** Request lenses
    gmsDistributionId,

    -- * Destructuring the response
    GetMonitoringSubscriptionResponse (..),
    mkGetMonitoringSubscriptionResponse,

    -- ** Response lenses
    gmsrsMonitoringSubscription,
    gmsrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMonitoringSubscription' smart constructor.
newtype GetMonitoringSubscription = GetMonitoringSubscription'
  { distributionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMonitoringSubscription' with the minimum fields required to make a request.
--
-- * 'distributionId' - The ID of the distribution that you are getting metrics information for.
mkGetMonitoringSubscription ::
  -- | 'distributionId'
  Lude.Text ->
  GetMonitoringSubscription
mkGetMonitoringSubscription pDistributionId_ =
  GetMonitoringSubscription' {distributionId = pDistributionId_}

-- | The ID of the distribution that you are getting metrics information for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsDistributionId :: Lens.Lens' GetMonitoringSubscription Lude.Text
gmsDistributionId = Lens.lens (distributionId :: GetMonitoringSubscription -> Lude.Text) (\s a -> s {distributionId = a} :: GetMonitoringSubscription)
{-# DEPRECATED gmsDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

instance Lude.AWSRequest GetMonitoringSubscription where
  type
    Rs GetMonitoringSubscription =
      GetMonitoringSubscriptionResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          GetMonitoringSubscriptionResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMonitoringSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetMonitoringSubscription where
  toPath GetMonitoringSubscription' {..} =
    Lude.mconcat
      [ "/2020-05-31/distributions/",
        Lude.toBS distributionId,
        "/monitoring-subscription"
      ]

instance Lude.ToQuery GetMonitoringSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMonitoringSubscriptionResponse' smart constructor.
data GetMonitoringSubscriptionResponse = GetMonitoringSubscriptionResponse'
  { monitoringSubscription ::
      Lude.Maybe
        MonitoringSubscription,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMonitoringSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'monitoringSubscription' - A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
-- * 'responseStatus' - The response status code.
mkGetMonitoringSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMonitoringSubscriptionResponse
mkGetMonitoringSubscriptionResponse pResponseStatus_ =
  GetMonitoringSubscriptionResponse'
    { monitoringSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrsMonitoringSubscription :: Lens.Lens' GetMonitoringSubscriptionResponse (Lude.Maybe MonitoringSubscription)
gmsrsMonitoringSubscription = Lens.lens (monitoringSubscription :: GetMonitoringSubscriptionResponse -> Lude.Maybe MonitoringSubscription) (\s a -> s {monitoringSubscription = a} :: GetMonitoringSubscriptionResponse)
{-# DEPRECATED gmsrsMonitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrsResponseStatus :: Lens.Lens' GetMonitoringSubscriptionResponse Lude.Int
gmsrsResponseStatus = Lens.lens (responseStatus :: GetMonitoringSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMonitoringSubscriptionResponse)
{-# DEPRECATED gmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
