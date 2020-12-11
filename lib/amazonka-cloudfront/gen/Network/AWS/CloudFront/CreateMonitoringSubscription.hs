{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables additional CloudWatch metrics for the specified CloudFront distribution. The additional metrics incur an additional cost.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/viewing-cloudfront-metrics.html#monitoring-console.distributions-additional Viewing additional CloudFront distribution metrics> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateMonitoringSubscription
  ( -- * Creating a request
    CreateMonitoringSubscription (..),
    mkCreateMonitoringSubscription,

    -- ** Request lenses
    cmsMonitoringSubscription,
    cmsDistributionId,

    -- * Destructuring the response
    CreateMonitoringSubscriptionResponse (..),
    mkCreateMonitoringSubscriptionResponse,

    -- ** Response lenses
    cmsrsMonitoringSubscription,
    cmsrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateMonitoringSubscription' smart constructor.
data CreateMonitoringSubscription = CreateMonitoringSubscription'
  { monitoringSubscription ::
      MonitoringSubscription,
    distributionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMonitoringSubscription' with the minimum fields required to make a request.
--
-- * 'distributionId' - The ID of the distribution that you are enabling metrics for.
-- * 'monitoringSubscription' - A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
mkCreateMonitoringSubscription ::
  -- | 'monitoringSubscription'
  MonitoringSubscription ->
  -- | 'distributionId'
  Lude.Text ->
  CreateMonitoringSubscription
mkCreateMonitoringSubscription
  pMonitoringSubscription_
  pDistributionId_ =
    CreateMonitoringSubscription'
      { monitoringSubscription =
          pMonitoringSubscription_,
        distributionId = pDistributionId_
      }

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsMonitoringSubscription :: Lens.Lens' CreateMonitoringSubscription MonitoringSubscription
cmsMonitoringSubscription = Lens.lens (monitoringSubscription :: CreateMonitoringSubscription -> MonitoringSubscription) (\s a -> s {monitoringSubscription = a} :: CreateMonitoringSubscription)
{-# DEPRECATED cmsMonitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead." #-}

-- | The ID of the distribution that you are enabling metrics for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsDistributionId :: Lens.Lens' CreateMonitoringSubscription Lude.Text
cmsDistributionId = Lens.lens (distributionId :: CreateMonitoringSubscription -> Lude.Text) (\s a -> s {distributionId = a} :: CreateMonitoringSubscription)
{-# DEPRECATED cmsDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

instance Lude.AWSRequest CreateMonitoringSubscription where
  type
    Rs CreateMonitoringSubscription =
      CreateMonitoringSubscriptionResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateMonitoringSubscriptionResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateMonitoringSubscription where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}MonitoringSubscription"
      Lude.. monitoringSubscription

instance Lude.ToHeaders CreateMonitoringSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateMonitoringSubscription where
  toPath CreateMonitoringSubscription' {..} =
    Lude.mconcat
      [ "/2020-05-31/distributions/",
        Lude.toBS distributionId,
        "/monitoring-subscription"
      ]

instance Lude.ToQuery CreateMonitoringSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMonitoringSubscriptionResponse' smart constructor.
data CreateMonitoringSubscriptionResponse = CreateMonitoringSubscriptionResponse'
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

-- | Creates a value of 'CreateMonitoringSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'monitoringSubscription' - A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
-- * 'responseStatus' - The response status code.
mkCreateMonitoringSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMonitoringSubscriptionResponse
mkCreateMonitoringSubscriptionResponse pResponseStatus_ =
  CreateMonitoringSubscriptionResponse'
    { monitoringSubscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'monitoringSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsrsMonitoringSubscription :: Lens.Lens' CreateMonitoringSubscriptionResponse (Lude.Maybe MonitoringSubscription)
cmsrsMonitoringSubscription = Lens.lens (monitoringSubscription :: CreateMonitoringSubscriptionResponse -> Lude.Maybe MonitoringSubscription) (\s a -> s {monitoringSubscription = a} :: CreateMonitoringSubscriptionResponse)
{-# DEPRECATED cmsrsMonitoringSubscription "Use generic-lens or generic-optics with 'monitoringSubscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmsrsResponseStatus :: Lens.Lens' CreateMonitoringSubscriptionResponse Lude.Int
cmsrsResponseStatus = Lens.lens (responseStatus :: CreateMonitoringSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMonitoringSubscriptionResponse)
{-# DEPRECATED cmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
