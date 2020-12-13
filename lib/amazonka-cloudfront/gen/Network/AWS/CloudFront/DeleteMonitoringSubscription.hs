{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables additional CloudWatch metrics for the specified CloudFront distribution.
module Network.AWS.CloudFront.DeleteMonitoringSubscription
  ( -- * Creating a request
    DeleteMonitoringSubscription (..),
    mkDeleteMonitoringSubscription,

    -- ** Request lenses
    dmsDistributionId,

    -- * Destructuring the response
    DeleteMonitoringSubscriptionResponse (..),
    mkDeleteMonitoringSubscriptionResponse,

    -- ** Response lenses
    dmsrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteMonitoringSubscription' smart constructor.
newtype DeleteMonitoringSubscription = DeleteMonitoringSubscription'
  { -- | The ID of the distribution that you are disabling metrics for.
    distributionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMonitoringSubscription' with the minimum fields required to make a request.
--
-- * 'distributionId' - The ID of the distribution that you are disabling metrics for.
mkDeleteMonitoringSubscription ::
  -- | 'distributionId'
  Lude.Text ->
  DeleteMonitoringSubscription
mkDeleteMonitoringSubscription pDistributionId_ =
  DeleteMonitoringSubscription' {distributionId = pDistributionId_}

-- | The ID of the distribution that you are disabling metrics for.
--
-- /Note:/ Consider using 'distributionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsDistributionId :: Lens.Lens' DeleteMonitoringSubscription Lude.Text
dmsDistributionId = Lens.lens (distributionId :: DeleteMonitoringSubscription -> Lude.Text) (\s a -> s {distributionId = a} :: DeleteMonitoringSubscription)
{-# DEPRECATED dmsDistributionId "Use generic-lens or generic-optics with 'distributionId' instead." #-}

instance Lude.AWSRequest DeleteMonitoringSubscription where
  type
    Rs DeleteMonitoringSubscription =
      DeleteMonitoringSubscriptionResponse
  request = Req.delete cloudFrontService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteMonitoringSubscriptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMonitoringSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteMonitoringSubscription where
  toPath DeleteMonitoringSubscription' {..} =
    Lude.mconcat
      [ "/2020-05-31/distributions/",
        Lude.toBS distributionId,
        "/monitoring-subscription"
      ]

instance Lude.ToQuery DeleteMonitoringSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMonitoringSubscriptionResponse' smart constructor.
newtype DeleteMonitoringSubscriptionResponse = DeleteMonitoringSubscriptionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMonitoringSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteMonitoringSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMonitoringSubscriptionResponse
mkDeleteMonitoringSubscriptionResponse pResponseStatus_ =
  DeleteMonitoringSubscriptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmsrsResponseStatus :: Lens.Lens' DeleteMonitoringSubscriptionResponse Lude.Int
dmsrsResponseStatus = Lens.lens (responseStatus :: DeleteMonitoringSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMonitoringSubscriptionResponse)
{-# DEPRECATED dmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
