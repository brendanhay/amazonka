{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.CreateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a subscription to a cost anomaly detection monitor. You can use each subscription to define subscribers with email or SNS notifications. Email subscribers can set a dollar threshold and a time frequency for receiving notifications.
module Network.AWS.CostExplorer.CreateAnomalySubscription
  ( -- * Creating a request
    CreateAnomalySubscription (..),
    mkCreateAnomalySubscription,

    -- ** Request lenses
    casAnomalySubscription,

    -- * Destructuring the response
    CreateAnomalySubscriptionResponse (..),
    mkCreateAnomalySubscriptionResponse,

    -- ** Response lenses
    casrsSubscriptionARN,
    casrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAnomalySubscription' smart constructor.
newtype CreateAnomalySubscription = CreateAnomalySubscription'
  { -- | The cost anomaly subscription object that you want to create.
    anomalySubscription :: AnomalySubscription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAnomalySubscription' with the minimum fields required to make a request.
--
-- * 'anomalySubscription' - The cost anomaly subscription object that you want to create.
mkCreateAnomalySubscription ::
  -- | 'anomalySubscription'
  AnomalySubscription ->
  CreateAnomalySubscription
mkCreateAnomalySubscription pAnomalySubscription_ =
  CreateAnomalySubscription'
    { anomalySubscription =
        pAnomalySubscription_
    }

-- | The cost anomaly subscription object that you want to create.
--
-- /Note:/ Consider using 'anomalySubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casAnomalySubscription :: Lens.Lens' CreateAnomalySubscription AnomalySubscription
casAnomalySubscription = Lens.lens (anomalySubscription :: CreateAnomalySubscription -> AnomalySubscription) (\s a -> s {anomalySubscription = a} :: CreateAnomalySubscription)
{-# DEPRECATED casAnomalySubscription "Use generic-lens or generic-optics with 'anomalySubscription' instead." #-}

instance Lude.AWSRequest CreateAnomalySubscription where
  type
    Rs CreateAnomalySubscription =
      CreateAnomalySubscriptionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAnomalySubscriptionResponse'
            Lude.<$> (x Lude..:> "SubscriptionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAnomalySubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.CreateAnomalySubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAnomalySubscription where
  toJSON CreateAnomalySubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AnomalySubscription" Lude..= anomalySubscription)]
      )

instance Lude.ToPath CreateAnomalySubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAnomalySubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAnomalySubscriptionResponse' smart constructor.
data CreateAnomalySubscriptionResponse = CreateAnomalySubscriptionResponse'
  { -- | The unique identifier of your newly created cost anomaly subscription.
    subscriptionARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAnomalySubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'subscriptionARN' - The unique identifier of your newly created cost anomaly subscription.
-- * 'responseStatus' - The response status code.
mkCreateAnomalySubscriptionResponse ::
  -- | 'subscriptionARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateAnomalySubscriptionResponse
mkCreateAnomalySubscriptionResponse
  pSubscriptionARN_
  pResponseStatus_ =
    CreateAnomalySubscriptionResponse'
      { subscriptionARN =
          pSubscriptionARN_,
        responseStatus = pResponseStatus_
      }

-- | The unique identifier of your newly created cost anomaly subscription.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casrsSubscriptionARN :: Lens.Lens' CreateAnomalySubscriptionResponse Lude.Text
casrsSubscriptionARN = Lens.lens (subscriptionARN :: CreateAnomalySubscriptionResponse -> Lude.Text) (\s a -> s {subscriptionARN = a} :: CreateAnomalySubscriptionResponse)
{-# DEPRECATED casrsSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casrsResponseStatus :: Lens.Lens' CreateAnomalySubscriptionResponse Lude.Int
casrsResponseStatus = Lens.lens (responseStatus :: CreateAnomalySubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAnomalySubscriptionResponse)
{-# DEPRECATED casrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
