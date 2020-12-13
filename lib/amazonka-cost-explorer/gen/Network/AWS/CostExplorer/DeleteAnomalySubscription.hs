{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DeleteAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cost anomaly subscription.
module Network.AWS.CostExplorer.DeleteAnomalySubscription
  ( -- * Creating a request
    DeleteAnomalySubscription (..),
    mkDeleteAnomalySubscription,

    -- ** Request lenses
    dasSubscriptionARN,

    -- * Destructuring the response
    DeleteAnomalySubscriptionResponse (..),
    mkDeleteAnomalySubscriptionResponse,

    -- ** Response lenses
    dasrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAnomalySubscription' smart constructor.
newtype DeleteAnomalySubscription = DeleteAnomalySubscription'
  { -- | The unique identifier of the cost anomaly subscription that you want to delete.
    subscriptionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnomalySubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionARN' - The unique identifier of the cost anomaly subscription that you want to delete.
mkDeleteAnomalySubscription ::
  -- | 'subscriptionARN'
  Lude.Text ->
  DeleteAnomalySubscription
mkDeleteAnomalySubscription pSubscriptionARN_ =
  DeleteAnomalySubscription' {subscriptionARN = pSubscriptionARN_}

-- | The unique identifier of the cost anomaly subscription that you want to delete.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasSubscriptionARN :: Lens.Lens' DeleteAnomalySubscription Lude.Text
dasSubscriptionARN = Lens.lens (subscriptionARN :: DeleteAnomalySubscription -> Lude.Text) (\s a -> s {subscriptionARN = a} :: DeleteAnomalySubscription)
{-# DEPRECATED dasSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

instance Lude.AWSRequest DeleteAnomalySubscription where
  type
    Rs DeleteAnomalySubscription =
      DeleteAnomalySubscriptionResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAnomalySubscriptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAnomalySubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.DeleteAnomalySubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAnomalySubscription where
  toJSON DeleteAnomalySubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SubscriptionArn" Lude..= subscriptionARN)]
      )

instance Lude.ToPath DeleteAnomalySubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAnomalySubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAnomalySubscriptionResponse' smart constructor.
newtype DeleteAnomalySubscriptionResponse = DeleteAnomalySubscriptionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAnomalySubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAnomalySubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAnomalySubscriptionResponse
mkDeleteAnomalySubscriptionResponse pResponseStatus_ =
  DeleteAnomalySubscriptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DeleteAnomalySubscriptionResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DeleteAnomalySubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAnomalySubscriptionResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
