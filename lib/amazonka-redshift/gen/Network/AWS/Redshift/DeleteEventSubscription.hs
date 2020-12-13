{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteEventSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift event notification subscription.
module Network.AWS.Redshift.DeleteEventSubscription
  ( -- * Creating a request
    DeleteEventSubscription (..),
    mkDeleteEventSubscription,

    -- ** Request lenses
    dSubscriptionName,

    -- * Destructuring the response
    DeleteEventSubscriptionResponse (..),
    mkDeleteEventSubscriptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { -- | The name of the Amazon Redshift event notification subscription to be deleted.
    subscriptionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionName' - The name of the Amazon Redshift event notification subscription to be deleted.
mkDeleteEventSubscription ::
  -- | 'subscriptionName'
  Lude.Text ->
  DeleteEventSubscription
mkDeleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription' {subscriptionName = pSubscriptionName_}

-- | The name of the Amazon Redshift event notification subscription to be deleted.
--
-- /Note:/ Consider using 'subscriptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubscriptionName :: Lens.Lens' DeleteEventSubscription Lude.Text
dSubscriptionName = Lens.lens (subscriptionName :: DeleteEventSubscription -> Lude.Text) (\s a -> s {subscriptionName = a} :: DeleteEventSubscription)
{-# DEPRECATED dSubscriptionName "Use generic-lens or generic-optics with 'subscriptionName' instead." #-}

instance Lude.AWSRequest DeleteEventSubscription where
  type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteEventSubscriptionResponse'

instance Lude.ToHeaders DeleteEventSubscription where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteEventSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEventSubscription where
  toQuery DeleteEventSubscription' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteEventSubscription" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SubscriptionName" Lude.=: subscriptionName
      ]

-- | /See:/ 'mkDeleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
mkDeleteEventSubscriptionResponse ::
  DeleteEventSubscriptionResponse
mkDeleteEventSubscriptionResponse =
  DeleteEventSubscriptionResponse'
