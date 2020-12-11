{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteNotificationSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription from the specified organization.
module Network.AWS.WorkDocs.DeleteNotificationSubscription
  ( -- * Creating a request
    DeleteNotificationSubscription (..),
    mkDeleteNotificationSubscription,

    -- ** Request lenses
    dnsSubscriptionId,
    dnsOrganizationId,

    -- * Destructuring the response
    DeleteNotificationSubscriptionResponse (..),
    mkDeleteNotificationSubscriptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteNotificationSubscription' smart constructor.
data DeleteNotificationSubscription = DeleteNotificationSubscription'
  { subscriptionId ::
      Lude.Text,
    organizationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationSubscription' with the minimum fields required to make a request.
--
-- * 'organizationId' - The ID of the organization.
-- * 'subscriptionId' - The ID of the subscription.
mkDeleteNotificationSubscription ::
  -- | 'subscriptionId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteNotificationSubscription
mkDeleteNotificationSubscription pSubscriptionId_ pOrganizationId_ =
  DeleteNotificationSubscription'
    { subscriptionId =
        pSubscriptionId_,
      organizationId = pOrganizationId_
    }

-- | The ID of the subscription.
--
-- /Note:/ Consider using 'subscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsSubscriptionId :: Lens.Lens' DeleteNotificationSubscription Lude.Text
dnsSubscriptionId = Lens.lens (subscriptionId :: DeleteNotificationSubscription -> Lude.Text) (\s a -> s {subscriptionId = a} :: DeleteNotificationSubscription)
{-# DEPRECATED dnsSubscriptionId "Use generic-lens or generic-optics with 'subscriptionId' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsOrganizationId :: Lens.Lens' DeleteNotificationSubscription Lude.Text
dnsOrganizationId = Lens.lens (organizationId :: DeleteNotificationSubscription -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteNotificationSubscription)
{-# DEPRECATED dnsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteNotificationSubscription where
  type
    Rs DeleteNotificationSubscription =
      DeleteNotificationSubscriptionResponse
  request = Req.delete workDocsService
  response = Res.receiveNull DeleteNotificationSubscriptionResponse'

instance Lude.ToHeaders DeleteNotificationSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteNotificationSubscription where
  toPath DeleteNotificationSubscription' {..} =
    Lude.mconcat
      [ "/api/v1/organizations/",
        Lude.toBS organizationId,
        "/subscriptions/",
        Lude.toBS subscriptionId
      ]

instance Lude.ToQuery DeleteNotificationSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteNotificationSubscriptionResponse' smart constructor.
data DeleteNotificationSubscriptionResponse = DeleteNotificationSubscriptionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNotificationSubscriptionResponse' with the minimum fields required to make a request.
mkDeleteNotificationSubscriptionResponse ::
  DeleteNotificationSubscriptionResponse
mkDeleteNotificationSubscriptionResponse =
  DeleteNotificationSubscriptionResponse'
