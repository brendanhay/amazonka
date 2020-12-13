{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription definition.
module Network.AWS.Greengrass.DeleteSubscriptionDefinition
  ( -- * Creating a request
    DeleteSubscriptionDefinition (..),
    mkDeleteSubscriptionDefinition,

    -- ** Request lenses
    dsdSubscriptionDefinitionId,

    -- * Destructuring the response
    DeleteSubscriptionDefinitionResponse (..),
    mkDeleteSubscriptionDefinitionResponse,

    -- ** Response lenses
    dsdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSubscriptionDefinition' smart constructor.
newtype DeleteSubscriptionDefinition = DeleteSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubscriptionDefinition' with the minimum fields required to make a request.
--
-- * 'subscriptionDefinitionId' - The ID of the subscription definition.
mkDeleteSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Lude.Text ->
  DeleteSubscriptionDefinition
mkDeleteSubscriptionDefinition pSubscriptionDefinitionId_ =
  DeleteSubscriptionDefinition'
    { subscriptionDefinitionId =
        pSubscriptionDefinitionId_
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdSubscriptionDefinitionId :: Lens.Lens' DeleteSubscriptionDefinition Lude.Text
dsdSubscriptionDefinitionId = Lens.lens (subscriptionDefinitionId :: DeleteSubscriptionDefinition -> Lude.Text) (\s a -> s {subscriptionDefinitionId = a} :: DeleteSubscriptionDefinition)
{-# DEPRECATED dsdSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

instance Lude.AWSRequest DeleteSubscriptionDefinition where
  type
    Rs DeleteSubscriptionDefinition =
      DeleteSubscriptionDefinitionResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteSubscriptionDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSubscriptionDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSubscriptionDefinition where
  toPath DeleteSubscriptionDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Lude.toBS subscriptionDefinitionId
      ]

instance Lude.ToQuery DeleteSubscriptionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSubscriptionDefinitionResponse' smart constructor.
newtype DeleteSubscriptionDefinitionResponse = DeleteSubscriptionDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSubscriptionDefinitionResponse
mkDeleteSubscriptionDefinitionResponse pResponseStatus_ =
  DeleteSubscriptionDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdrsResponseStatus :: Lens.Lens' DeleteSubscriptionDefinitionResponse Lude.Int
dsdrsResponseStatus = Lens.lens (responseStatus :: DeleteSubscriptionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSubscriptionDefinitionResponse)
{-# DEPRECATED dsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
