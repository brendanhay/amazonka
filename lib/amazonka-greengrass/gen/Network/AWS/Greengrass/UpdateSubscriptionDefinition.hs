{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.UpdateSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscription definition.
module Network.AWS.Greengrass.UpdateSubscriptionDefinition
  ( -- * Creating a request
    UpdateSubscriptionDefinition (..),
    mkUpdateSubscriptionDefinition,

    -- ** Request lenses
    usdSubscriptionDefinitionId,
    usdName,

    -- * Destructuring the response
    UpdateSubscriptionDefinitionResponse (..),
    mkUpdateSubscriptionDefinitionResponse,

    -- ** Response lenses
    usdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Lude.Text,
    -- | The name of the definition.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSubscriptionDefinition' with the minimum fields required to make a request.
--
-- * 'subscriptionDefinitionId' - The ID of the subscription definition.
-- * 'name' - The name of the definition.
mkUpdateSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Lude.Text ->
  UpdateSubscriptionDefinition
mkUpdateSubscriptionDefinition pSubscriptionDefinitionId_ =
  UpdateSubscriptionDefinition'
    { subscriptionDefinitionId =
        pSubscriptionDefinitionId_,
      name = Lude.Nothing
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdSubscriptionDefinitionId :: Lens.Lens' UpdateSubscriptionDefinition Lude.Text
usdSubscriptionDefinitionId = Lens.lens (subscriptionDefinitionId :: UpdateSubscriptionDefinition -> Lude.Text) (\s a -> s {subscriptionDefinitionId = a} :: UpdateSubscriptionDefinition)
{-# DEPRECATED usdSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdName :: Lens.Lens' UpdateSubscriptionDefinition (Lude.Maybe Lude.Text)
usdName = Lens.lens (name :: UpdateSubscriptionDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateSubscriptionDefinition)
{-# DEPRECATED usdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateSubscriptionDefinition where
  type
    Rs UpdateSubscriptionDefinition =
      UpdateSubscriptionDefinitionResponse
  request = Req.putJSON greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionDefinitionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSubscriptionDefinition where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSubscriptionDefinition where
  toJSON UpdateSubscriptionDefinition' {..} =
    Lude.object (Lude.catMaybes [("Name" Lude..=) Lude.<$> name])

instance Lude.ToPath UpdateSubscriptionDefinition where
  toPath UpdateSubscriptionDefinition' {..} =
    Lude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Lude.toBS subscriptionDefinitionId
      ]

instance Lude.ToQuery UpdateSubscriptionDefinition where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSubscriptionDefinitionResponse' smart constructor.
newtype UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSubscriptionDefinitionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSubscriptionDefinitionResponse
mkUpdateSubscriptionDefinitionResponse pResponseStatus_ =
  UpdateSubscriptionDefinitionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usdrsResponseStatus :: Lens.Lens' UpdateSubscriptionDefinitionResponse Lude.Int
usdrsResponseStatus = Lens.lens (responseStatus :: UpdateSubscriptionDefinitionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSubscriptionDefinitionResponse)
{-# DEPRECATED usdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
