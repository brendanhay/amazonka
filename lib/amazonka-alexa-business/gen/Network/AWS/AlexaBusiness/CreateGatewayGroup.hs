{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway group with the specified details.
module Network.AWS.AlexaBusiness.CreateGatewayGroup
  ( -- * Creating a request
    CreateGatewayGroup (..),
    mkCreateGatewayGroup,

    -- ** Request lenses
    cggName,
    cggClientRequestToken,
    cggDescription,

    -- * Destructuring the response
    CreateGatewayGroupResponse (..),
    mkCreateGatewayGroupResponse,

    -- ** Response lenses
    cggrsGatewayGroupARN,
    cggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGatewayGroup' smart constructor.
data CreateGatewayGroup = CreateGatewayGroup'
  { -- | The name of the gateway group.
    name :: Lude.Text,
    -- | A unique, user-specified identifier for the request that ensures idempotency.
    clientRequestToken :: Lude.Text,
    -- | The description of the gateway group.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGatewayGroup' with the minimum fields required to make a request.
--
-- * 'name' - The name of the gateway group.
-- * 'clientRequestToken' - A unique, user-specified identifier for the request that ensures idempotency.
-- * 'description' - The description of the gateway group.
mkCreateGatewayGroup ::
  -- | 'name'
  Lude.Text ->
  -- | 'clientRequestToken'
  Lude.Text ->
  CreateGatewayGroup
mkCreateGatewayGroup pName_ pClientRequestToken_ =
  CreateGatewayGroup'
    { name = pName_,
      clientRequestToken = pClientRequestToken_,
      description = Lude.Nothing
    }

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggName :: Lens.Lens' CreateGatewayGroup Lude.Text
cggName = Lens.lens (name :: CreateGatewayGroup -> Lude.Text) (\s a -> s {name = a} :: CreateGatewayGroup)
{-# DEPRECATED cggName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggClientRequestToken :: Lens.Lens' CreateGatewayGroup Lude.Text
cggClientRequestToken = Lens.lens (clientRequestToken :: CreateGatewayGroup -> Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateGatewayGroup)
{-# DEPRECATED cggClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggDescription :: Lens.Lens' CreateGatewayGroup (Lude.Maybe Lude.Text)
cggDescription = Lens.lens (description :: CreateGatewayGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateGatewayGroup)
{-# DEPRECATED cggDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateGatewayGroup where
  type Rs CreateGatewayGroup = CreateGatewayGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGatewayGroupResponse'
            Lude.<$> (x Lude..?> "GatewayGroupArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGatewayGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateGatewayGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGatewayGroup where
  toJSON CreateGatewayGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ClientRequestToken" Lude..= clientRequestToken),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateGatewayGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGatewayGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGatewayGroupResponse' smart constructor.
data CreateGatewayGroupResponse = CreateGatewayGroupResponse'
  { -- | The ARN of the created gateway group.
    gatewayGroupARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGatewayGroupResponse' with the minimum fields required to make a request.
--
-- * 'gatewayGroupARN' - The ARN of the created gateway group.
-- * 'responseStatus' - The response status code.
mkCreateGatewayGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGatewayGroupResponse
mkCreateGatewayGroupResponse pResponseStatus_ =
  CreateGatewayGroupResponse'
    { gatewayGroupARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the created gateway group.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggrsGatewayGroupARN :: Lens.Lens' CreateGatewayGroupResponse (Lude.Maybe Lude.Text)
cggrsGatewayGroupARN = Lens.lens (gatewayGroupARN :: CreateGatewayGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayGroupARN = a} :: CreateGatewayGroupResponse)
{-# DEPRECATED cggrsGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggrsResponseStatus :: Lens.Lens' CreateGatewayGroupResponse Lude.Int
cggrsResponseStatus = Lens.lens (responseStatus :: CreateGatewayGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGatewayGroupResponse)
{-# DEPRECATED cggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
