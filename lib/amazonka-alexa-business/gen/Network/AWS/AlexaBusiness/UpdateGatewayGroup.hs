{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway group. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGatewayGroup
  ( -- * Creating a request
    UpdateGatewayGroup (..),
    mkUpdateGatewayGroup,

    -- ** Request lenses
    uggName,
    uggGatewayGroupARN,
    uggDescription,

    -- * Destructuring the response
    UpdateGatewayGroupResponse (..),
    mkUpdateGatewayGroupResponse,

    -- ** Response lenses
    uggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGatewayGroup' smart constructor.
data UpdateGatewayGroup = UpdateGatewayGroup'
  { -- | The updated name of the gateway group.
    name :: Lude.Maybe Lude.Text,
    -- | The ARN of the gateway group to update.
    gatewayGroupARN :: Lude.Text,
    -- | The updated description of the gateway group.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGatewayGroup' with the minimum fields required to make a request.
--
-- * 'name' - The updated name of the gateway group.
-- * 'gatewayGroupARN' - The ARN of the gateway group to update.
-- * 'description' - The updated description of the gateway group.
mkUpdateGatewayGroup ::
  -- | 'gatewayGroupARN'
  Lude.Text ->
  UpdateGatewayGroup
mkUpdateGatewayGroup pGatewayGroupARN_ =
  UpdateGatewayGroup'
    { name = Lude.Nothing,
      gatewayGroupARN = pGatewayGroupARN_,
      description = Lude.Nothing
    }

-- | The updated name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggName :: Lens.Lens' UpdateGatewayGroup (Lude.Maybe Lude.Text)
uggName = Lens.lens (name :: UpdateGatewayGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateGatewayGroup)
{-# DEPRECATED uggName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the gateway group to update.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggGatewayGroupARN :: Lens.Lens' UpdateGatewayGroup Lude.Text
uggGatewayGroupARN = Lens.lens (gatewayGroupARN :: UpdateGatewayGroup -> Lude.Text) (\s a -> s {gatewayGroupARN = a} :: UpdateGatewayGroup)
{-# DEPRECATED uggGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

-- | The updated description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggDescription :: Lens.Lens' UpdateGatewayGroup (Lude.Maybe Lude.Text)
uggDescription = Lens.lens (description :: UpdateGatewayGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateGatewayGroup)
{-# DEPRECATED uggDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateGatewayGroup where
  type Rs UpdateGatewayGroup = UpdateGatewayGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateGatewayGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGatewayGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateGatewayGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGatewayGroup where
  toJSON UpdateGatewayGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            Lude.Just ("GatewayGroupArn" Lude..= gatewayGroupARN),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateGatewayGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGatewayGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGatewayGroupResponse' smart constructor.
newtype UpdateGatewayGroupResponse = UpdateGatewayGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGatewayGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateGatewayGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGatewayGroupResponse
mkUpdateGatewayGroupResponse pResponseStatus_ =
  UpdateGatewayGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggrsResponseStatus :: Lens.Lens' UpdateGatewayGroupResponse Lude.Int
uggrsResponseStatus = Lens.lens (responseStatus :: UpdateGatewayGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGatewayGroupResponse)
{-# DEPRECATED uggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
