{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGateway
  ( -- * Creating a request
    UpdateGateway (..),
    mkUpdateGateway,

    -- ** Request lenses
    ugName,
    ugSoftwareVersion,
    ugDescription,
    ugGatewayARN,

    -- * Destructuring the response
    UpdateGatewayResponse (..),
    mkUpdateGatewayResponse,

    -- ** Response lenses
    ugrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { name :: Lude.Maybe Lude.Text,
    softwareVersion :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    gatewayARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGateway' with the minimum fields required to make a request.
--
-- * 'description' - The updated description of the gateway.
-- * 'gatewayARN' - The ARN of the gateway to update.
-- * 'name' - The updated name of the gateway.
-- * 'softwareVersion' - The updated software version of the gateway. The gateway automatically updates its software version during normal operation.
mkUpdateGateway ::
  -- | 'gatewayARN'
  Lude.Text ->
  UpdateGateway
mkUpdateGateway pGatewayARN_ =
  UpdateGateway'
    { name = Lude.Nothing,
      softwareVersion = Lude.Nothing,
      description = Lude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | The updated name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugName :: Lens.Lens' UpdateGateway (Lude.Maybe Lude.Text)
ugName = Lens.lens (name :: UpdateGateway -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateGateway)
{-# DEPRECATED ugName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The updated software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugSoftwareVersion :: Lens.Lens' UpdateGateway (Lude.Maybe Lude.Text)
ugSoftwareVersion = Lens.lens (softwareVersion :: UpdateGateway -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: UpdateGateway)
{-# DEPRECATED ugSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The updated description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGateway (Lude.Maybe Lude.Text)
ugDescription = Lens.lens (description :: UpdateGateway -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateGateway)
{-# DEPRECATED ugDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the gateway to update.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGatewayARN :: Lens.Lens' UpdateGateway Lude.Text
ugGatewayARN = Lens.lens (gatewayARN :: UpdateGateway -> Lude.Text) (\s a -> s {gatewayARN = a} :: UpdateGateway)
{-# DEPRECATED ugGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Lude.AWSRequest UpdateGateway where
  type Rs UpdateGateway = UpdateGatewayResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateGatewayResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGateway where
  toJSON UpdateGateway' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Name" Lude..=) Lude.<$> name,
            ("SoftwareVersion" Lude..=) Lude.<$> softwareVersion,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("GatewayArn" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath UpdateGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGateway where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGatewayResponse' smart constructor.
newtype UpdateGatewayResponse = UpdateGatewayResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGatewayResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGatewayResponse
mkUpdateGatewayResponse pResponseStatus_ =
  UpdateGatewayResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsResponseStatus :: Lens.Lens' UpdateGatewayResponse Lude.Int
ugrsResponseStatus = Lens.lens (responseStatus :: UpdateGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGatewayResponse)
{-# DEPRECATED ugrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
