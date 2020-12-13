{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.UpdateThingShadow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html UpdateThingShadow> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.UpdateThingShadow
  ( -- * Creating a request
    UpdateThingShadow (..),
    mkUpdateThingShadow,

    -- ** Request lenses
    utsPayload,
    utsShadowName,
    utsThingName,

    -- * Destructuring the response
    UpdateThingShadowResponse (..),
    mkUpdateThingShadowResponse,

    -- ** Response lenses
    utsrsPayload,
    utsrsResponseStatus,
  )
where

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the UpdateThingShadow operation.
--
-- /See:/ 'mkUpdateThingShadow' smart constructor.
data UpdateThingShadow = UpdateThingShadow'
  { -- | The state information, in JSON format.
    payload :: Lude.ByteString,
    -- | The name of the shadow.
    shadowName :: Lude.Maybe Lude.Text,
    -- | The name of the thing.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingShadow' with the minimum fields required to make a request.
--
-- * 'payload' - The state information, in JSON format.
-- * 'shadowName' - The name of the shadow.
-- * 'thingName' - The name of the thing.
mkUpdateThingShadow ::
  -- | 'payload'
  Lude.ByteString ->
  -- | 'thingName'
  Lude.Text ->
  UpdateThingShadow
mkUpdateThingShadow pPayload_ pThingName_ =
  UpdateThingShadow'
    { payload = pPayload_,
      shadowName = Lude.Nothing,
      thingName = pThingName_
    }

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsPayload :: Lens.Lens' UpdateThingShadow Lude.ByteString
utsPayload = Lens.lens (payload :: UpdateThingShadow -> Lude.ByteString) (\s a -> s {payload = a} :: UpdateThingShadow)
{-# DEPRECATED utsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsShadowName :: Lens.Lens' UpdateThingShadow (Lude.Maybe Lude.Text)
utsShadowName = Lens.lens (shadowName :: UpdateThingShadow -> Lude.Maybe Lude.Text) (\s a -> s {shadowName = a} :: UpdateThingShadow)
{-# DEPRECATED utsShadowName "Use generic-lens or generic-optics with 'shadowName' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsThingName :: Lens.Lens' UpdateThingShadow Lude.Text
utsThingName = Lens.lens (thingName :: UpdateThingShadow -> Lude.Text) (\s a -> s {thingName = a} :: UpdateThingShadow)
{-# DEPRECATED utsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest UpdateThingShadow where
  type Rs UpdateThingShadow = UpdateThingShadowResponse
  request = Req.postBody ioTDataService
  response =
    Res.receiveBytes
      ( \s h x ->
          UpdateThingShadowResponse'
            Lude.<$> (Lude.pure (Lude.Just x)) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody UpdateThingShadow where
  toBody = Lude.toBody Lude.. payload

instance Lude.ToHeaders UpdateThingShadow where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateThingShadow where
  toPath UpdateThingShadow' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/shadow"]

instance Lude.ToQuery UpdateThingShadow where
  toQuery UpdateThingShadow' {..} =
    Lude.mconcat ["name" Lude.=: shadowName]

-- | The output from the UpdateThingShadow operation.
--
-- /See:/ 'mkUpdateThingShadowResponse' smart constructor.
data UpdateThingShadowResponse = UpdateThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Lude.Maybe Lude.ByteString,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateThingShadowResponse' with the minimum fields required to make a request.
--
-- * 'payload' - The state information, in JSON format.
-- * 'responseStatus' - The response status code.
mkUpdateThingShadowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateThingShadowResponse
mkUpdateThingShadowResponse pResponseStatus_ =
  UpdateThingShadowResponse'
    { payload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrsPayload :: Lens.Lens' UpdateThingShadowResponse (Lude.Maybe Lude.ByteString)
utsrsPayload = Lens.lens (payload :: UpdateThingShadowResponse -> Lude.Maybe Lude.ByteString) (\s a -> s {payload = a} :: UpdateThingShadowResponse)
{-# DEPRECATED utsrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsrsResponseStatus :: Lens.Lens' UpdateThingShadowResponse Lude.Int
utsrsResponseStatus = Lens.lens (responseStatus :: UpdateThingShadowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateThingShadowResponse)
{-# DEPRECATED utsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
