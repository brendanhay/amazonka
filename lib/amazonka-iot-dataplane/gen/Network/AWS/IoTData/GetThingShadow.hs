{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.GetThingShadow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html GetThingShadow> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.GetThingShadow
  ( -- * Creating a request
    GetThingShadow (..),
    mkGetThingShadow,

    -- ** Request lenses
    gtsShadowName,
    gtsThingName,

    -- * Destructuring the response
    GetThingShadowResponse (..),
    mkGetThingShadowResponse,

    -- ** Response lenses
    gtsrsPayload,
    gtsrsResponseStatus,
  )
where

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetThingShadow operation.
--
-- /See:/ 'mkGetThingShadow' smart constructor.
data GetThingShadow = GetThingShadow'
  { shadowName ::
      Lude.Maybe Lude.Text,
    thingName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThingShadow' with the minimum fields required to make a request.
--
-- * 'shadowName' - The name of the shadow.
-- * 'thingName' - The name of the thing.
mkGetThingShadow ::
  -- | 'thingName'
  Lude.Text ->
  GetThingShadow
mkGetThingShadow pThingName_ =
  GetThingShadow'
    { shadowName = Lude.Nothing,
      thingName = pThingName_
    }

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsShadowName :: Lens.Lens' GetThingShadow (Lude.Maybe Lude.Text)
gtsShadowName = Lens.lens (shadowName :: GetThingShadow -> Lude.Maybe Lude.Text) (\s a -> s {shadowName = a} :: GetThingShadow)
{-# DEPRECATED gtsShadowName "Use generic-lens or generic-optics with 'shadowName' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsThingName :: Lens.Lens' GetThingShadow Lude.Text
gtsThingName = Lens.lens (thingName :: GetThingShadow -> Lude.Text) (\s a -> s {thingName = a} :: GetThingShadow)
{-# DEPRECATED gtsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest GetThingShadow where
  type Rs GetThingShadow = GetThingShadowResponse
  request = Req.get ioTDataService
  response =
    Res.receiveBytes
      ( \s h x ->
          GetThingShadowResponse'
            Lude.<$> (Lude.pure (Lude.Just x)) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetThingShadow where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetThingShadow where
  toPath GetThingShadow' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/shadow"]

instance Lude.ToQuery GetThingShadow where
  toQuery GetThingShadow' {..} =
    Lude.mconcat ["name" Lude.=: shadowName]

-- | The output from the GetThingShadow operation.
--
-- /See:/ 'mkGetThingShadowResponse' smart constructor.
data GetThingShadowResponse = GetThingShadowResponse'
  { payload ::
      Lude.Maybe Lude.ByteString,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThingShadowResponse' with the minimum fields required to make a request.
--
-- * 'payload' - The state information, in JSON format.
-- * 'responseStatus' - The response status code.
mkGetThingShadowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetThingShadowResponse
mkGetThingShadowResponse pResponseStatus_ =
  GetThingShadowResponse'
    { payload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsPayload :: Lens.Lens' GetThingShadowResponse (Lude.Maybe Lude.ByteString)
gtsrsPayload = Lens.lens (payload :: GetThingShadowResponse -> Lude.Maybe Lude.ByteString) (\s a -> s {payload = a} :: GetThingShadowResponse)
{-# DEPRECATED gtsrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResponseStatus :: Lens.Lens' GetThingShadowResponse Lude.Int
gtsrsResponseStatus = Lens.lens (responseStatus :: GetThingShadowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetThingShadowResponse)
{-# DEPRECATED gtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
