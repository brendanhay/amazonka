{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.DeleteThingShadow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the shadow for the specified thing.
--
-- For more information, see <http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html DeleteThingShadow> in the AWS IoT Developer Guide.
module Network.AWS.IoTData.DeleteThingShadow
  ( -- * Creating a request
    DeleteThingShadow (..),
    mkDeleteThingShadow,

    -- ** Request lenses
    dtsShadowName,
    dtsThingName,

    -- * Destructuring the response
    DeleteThingShadowResponse (..),
    mkDeleteThingShadowResponse,

    -- ** Response lenses
    dtsrsPayload,
    dtsrsResponseStatus,
  )
where

import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeleteThingShadow operation.
--
-- /See:/ 'mkDeleteThingShadow' smart constructor.
data DeleteThingShadow = DeleteThingShadow'
  { -- | The name of the shadow.
    shadowName :: Lude.Maybe Lude.Text,
    -- | The name of the thing.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingShadow' with the minimum fields required to make a request.
--
-- * 'shadowName' - The name of the shadow.
-- * 'thingName' - The name of the thing.
mkDeleteThingShadow ::
  -- | 'thingName'
  Lude.Text ->
  DeleteThingShadow
mkDeleteThingShadow pThingName_ =
  DeleteThingShadow'
    { shadowName = Lude.Nothing,
      thingName = pThingName_
    }

-- | The name of the shadow.
--
-- /Note:/ Consider using 'shadowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsShadowName :: Lens.Lens' DeleteThingShadow (Lude.Maybe Lude.Text)
dtsShadowName = Lens.lens (shadowName :: DeleteThingShadow -> Lude.Maybe Lude.Text) (\s a -> s {shadowName = a} :: DeleteThingShadow)
{-# DEPRECATED dtsShadowName "Use generic-lens or generic-optics with 'shadowName' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsThingName :: Lens.Lens' DeleteThingShadow Lude.Text
dtsThingName = Lens.lens (thingName :: DeleteThingShadow -> Lude.Text) (\s a -> s {thingName = a} :: DeleteThingShadow)
{-# DEPRECATED dtsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest DeleteThingShadow where
  type Rs DeleteThingShadow = DeleteThingShadowResponse
  request = Req.delete ioTDataService
  response =
    Res.receiveBytes
      ( \s h x ->
          DeleteThingShadowResponse'
            Lude.<$> (Lude.pure x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteThingShadow where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteThingShadow where
  toPath DeleteThingShadow' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName, "/shadow"]

instance Lude.ToQuery DeleteThingShadow where
  toQuery DeleteThingShadow' {..} =
    Lude.mconcat ["name" Lude.=: shadowName]

-- | The output from the DeleteThingShadow operation.
--
-- /See:/ 'mkDeleteThingShadowResponse' smart constructor.
data DeleteThingShadowResponse = DeleteThingShadowResponse'
  { -- | The state information, in JSON format.
    payload :: Lude.ByteString,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingShadowResponse' with the minimum fields required to make a request.
--
-- * 'payload' - The state information, in JSON format.
-- * 'responseStatus' - The response status code.
mkDeleteThingShadowResponse ::
  -- | 'payload'
  Lude.ByteString ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteThingShadowResponse
mkDeleteThingShadowResponse pPayload_ pResponseStatus_ =
  DeleteThingShadowResponse'
    { payload = pPayload_,
      responseStatus = pResponseStatus_
    }

-- | The state information, in JSON format.
--
-- /Note:/ Consider using 'payload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsPayload :: Lens.Lens' DeleteThingShadowResponse Lude.ByteString
dtsrsPayload = Lens.lens (payload :: DeleteThingShadowResponse -> Lude.ByteString) (\s a -> s {payload = a} :: DeleteThingShadowResponse)
{-# DEPRECATED dtsrsPayload "Use generic-lens or generic-optics with 'payload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtsrsResponseStatus :: Lens.Lens' DeleteThingShadowResponse Lude.Int
dtsrsResponseStatus = Lens.lens (responseStatus :: DeleteThingShadowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteThingShadowResponse)
{-# DEPRECATED dtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
