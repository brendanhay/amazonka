{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model-specific endpoint for a previously-trained custom model. All endpoints must be deleted in order for the model to be deleted.
module Network.AWS.Comprehend.DeleteEndpoint
  ( -- * Creating a request
    DeleteEndpoint (..),
    mkDeleteEndpoint,

    -- ** Request lenses
    deEndpointARN,

    -- * Destructuring the response
    DeleteEndpointResponse (..),
    mkDeleteEndpointResponse,

    -- ** Response lenses
    delrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint' {endpointARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Number (ARN) of the endpoint being deleted.
mkDeleteEndpoint ::
  -- | 'endpointARN'
  Lude.Text ->
  DeleteEndpoint
mkDeleteEndpoint pEndpointARN_ =
  DeleteEndpoint' {endpointARN = pEndpointARN_}

-- | The Amazon Resource Number (ARN) of the endpoint being deleted.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointARN :: Lens.Lens' DeleteEndpoint Lude.Text
deEndpointARN = Lens.lens (endpointARN :: DeleteEndpoint -> Lude.Text) (\s a -> s {endpointARN = a} :: DeleteEndpoint)
{-# DEPRECATED deEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteEndpointResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.DeleteEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointArn" Lude..= endpointARN)])

instance Lude.ToPath DeleteEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEndpointResponse' smart constructor.
newtype DeleteEndpointResponse = DeleteEndpointResponse'
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

-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEndpointResponse
mkDeleteEndpointResponse pResponseStatus_ =
  DeleteEndpointResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteEndpointResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEndpointResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
