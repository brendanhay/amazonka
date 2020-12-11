{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteRealtimeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real time endpoint of an @MLModel@ .
module Network.AWS.MachineLearning.DeleteRealtimeEndpoint
  ( -- * Creating a request
    DeleteRealtimeEndpoint (..),
    mkDeleteRealtimeEndpoint,

    -- ** Request lenses
    dreMLModelId,

    -- * Destructuring the response
    DeleteRealtimeEndpointResponse (..),
    mkDeleteRealtimeEndpointResponse,

    -- ** Response lenses
    drersRealtimeEndpointInfo,
    drersMLModelId,
    drersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRealtimeEndpoint' smart constructor.
newtype DeleteRealtimeEndpoint = DeleteRealtimeEndpoint'
  { mLModelId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRealtimeEndpoint' with the minimum fields required to make a request.
--
-- * 'mLModelId' - The ID assigned to the @MLModel@ during creation.
mkDeleteRealtimeEndpoint ::
  -- | 'mLModelId'
  Lude.Text ->
  DeleteRealtimeEndpoint
mkDeleteRealtimeEndpoint pMLModelId_ =
  DeleteRealtimeEndpoint' {mLModelId = pMLModelId_}

-- | The ID assigned to the @MLModel@ during creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreMLModelId :: Lens.Lens' DeleteRealtimeEndpoint Lude.Text
dreMLModelId = Lens.lens (mLModelId :: DeleteRealtimeEndpoint -> Lude.Text) (\s a -> s {mLModelId = a} :: DeleteRealtimeEndpoint)
{-# DEPRECATED dreMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

instance Lude.AWSRequest DeleteRealtimeEndpoint where
  type Rs DeleteRealtimeEndpoint = DeleteRealtimeEndpointResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRealtimeEndpointResponse'
            Lude.<$> (x Lude..?> "RealtimeEndpointInfo")
            Lude.<*> (x Lude..?> "MLModelId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRealtimeEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DeleteRealtimeEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRealtimeEndpoint where
  toJSON DeleteRealtimeEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("MLModelId" Lude..= mLModelId)])

instance Lude.ToPath DeleteRealtimeEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRealtimeEndpoint where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @DeleteRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the @MLModel@ .
--
-- /See:/ 'mkDeleteRealtimeEndpointResponse' smart constructor.
data DeleteRealtimeEndpointResponse = DeleteRealtimeEndpointResponse'
  { realtimeEndpointInfo ::
      Lude.Maybe
        RealtimeEndpointInfo,
    mLModelId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRealtimeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
-- * 'realtimeEndpointInfo' - The endpoint information of the @MLModel@
-- * 'responseStatus' - The response status code.
mkDeleteRealtimeEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRealtimeEndpointResponse
mkDeleteRealtimeEndpointResponse pResponseStatus_ =
  DeleteRealtimeEndpointResponse'
    { realtimeEndpointInfo =
        Lude.Nothing,
      mLModelId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The endpoint information of the @MLModel@
--
-- /Note:/ Consider using 'realtimeEndpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drersRealtimeEndpointInfo :: Lens.Lens' DeleteRealtimeEndpointResponse (Lude.Maybe RealtimeEndpointInfo)
drersRealtimeEndpointInfo = Lens.lens (realtimeEndpointInfo :: DeleteRealtimeEndpointResponse -> Lude.Maybe RealtimeEndpointInfo) (\s a -> s {realtimeEndpointInfo = a} :: DeleteRealtimeEndpointResponse)
{-# DEPRECATED drersRealtimeEndpointInfo "Use generic-lens or generic-optics with 'realtimeEndpointInfo' instead." #-}

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drersMLModelId :: Lens.Lens' DeleteRealtimeEndpointResponse (Lude.Maybe Lude.Text)
drersMLModelId = Lens.lens (mLModelId :: DeleteRealtimeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: DeleteRealtimeEndpointResponse)
{-# DEPRECATED drersMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drersResponseStatus :: Lens.Lens' DeleteRealtimeEndpointResponse Lude.Int
drersResponseStatus = Lens.lens (responseStatus :: DeleteRealtimeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRealtimeEndpointResponse)
{-# DEPRECATED drersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
