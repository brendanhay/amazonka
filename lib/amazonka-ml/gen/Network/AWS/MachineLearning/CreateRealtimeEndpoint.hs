{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateRealtimeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time endpoint for the @MLModel@ . The endpoint contains the URI of the @MLModel@ ; that is, the location to send real-time prediction requests for the specified @MLModel@ .
module Network.AWS.MachineLearning.CreateRealtimeEndpoint
  ( -- * Creating a request
    CreateRealtimeEndpoint (..),
    mkCreateRealtimeEndpoint,

    -- ** Request lenses
    creMLModelId,

    -- * Destructuring the response
    CreateRealtimeEndpointResponse (..),
    mkCreateRealtimeEndpointResponse,

    -- ** Response lenses
    crersRealtimeEndpointInfo,
    crersMLModelId,
    crersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRealtimeEndpoint' smart constructor.
newtype CreateRealtimeEndpoint = CreateRealtimeEndpoint'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRealtimeEndpoint' with the minimum fields required to make a request.
--
-- * 'mLModelId' - The ID assigned to the @MLModel@ during creation.
mkCreateRealtimeEndpoint ::
  -- | 'mLModelId'
  Lude.Text ->
  CreateRealtimeEndpoint
mkCreateRealtimeEndpoint pMLModelId_ =
  CreateRealtimeEndpoint' {mLModelId = pMLModelId_}

-- | The ID assigned to the @MLModel@ during creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creMLModelId :: Lens.Lens' CreateRealtimeEndpoint Lude.Text
creMLModelId = Lens.lens (mLModelId :: CreateRealtimeEndpoint -> Lude.Text) (\s a -> s {mLModelId = a} :: CreateRealtimeEndpoint)
{-# DEPRECATED creMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

instance Lude.AWSRequest CreateRealtimeEndpoint where
  type Rs CreateRealtimeEndpoint = CreateRealtimeEndpointResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRealtimeEndpointResponse'
            Lude.<$> (x Lude..?> "RealtimeEndpointInfo")
            Lude.<*> (x Lude..?> "MLModelId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRealtimeEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.CreateRealtimeEndpoint" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRealtimeEndpoint where
  toJSON CreateRealtimeEndpoint' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("MLModelId" Lude..= mLModelId)])

instance Lude.ToPath CreateRealtimeEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRealtimeEndpoint where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @CreateRealtimeEndpoint@ operation.
--
-- The result contains the @MLModelId@ and the endpoint information for the @MLModel@ .
--
-- /See:/ 'mkCreateRealtimeEndpointResponse' smart constructor.
data CreateRealtimeEndpointResponse = CreateRealtimeEndpointResponse'
  { -- | The endpoint information of the @MLModel@
    realtimeEndpointInfo :: Lude.Maybe RealtimeEndpointInfo,
    -- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
    mLModelId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRealtimeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'realtimeEndpointInfo' - The endpoint information of the @MLModel@
-- * 'mLModelId' - A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateRealtimeEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRealtimeEndpointResponse
mkCreateRealtimeEndpointResponse pResponseStatus_ =
  CreateRealtimeEndpointResponse'
    { realtimeEndpointInfo =
        Lude.Nothing,
      mLModelId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The endpoint information of the @MLModel@
--
-- /Note:/ Consider using 'realtimeEndpointInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersRealtimeEndpointInfo :: Lens.Lens' CreateRealtimeEndpointResponse (Lude.Maybe RealtimeEndpointInfo)
crersRealtimeEndpointInfo = Lens.lens (realtimeEndpointInfo :: CreateRealtimeEndpointResponse -> Lude.Maybe RealtimeEndpointInfo) (\s a -> s {realtimeEndpointInfo = a} :: CreateRealtimeEndpointResponse)
{-# DEPRECATED crersRealtimeEndpointInfo "Use generic-lens or generic-optics with 'realtimeEndpointInfo' instead." #-}

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelId@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersMLModelId :: Lens.Lens' CreateRealtimeEndpointResponse (Lude.Maybe Lude.Text)
crersMLModelId = Lens.lens (mLModelId :: CreateRealtimeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: CreateRealtimeEndpointResponse)
{-# DEPRECATED crersMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersResponseStatus :: Lens.Lens' CreateRealtimeEndpointResponse Lude.Int
crersResponseStatus = Lens.lens (responseStatus :: CreateRealtimeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRealtimeEndpointResponse)
{-# DEPRECATED crersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
