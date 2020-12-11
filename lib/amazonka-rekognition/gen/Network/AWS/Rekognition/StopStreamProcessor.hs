{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StopStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running stream processor that was created by 'CreateStreamProcessor' .
module Network.AWS.Rekognition.StopStreamProcessor
  ( -- * Creating a request
    StopStreamProcessor (..),
    mkStopStreamProcessor,

    -- ** Request lenses
    sspName,

    -- * Destructuring the response
    StopStreamProcessorResponse (..),
    mkStopStreamProcessorResponse,

    -- ** Response lenses
    ssprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopStreamProcessor' smart constructor.
newtype StopStreamProcessor = StopStreamProcessor'
  { name ::
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

-- | Creates a value of 'StopStreamProcessor' with the minimum fields required to make a request.
--
-- * 'name' - The name of a stream processor created by 'CreateStreamProcessor' .
mkStopStreamProcessor ::
  -- | 'name'
  Lude.Text ->
  StopStreamProcessor
mkStopStreamProcessor pName_ = StopStreamProcessor' {name = pName_}

-- | The name of a stream processor created by 'CreateStreamProcessor' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sspName :: Lens.Lens' StopStreamProcessor Lude.Text
sspName = Lens.lens (name :: StopStreamProcessor -> Lude.Text) (\s a -> s {name = a} :: StopStreamProcessor)
{-# DEPRECATED sspName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StopStreamProcessor where
  type Rs StopStreamProcessor = StopStreamProcessorResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopStreamProcessorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopStreamProcessor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StopStreamProcessor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopStreamProcessor where
  toJSON StopStreamProcessor' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StopStreamProcessor where
  toPath = Lude.const "/"

instance Lude.ToQuery StopStreamProcessor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopStreamProcessorResponse' smart constructor.
newtype StopStreamProcessorResponse = StopStreamProcessorResponse'
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

-- | Creates a value of 'StopStreamProcessorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopStreamProcessorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopStreamProcessorResponse
mkStopStreamProcessorResponse pResponseStatus_ =
  StopStreamProcessorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssprsResponseStatus :: Lens.Lens' StopStreamProcessorResponse Lude.Int
ssprsResponseStatus = Lens.lens (responseStatus :: StopStreamProcessorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopStreamProcessorResponse)
{-# DEPRECATED ssprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
