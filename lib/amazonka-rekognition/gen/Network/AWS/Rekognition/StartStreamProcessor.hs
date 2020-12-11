{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts processing a stream processor. You create a stream processor by calling 'CreateStreamProcessor' . To tell @StartStreamProcessor@ which stream processor to start, use the value of the @Name@ field specified in the call to @CreateStreamProcessor@ .
module Network.AWS.Rekognition.StartStreamProcessor
  ( -- * Creating a request
    StartStreamProcessor (..),
    mkStartStreamProcessor,

    -- ** Request lenses
    sName,

    -- * Destructuring the response
    StartStreamProcessorResponse (..),
    mkStartStreamProcessorResponse,

    -- ** Response lenses
    starsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartStreamProcessor' smart constructor.
newtype StartStreamProcessor = StartStreamProcessor'
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

-- | Creates a value of 'StartStreamProcessor' with the minimum fields required to make a request.
--
-- * 'name' - The name of the stream processor to start processing.
mkStartStreamProcessor ::
  -- | 'name'
  Lude.Text ->
  StartStreamProcessor
mkStartStreamProcessor pName_ =
  StartStreamProcessor' {name = pName_}

-- | The name of the stream processor to start processing.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StartStreamProcessor Lude.Text
sName = Lens.lens (name :: StartStreamProcessor -> Lude.Text) (\s a -> s {name = a} :: StartStreamProcessor)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartStreamProcessor where
  type Rs StartStreamProcessor = StartStreamProcessorResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartStreamProcessorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartStreamProcessor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartStreamProcessor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartStreamProcessor where
  toJSON StartStreamProcessor' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StartStreamProcessor where
  toPath = Lude.const "/"

instance Lude.ToQuery StartStreamProcessor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartStreamProcessorResponse' smart constructor.
newtype StartStreamProcessorResponse = StartStreamProcessorResponse'
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

-- | Creates a value of 'StartStreamProcessorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartStreamProcessorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartStreamProcessorResponse
mkStartStreamProcessorResponse pResponseStatus_ =
  StartStreamProcessorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsResponseStatus :: Lens.Lens' StartStreamProcessorResponse Lude.Int
starsResponseStatus = Lens.lens (responseStatus :: StartStreamProcessorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartStreamProcessorResponse)
{-# DEPRECATED starsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
