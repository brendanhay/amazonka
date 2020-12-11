{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream processor identified by @Name@ . You assign the value for @Name@ when you create the stream processor with 'CreateStreamProcessor' . You might not be able to use the same name for a stream processor for a few seconds after calling @DeleteStreamProcessor@ .
module Network.AWS.Rekognition.DeleteStreamProcessor
  ( -- * Creating a request
    DeleteStreamProcessor (..),
    mkDeleteStreamProcessor,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DeleteStreamProcessorResponse (..),
    mkDeleteStreamProcessorResponse,

    -- ** Response lenses
    dspsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStreamProcessor' smart constructor.
newtype DeleteStreamProcessor = DeleteStreamProcessor'
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

-- | Creates a value of 'DeleteStreamProcessor' with the minimum fields required to make a request.
--
-- * 'name' - The name of the stream processor you want to delete.
mkDeleteStreamProcessor ::
  -- | 'name'
  Lude.Text ->
  DeleteStreamProcessor
mkDeleteStreamProcessor pName_ =
  DeleteStreamProcessor' {name = pName_}

-- | The name of the stream processor you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteStreamProcessor Lude.Text
dName = Lens.lens (name :: DeleteStreamProcessor -> Lude.Text) (\s a -> s {name = a} :: DeleteStreamProcessor)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteStreamProcessor where
  type Rs DeleteStreamProcessor = DeleteStreamProcessorResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteStreamProcessorResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStreamProcessor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DeleteStreamProcessor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStreamProcessor where
  toJSON DeleteStreamProcessor' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteStreamProcessor where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStreamProcessor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStreamProcessorResponse' smart constructor.
newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse'
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

-- | Creates a value of 'DeleteStreamProcessorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteStreamProcessorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteStreamProcessorResponse
mkDeleteStreamProcessorResponse pResponseStatus_ =
  DeleteStreamProcessorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspsrsResponseStatus :: Lens.Lens' DeleteStreamProcessorResponse Lude.Int
dspsrsResponseStatus = Lens.lens (responseStatus :: DeleteStreamProcessorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteStreamProcessorResponse)
{-# DEPRECATED dspsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
