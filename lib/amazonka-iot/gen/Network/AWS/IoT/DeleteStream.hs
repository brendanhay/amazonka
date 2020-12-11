{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stream.
module Network.AWS.IoT.DeleteStream
  ( -- * Creating a request
    DeleteStream (..),
    mkDeleteStream,

    -- ** Request lenses
    dsStreamId,

    -- * Destructuring the response
    DeleteStreamResponse (..),
    mkDeleteStreamResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStream' smart constructor.
newtype DeleteStream = DeleteStream' {streamId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStream' with the minimum fields required to make a request.
--
-- * 'streamId' - The stream ID.
mkDeleteStream ::
  -- | 'streamId'
  Lude.Text ->
  DeleteStream
mkDeleteStream pStreamId_ = DeleteStream' {streamId = pStreamId_}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamId :: Lens.Lens' DeleteStream Lude.Text
dsStreamId = Lens.lens (streamId :: DeleteStream -> Lude.Text) (\s a -> s {streamId = a} :: DeleteStream)
{-# DEPRECATED dsStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

instance Lude.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteStreamResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteStream where
  toPath DeleteStream' {..} =
    Lude.mconcat ["/streams/", Lude.toBS streamId]

instance Lude.ToQuery DeleteStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStreamResponse' smart constructor.
newtype DeleteStreamResponse = DeleteStreamResponse'
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

-- | Creates a value of 'DeleteStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteStreamResponse
mkDeleteStreamResponse pResponseStatus_ =
  DeleteStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteStreamResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteStreamResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
