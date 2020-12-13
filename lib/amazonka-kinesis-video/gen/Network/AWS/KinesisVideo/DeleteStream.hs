{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DeleteStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis video stream and the data contained in the stream.
--
-- This method marks the stream for deletion, and makes the data in the stream inaccessible immediately.
--
-- To ensure that you have the latest version of the stream before deleting it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the @DescribeStream@ API.
-- This operation requires permission for the @KinesisVideo:DeleteStream@ action.
module Network.AWS.KinesisVideo.DeleteStream
  ( -- * Creating a request
    DeleteStream (..),
    mkDeleteStream,

    -- ** Request lenses
    dCurrentVersion,
    dStreamARN,

    -- * Destructuring the response
    DeleteStreamResponse (..),
    mkDeleteStreamResponse,

    -- ** Response lenses
    dsrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | Optional: The version of the stream that you want to delete.
    --
    -- Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API.
    -- If not specified, only the @CreationTime@ is checked before deleting the stream.
    currentVersion :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the stream that you want to delete.
    streamARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStream' with the minimum fields required to make a request.
--
-- * 'currentVersion' - Optional: The version of the stream that you want to delete.
--
-- Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API.
-- If not specified, only the @CreationTime@ is checked before deleting the stream.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream that you want to delete.
mkDeleteStream ::
  -- | 'streamARN'
  Lude.Text ->
  DeleteStream
mkDeleteStream pStreamARN_ =
  DeleteStream'
    { currentVersion = Lude.Nothing,
      streamARN = pStreamARN_
    }

-- | Optional: The version of the stream that you want to delete.
--
-- Specify the version as a safeguard to ensure that your are deleting the correct stream. To get the stream version, use the @DescribeStream@ API.
-- If not specified, only the @CreationTime@ is checked before deleting the stream.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCurrentVersion :: Lens.Lens' DeleteStream (Lude.Maybe Lude.Text)
dCurrentVersion = Lens.lens (currentVersion :: DeleteStream -> Lude.Maybe Lude.Text) (\s a -> s {currentVersion = a} :: DeleteStream)
{-# DEPRECATED dCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream that you want to delete.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamARN :: Lens.Lens' DeleteStream Lude.Text
dStreamARN = Lens.lens (streamARN :: DeleteStream -> Lude.Text) (\s a -> s {streamARN = a} :: DeleteStream)
{-# DEPRECATED dStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

instance Lude.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteStreamResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeleteStream where
  toJSON DeleteStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CurrentVersion" Lude..=) Lude.<$> currentVersion,
            Lude.Just ("StreamARN" Lude..= streamARN)
          ]
      )

instance Lude.ToPath DeleteStream where
  toPath = Lude.const "/deleteStream"

instance Lude.ToQuery DeleteStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStreamResponse' smart constructor.
newtype DeleteStreamResponse = DeleteStreamResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
