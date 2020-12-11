{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.DescribeStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the most current information about the specified stream. You must specify either the @StreamName@ or the @StreamARN@ .
module Network.AWS.KinesisVideo.DescribeStream
  ( -- * Creating a request
    DescribeStream (..),
    mkDescribeStream,

    -- ** Request lenses
    dStreamARN,
    dStreamName,

    -- * Destructuring the response
    DescribeStreamResponse (..),
    mkDescribeStreamResponse,

    -- ** Response lenses
    drsStreamInfo,
    drsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { streamARN ::
      Lude.Maybe Lude.Text,
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStream' with the minimum fields required to make a request.
--
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream.
-- * 'streamName' - The name of the stream.
mkDescribeStream ::
  DescribeStream
mkDescribeStream =
  DescribeStream'
    { streamARN = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamARN :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Text)
dStreamARN = Lens.lens (streamARN :: DescribeStream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: DescribeStream)
{-# DEPRECATED dStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStreamName :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Text)
dStreamName = Lens.lens (streamName :: DescribeStream -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: DescribeStream)
{-# DEPRECATED dStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest DescribeStream where
  type Rs DescribeStream = DescribeStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStreamResponse'
            Lude.<$> (x Lude..?> "StreamInfo") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribeStream where
  toJSON DescribeStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath DescribeStream where
  toPath = Lude.const "/describeStream"

instance Lude.ToQuery DescribeStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStreamResponse' smart constructor.
data DescribeStreamResponse = DescribeStreamResponse'
  { streamInfo ::
      Lude.Maybe StreamInfo,
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

-- | Creates a value of 'DescribeStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'streamInfo' - An object that describes the stream.
mkDescribeStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStreamResponse
mkDescribeStreamResponse pResponseStatus_ =
  DescribeStreamResponse'
    { streamInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the stream.
--
-- /Note:/ Consider using 'streamInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStreamInfo :: Lens.Lens' DescribeStreamResponse (Lude.Maybe StreamInfo)
drsStreamInfo = Lens.lens (streamInfo :: DescribeStreamResponse -> Lude.Maybe StreamInfo) (\s a -> s {streamInfo = a} :: DescribeStreamResponse)
{-# DEPRECATED drsStreamInfo "Use generic-lens or generic-optics with 'streamInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeStreamResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
