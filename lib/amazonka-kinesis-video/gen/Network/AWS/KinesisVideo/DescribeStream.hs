{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dsStreamARN,
    dsStreamName,

    -- * Destructuring the response
    DescribeStreamResponse (..),
    mkDescribeStreamResponse,

    -- ** Response lenses
    dsfrsStreamInfo,
    dsfrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStream' smart constructor.
data DescribeStream = DescribeStream'
  { -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The name of the stream.
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
dsStreamARN :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Text)
dsStreamARN = Lens.lens (streamARN :: DescribeStream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: DescribeStream)
{-# DEPRECATED dsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStreamName :: Lens.Lens' DescribeStream (Lude.Maybe Lude.Text)
dsStreamName = Lens.lens (streamName :: DescribeStream -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: DescribeStream)
{-# DEPRECATED dsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

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
  { -- | An object that describes the stream.
    streamInfo :: Lude.Maybe StreamInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStreamResponse' with the minimum fields required to make a request.
--
-- * 'streamInfo' - An object that describes the stream.
-- * 'responseStatus' - The response status code.
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
dsfrsStreamInfo :: Lens.Lens' DescribeStreamResponse (Lude.Maybe StreamInfo)
dsfrsStreamInfo = Lens.lens (streamInfo :: DescribeStreamResponse -> Lude.Maybe StreamInfo) (\s a -> s {streamInfo = a} :: DescribeStreamResponse)
{-# DEPRECATED dsfrsStreamInfo "Use generic-lens or generic-optics with 'streamInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrsResponseStatus :: Lens.Lens' DescribeStreamResponse Lude.Int
dsfrsResponseStatus = Lens.lens (responseStatus :: DescribeStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStreamResponse)
{-# DEPRECATED dsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
