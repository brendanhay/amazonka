{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.TagStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a stream. A /tag/ is a key-value pair (the value is optional) that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
-- You must provide either the @StreamName@ or the @StreamARN@ .
-- This operation requires permission for the @KinesisVideo:TagStream@ action.
-- Kinesis video streams support up to 50 tags.
module Network.AWS.KinesisVideo.TagStream
  ( -- * Creating a request
    TagStream (..),
    mkTagStream,

    -- ** Request lenses
    tsStreamARN,
    tsStreamName,
    tsTags,

    -- * Destructuring the response
    TagStreamResponse (..),
    mkTagStreamResponse,

    -- ** Response lenses
    tsrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagStream' smart constructor.
data TagStream = TagStream'
  { -- | The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The name of the stream that you want to add the tag or tags to.
    streamName :: Lude.Maybe Lude.Text,
    -- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagStream' with the minimum fields required to make a request.
--
-- * 'streamARN' - The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
-- * 'streamName' - The name of the stream that you want to add the tag or tags to.
-- * 'tags' - A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
mkTagStream ::
  TagStream
mkTagStream =
  TagStream'
    { streamARN = Lude.Nothing,
      streamName = Lude.Nothing,
      tags = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the resource that you want to add the tag or tags to.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStreamARN :: Lens.Lens' TagStream (Lude.Maybe Lude.Text)
tsStreamARN = Lens.lens (streamARN :: TagStream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: TagStream)
{-# DEPRECATED tsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to add the tag or tags to.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsStreamName :: Lens.Lens' TagStream (Lude.Maybe Lude.Text)
tsStreamName = Lens.lens (streamName :: TagStream -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: TagStream)
{-# DEPRECATED tsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | A list of tags to associate with the specified stream. Each tag is a key-value pair (the value is optional).
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTags :: Lens.Lens' TagStream (Lude.HashMap Lude.Text (Lude.Text))
tsTags = Lens.lens (tags :: TagStream -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagStream)
{-# DEPRECATED tsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagStream where
  type Rs TagStream = TagStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          TagStreamResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON TagStream where
  toJSON TagStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName,
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath TagStream where
  toPath = Lude.const "/tagStream"

instance Lude.ToQuery TagStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagStreamResponse' smart constructor.
newtype TagStreamResponse = TagStreamResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkTagStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagStreamResponse
mkTagStreamResponse pResponseStatus_ =
  TagStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsrsResponseStatus :: Lens.Lens' TagStreamResponse Lude.Int
tsrsResponseStatus = Lens.lens (responseStatus :: TagStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagStreamResponse)
{-# DEPRECATED tsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
