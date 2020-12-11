{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UntagStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a stream. In the request, specify only a tag key or keys; don't specify the value. If you specify a tag key that does not exist, it's ignored.
--
-- In the request, you must provide the @StreamName@ or @StreamARN@ .
module Network.AWS.KinesisVideo.UntagStream
  ( -- * Creating a request
    UntagStream (..),
    mkUntagStream,

    -- ** Request lenses
    usStreamARN,
    usStreamName,
    usTagKeyList,

    -- * Destructuring the response
    UntagStreamResponse (..),
    mkUntagStreamResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagStream' smart constructor.
data UntagStream = UntagStream'
  { streamARN :: Lude.Maybe Lude.Text,
    streamName :: Lude.Maybe Lude.Text,
    tagKeyList :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagStream' with the minimum fields required to make a request.
--
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
-- * 'streamName' - The name of the stream that you want to remove tags from.
-- * 'tagKeyList' - A list of the keys of the tags that you want to remove.
mkUntagStream ::
  -- | 'tagKeyList'
  Lude.NonEmpty Lude.Text ->
  UntagStream
mkUntagStream pTagKeyList_ =
  UntagStream'
    { streamARN = Lude.Nothing,
      streamName = Lude.Nothing,
      tagKeyList = pTagKeyList_
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamARN :: Lens.Lens' UntagStream (Lude.Maybe Lude.Text)
usStreamARN = Lens.lens (streamARN :: UntagStream -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: UntagStream)
{-# DEPRECATED usStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream that you want to remove tags from.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStreamName :: Lens.Lens' UntagStream (Lude.Maybe Lude.Text)
usStreamName = Lens.lens (streamName :: UntagStream -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: UntagStream)
{-# DEPRECATED usStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | A list of the keys of the tags that you want to remove.
--
-- /Note:/ Consider using 'tagKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTagKeyList :: Lens.Lens' UntagStream (Lude.NonEmpty Lude.Text)
usTagKeyList = Lens.lens (tagKeyList :: UntagStream -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeyList = a} :: UntagStream)
{-# DEPRECATED usTagKeyList "Use generic-lens or generic-optics with 'tagKeyList' instead." #-}

instance Lude.AWSRequest UntagStream where
  type Rs UntagStream = UntagStreamResponse
  request = Req.postJSON kinesisVideoService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UntagStreamResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UntagStream where
  toJSON UntagStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("StreamName" Lude..=) Lude.<$> streamName,
            Lude.Just ("TagKeyList" Lude..= tagKeyList)
          ]
      )

instance Lude.ToPath UntagStream where
  toPath = Lude.const "/untagStream"

instance Lude.ToQuery UntagStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagStreamResponse' smart constructor.
newtype UntagStreamResponse = UntagStreamResponse'
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

-- | Creates a value of 'UntagStreamResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUntagStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagStreamResponse
mkUntagStreamResponse pResponseStatus_ =
  UntagStreamResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UntagStreamResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UntagStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagStreamResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
