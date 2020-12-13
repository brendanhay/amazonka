{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.AddTagsToStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified Kinesis data stream. Each time you invoke this operation, you can specify up to 10 tags. If you want to add more than 10 tags to your stream, you can invoke this operation multiple times. In total, each stream can have up to 50 tags.
--
-- If tags have already been assigned to the stream, @AddTagsToStream@ overwrites any existing tags that correspond to the specified tag keys.
-- 'AddTagsToStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.AddTagsToStream
  ( -- * Creating a request
    AddTagsToStream (..),
    mkAddTagsToStream,

    -- ** Request lenses
    attsStreamName,
    attsTags,

    -- * Destructuring the response
    AddTagsToStreamResponse (..),
    mkAddTagsToStreamResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @AddTagsToStream@ .
--
-- /See:/ 'mkAddTagsToStream' smart constructor.
data AddTagsToStream = AddTagsToStream'
  { -- | The name of the stream.
    streamName :: Lude.Text,
    -- | A set of up to 10 key-value pairs to use to create the tags.
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToStream' with the minimum fields required to make a request.
--
-- * 'streamName' - The name of the stream.
-- * 'tags' - A set of up to 10 key-value pairs to use to create the tags.
mkAddTagsToStream ::
  -- | 'streamName'
  Lude.Text ->
  AddTagsToStream
mkAddTagsToStream pStreamName_ =
  AddTagsToStream' {streamName = pStreamName_, tags = Lude.mempty}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attsStreamName :: Lens.Lens' AddTagsToStream Lude.Text
attsStreamName = Lens.lens (streamName :: AddTagsToStream -> Lude.Text) (\s a -> s {streamName = a} :: AddTagsToStream)
{-# DEPRECATED attsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | A set of up to 10 key-value pairs to use to create the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
attsTags :: Lens.Lens' AddTagsToStream (Lude.HashMap Lude.Text (Lude.Text))
attsTags = Lens.lens (tags :: AddTagsToStream -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: AddTagsToStream)
{-# DEPRECATED attsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest AddTagsToStream where
  type Rs AddTagsToStream = AddTagsToStreamResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull AddTagsToStreamResponse'

instance Lude.ToHeaders AddTagsToStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.AddTagsToStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddTagsToStream where
  toJSON AddTagsToStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamName" Lude..= streamName),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath AddTagsToStream where
  toPath = Lude.const "/"

instance Lude.ToQuery AddTagsToStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddTagsToStreamResponse' smart constructor.
data AddTagsToStreamResponse = AddTagsToStreamResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddTagsToStreamResponse' with the minimum fields required to make a request.
mkAddTagsToStreamResponse ::
  AddTagsToStreamResponse
mkAddTagsToStreamResponse = AddTagsToStreamResponse'
