{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified Kinesis data stream. Removed tags are deleted and cannot be recovered after this operation successfully completes.
--
-- If you specify a tag that does not exist, it is ignored.
-- 'RemoveTagsFromStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.RemoveTagsFromStream
  ( -- * Creating a request
    RemoveTagsFromStream (..),
    mkRemoveTagsFromStream,

    -- ** Request lenses
    rtfsTagKeys,
    rtfsStreamName,

    -- * Destructuring the response
    RemoveTagsFromStreamResponse (..),
    mkRemoveTagsFromStreamResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for @RemoveTagsFromStream@ .
--
-- /See:/ 'mkRemoveTagsFromStream' smart constructor.
data RemoveTagsFromStream = RemoveTagsFromStream'
  { -- | A list of tag keys. Each corresponding tag is removed from the stream.
    tagKeys :: Lude.NonEmpty Lude.Text,
    -- | The name of the stream.
    streamName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromStream' with the minimum fields required to make a request.
--
-- * 'tagKeys' - A list of tag keys. Each corresponding tag is removed from the stream.
-- * 'streamName' - The name of the stream.
mkRemoveTagsFromStream ::
  -- | 'tagKeys'
  Lude.NonEmpty Lude.Text ->
  -- | 'streamName'
  Lude.Text ->
  RemoveTagsFromStream
mkRemoveTagsFromStream pTagKeys_ pStreamName_ =
  RemoveTagsFromStream'
    { tagKeys = pTagKeys_,
      streamName = pStreamName_
    }

-- | A list of tag keys. Each corresponding tag is removed from the stream.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfsTagKeys :: Lens.Lens' RemoveTagsFromStream (Lude.NonEmpty Lude.Text)
rtfsTagKeys = Lens.lens (tagKeys :: RemoveTagsFromStream -> Lude.NonEmpty Lude.Text) (\s a -> s {tagKeys = a} :: RemoveTagsFromStream)
{-# DEPRECATED rtfsTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfsStreamName :: Lens.Lens' RemoveTagsFromStream Lude.Text
rtfsStreamName = Lens.lens (streamName :: RemoveTagsFromStream -> Lude.Text) (\s a -> s {streamName = a} :: RemoveTagsFromStream)
{-# DEPRECATED rtfsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest RemoveTagsFromStream where
  type Rs RemoveTagsFromStream = RemoveTagsFromStreamResponse
  request = Req.postJSON kinesisService
  response = Res.receiveNull RemoveTagsFromStreamResponse'

instance Lude.ToHeaders RemoveTagsFromStream where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Kinesis_20131202.RemoveTagsFromStream" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveTagsFromStream where
  toJSON RemoveTagsFromStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TagKeys" Lude..= tagKeys),
            Lude.Just ("StreamName" Lude..= streamName)
          ]
      )

instance Lude.ToPath RemoveTagsFromStream where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveTagsFromStream where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveTagsFromStreamResponse' smart constructor.
data RemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveTagsFromStreamResponse' with the minimum fields required to make a request.
mkRemoveTagsFromStreamResponse ::
  RemoveTagsFromStreamResponse
mkRemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse'
