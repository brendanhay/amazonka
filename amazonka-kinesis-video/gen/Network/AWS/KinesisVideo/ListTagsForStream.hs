{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListTagsForStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified stream.
--
-- In the request, you must specify either the @StreamName@ or the
-- @StreamARN@.
module Network.AWS.KinesisVideo.ListTagsForStream
  ( -- * Creating a Request
    ListTagsForStream (..),
    newListTagsForStream,

    -- * Request Lenses
    listTagsForStream_nextToken,
    listTagsForStream_streamARN,
    listTagsForStream_streamName,

    -- * Destructuring the Response
    ListTagsForStreamResponse (..),
    newListTagsForStreamResponse,

    -- * Response Lenses
    listTagsForStreamResponse_nextToken,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { -- | If you specify this parameter and the result of a @ListTagsForStream@
    -- call is truncated, the response includes a token that you can use in the
    -- next request to fetch the next batch of tags.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the stream that you want to list tags
    -- for.
    streamARN :: Core.Maybe Core.Text,
    -- | The name of the stream that you want to list tags for.
    streamName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsForStream_nextToken' - If you specify this parameter and the result of a @ListTagsForStream@
-- call is truncated, the response includes a token that you can use in the
-- next request to fetch the next batch of tags.
--
-- 'streamARN', 'listTagsForStream_streamARN' - The Amazon Resource Name (ARN) of the stream that you want to list tags
-- for.
--
-- 'streamName', 'listTagsForStream_streamName' - The name of the stream that you want to list tags for.
newListTagsForStream ::
  ListTagsForStream
newListTagsForStream =
  ListTagsForStream'
    { nextToken = Core.Nothing,
      streamARN = Core.Nothing,
      streamName = Core.Nothing
    }

-- | If you specify this parameter and the result of a @ListTagsForStream@
-- call is truncated, the response includes a token that you can use in the
-- next request to fetch the next batch of tags.
listTagsForStream_nextToken :: Lens.Lens' ListTagsForStream (Core.Maybe Core.Text)
listTagsForStream_nextToken = Lens.lens (\ListTagsForStream' {nextToken} -> nextToken) (\s@ListTagsForStream' {} a -> s {nextToken = a} :: ListTagsForStream)

-- | The Amazon Resource Name (ARN) of the stream that you want to list tags
-- for.
listTagsForStream_streamARN :: Lens.Lens' ListTagsForStream (Core.Maybe Core.Text)
listTagsForStream_streamARN = Lens.lens (\ListTagsForStream' {streamARN} -> streamARN) (\s@ListTagsForStream' {} a -> s {streamARN = a} :: ListTagsForStream)

-- | The name of the stream that you want to list tags for.
listTagsForStream_streamName :: Lens.Lens' ListTagsForStream (Core.Maybe Core.Text)
listTagsForStream_streamName = Lens.lens (\ListTagsForStream' {streamName} -> streamName) (\s@ListTagsForStream' {} a -> s {streamName = a} :: ListTagsForStream)

instance Core.AWSRequest ListTagsForStream where
  type
    AWSResponse ListTagsForStream =
      ListTagsForStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForStreamResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForStream

instance Core.NFData ListTagsForStream

instance Core.ToHeaders ListTagsForStream where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.ToPath ListTagsForStream where
  toPath = Core.const "/listTagsForStream"

instance Core.ToQuery ListTagsForStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { -- | If you specify this parameter and the result of a @ListTags@ call is
    -- truncated, the response includes a token that you can use in the next
    -- request to fetch the next set of tags.
    nextToken :: Core.Maybe Core.Text,
    -- | A map of tag keys and values associated with the specified stream.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTagsForStreamResponse_nextToken' - If you specify this parameter and the result of a @ListTags@ call is
-- truncated, the response includes a token that you can use in the next
-- request to fetch the next set of tags.
--
-- 'tags', 'listTagsForStreamResponse_tags' - A map of tag keys and values associated with the specified stream.
--
-- 'httpStatus', 'listTagsForStreamResponse_httpStatus' - The response's http status code.
newListTagsForStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForStreamResponse
newListTagsForStreamResponse pHttpStatus_ =
  ListTagsForStreamResponse'
    { nextToken =
        Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify this parameter and the result of a @ListTags@ call is
-- truncated, the response includes a token that you can use in the next
-- request to fetch the next set of tags.
listTagsForStreamResponse_nextToken :: Lens.Lens' ListTagsForStreamResponse (Core.Maybe Core.Text)
listTagsForStreamResponse_nextToken = Lens.lens (\ListTagsForStreamResponse' {nextToken} -> nextToken) (\s@ListTagsForStreamResponse' {} a -> s {nextToken = a} :: ListTagsForStreamResponse)

-- | A map of tag keys and values associated with the specified stream.
listTagsForStreamResponse_tags :: Lens.Lens' ListTagsForStreamResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
listTagsForStreamResponse_tags = Lens.lens (\ListTagsForStreamResponse' {tags} -> tags) (\s@ListTagsForStreamResponse' {} a -> s {tags = a} :: ListTagsForStreamResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForStreamResponse_httpStatus :: Lens.Lens' ListTagsForStreamResponse Core.Int
listTagsForStreamResponse_httpStatus = Lens.lens (\ListTagsForStreamResponse' {httpStatus} -> httpStatus) (\s@ListTagsForStreamResponse' {} a -> s {httpStatus = a} :: ListTagsForStreamResponse)

instance Core.NFData ListTagsForStreamResponse
