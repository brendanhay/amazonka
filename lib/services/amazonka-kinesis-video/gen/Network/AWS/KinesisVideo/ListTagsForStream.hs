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
    listTagsForStream_streamARN,
    listTagsForStream_nextToken,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { -- | The Amazon Resource Name (ARN) of the stream that you want to list tags
    -- for.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | If you specify this parameter and the result of a @ListTagsForStream@
    -- call is truncated, the response includes a token that you can use in the
    -- next request to fetch the next batch of tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream that you want to list tags for.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'listTagsForStream_streamARN' - The Amazon Resource Name (ARN) of the stream that you want to list tags
-- for.
--
-- 'nextToken', 'listTagsForStream_nextToken' - If you specify this parameter and the result of a @ListTagsForStream@
-- call is truncated, the response includes a token that you can use in the
-- next request to fetch the next batch of tags.
--
-- 'streamName', 'listTagsForStream_streamName' - The name of the stream that you want to list tags for.
newListTagsForStream ::
  ListTagsForStream
newListTagsForStream =
  ListTagsForStream'
    { streamARN = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the stream that you want to list tags
-- for.
listTagsForStream_streamARN :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_streamARN = Lens.lens (\ListTagsForStream' {streamARN} -> streamARN) (\s@ListTagsForStream' {} a -> s {streamARN = a} :: ListTagsForStream)

-- | If you specify this parameter and the result of a @ListTagsForStream@
-- call is truncated, the response includes a token that you can use in the
-- next request to fetch the next batch of tags.
listTagsForStream_nextToken :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_nextToken = Lens.lens (\ListTagsForStream' {nextToken} -> nextToken) (\s@ListTagsForStream' {} a -> s {nextToken = a} :: ListTagsForStream)

-- | The name of the stream that you want to list tags for.
listTagsForStream_streamName :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForStream

instance Prelude.NFData ListTagsForStream

instance Core.ToHeaders ListTagsForStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StreamARN" Core..=) Prelude.<$> streamARN,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("StreamName" Core..=) Prelude.<$> streamName
          ]
      )

instance Core.ToPath ListTagsForStream where
  toPath = Prelude.const "/listTagsForStream"

instance Core.ToQuery ListTagsForStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { -- | If you specify this parameter and the result of a @ListTags@ call is
    -- truncated, the response includes a token that you can use in the next
    -- request to fetch the next set of tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A map of tag keys and values associated with the specified stream.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTagsForStreamResponse
newListTagsForStreamResponse pHttpStatus_ =
  ListTagsForStreamResponse'
    { nextToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify this parameter and the result of a @ListTags@ call is
-- truncated, the response includes a token that you can use in the next
-- request to fetch the next set of tags.
listTagsForStreamResponse_nextToken :: Lens.Lens' ListTagsForStreamResponse (Prelude.Maybe Prelude.Text)
listTagsForStreamResponse_nextToken = Lens.lens (\ListTagsForStreamResponse' {nextToken} -> nextToken) (\s@ListTagsForStreamResponse' {} a -> s {nextToken = a} :: ListTagsForStreamResponse)

-- | A map of tag keys and values associated with the specified stream.
listTagsForStreamResponse_tags :: Lens.Lens' ListTagsForStreamResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsForStreamResponse_tags = Lens.lens (\ListTagsForStreamResponse' {tags} -> tags) (\s@ListTagsForStreamResponse' {} a -> s {tags = a} :: ListTagsForStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForStreamResponse_httpStatus :: Lens.Lens' ListTagsForStreamResponse Prelude.Int
listTagsForStreamResponse_httpStatus = Lens.lens (\ListTagsForStreamResponse' {httpStatus} -> httpStatus) (\s@ListTagsForStreamResponse' {} a -> s {httpStatus = a} :: ListTagsForStreamResponse)

instance Prelude.NFData ListTagsForStreamResponse
