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
-- Module      : Amazonka.KinesisVideo.ListTagsForStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags associated with the specified stream.
--
-- In the request, you must specify either the @StreamName@ or the
-- @StreamARN@.
module Amazonka.KinesisVideo.ListTagsForStream
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
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_nextToken,
    listTagsForStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { -- | If you specify this parameter and the result of a @ListTagsForStream@
    -- call is truncated, the response includes a token that you can use in the
    -- next request to fetch the next batch of tags.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stream that you want to list tags
    -- for.
    streamARN :: Prelude.Maybe Prelude.Text,
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
    { nextToken = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | If you specify this parameter and the result of a @ListTagsForStream@
-- call is truncated, the response includes a token that you can use in the
-- next request to fetch the next batch of tags.
listTagsForStream_nextToken :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_nextToken = Lens.lens (\ListTagsForStream' {nextToken} -> nextToken) (\s@ListTagsForStream' {} a -> s {nextToken = a} :: ListTagsForStream)

-- | The Amazon Resource Name (ARN) of the stream that you want to list tags
-- for.
listTagsForStream_streamARN :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_streamARN = Lens.lens (\ListTagsForStream' {streamARN} -> streamARN) (\s@ListTagsForStream' {} a -> s {streamARN = a} :: ListTagsForStream)

-- | The name of the stream that you want to list tags for.
listTagsForStream_streamName :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_streamName = Lens.lens (\ListTagsForStream' {streamName} -> streamName) (\s@ListTagsForStream' {} a -> s {streamName = a} :: ListTagsForStream)

instance Core.AWSRequest ListTagsForStream where
  type
    AWSResponse ListTagsForStream =
      ListTagsForStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForStreamResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForStream where
  hashWithSalt _salt ListTagsForStream' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData ListTagsForStream where
  rnf ListTagsForStream' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders ListTagsForStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath ListTagsForStream where
  toPath = Prelude.const "/listTagsForStream"

instance Data.ToQuery ListTagsForStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { -- | A map of tag keys and values associated with the specified stream.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If you specify this parameter and the result of a @ListTags@ call is
    -- truncated, the response includes a token that you can use in the next
    -- request to fetch the next set of tags.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'tags', 'listTagsForStreamResponse_tags' - A map of tag keys and values associated with the specified stream.
--
-- 'nextToken', 'listTagsForStreamResponse_nextToken' - If you specify this parameter and the result of a @ListTags@ call is
-- truncated, the response includes a token that you can use in the next
-- request to fetch the next set of tags.
--
-- 'httpStatus', 'listTagsForStreamResponse_httpStatus' - The response's http status code.
newListTagsForStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForStreamResponse
newListTagsForStreamResponse pHttpStatus_ =
  ListTagsForStreamResponse'
    { tags = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of tag keys and values associated with the specified stream.
listTagsForStreamResponse_tags :: Lens.Lens' ListTagsForStreamResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listTagsForStreamResponse_tags = Lens.lens (\ListTagsForStreamResponse' {tags} -> tags) (\s@ListTagsForStreamResponse' {} a -> s {tags = a} :: ListTagsForStreamResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you specify this parameter and the result of a @ListTags@ call is
-- truncated, the response includes a token that you can use in the next
-- request to fetch the next set of tags.
listTagsForStreamResponse_nextToken :: Lens.Lens' ListTagsForStreamResponse (Prelude.Maybe Prelude.Text)
listTagsForStreamResponse_nextToken = Lens.lens (\ListTagsForStreamResponse' {nextToken} -> nextToken) (\s@ListTagsForStreamResponse' {} a -> s {nextToken = a} :: ListTagsForStreamResponse)

-- | The response's http status code.
listTagsForStreamResponse_httpStatus :: Lens.Lens' ListTagsForStreamResponse Prelude.Int
listTagsForStreamResponse_httpStatus = Lens.lens (\ListTagsForStreamResponse' {httpStatus} -> httpStatus) (\s@ListTagsForStreamResponse' {} a -> s {httpStatus = a} :: ListTagsForStreamResponse)

instance Prelude.NFData ListTagsForStreamResponse where
  rnf ListTagsForStreamResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
