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
-- Module      : Amazonka.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified Kinesis data stream. This operation has
-- a limit of five transactions per second per account.
module Amazonka.Kinesis.ListTagsForStream
  ( -- * Creating a Request
    ListTagsForStream (..),
    newListTagsForStream,

    -- * Request Lenses
    listTagsForStream_limit,
    listTagsForStream_exclusiveStartTagKey,
    listTagsForStream_streamName,

    -- * Destructuring the Response
    ListTagsForStreamResponse (..),
    newListTagsForStreamResponse,

    -- * Response Lenses
    listTagsForStreamResponse_httpStatus,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_hasMoreTags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for @ListTagsForStream@.
--
-- /See:/ 'newListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { -- | The number of tags to return. If this number is less than the total
    -- number of tags associated with the stream, @HasMoreTags@ is set to
    -- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
    -- key in the response.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The key to use as the starting point for the list of tags. If this
    -- parameter is set, @ListTagsForStream@ gets all tags that occur after
    -- @ExclusiveStartTagKey@.
    exclusiveStartTagKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream.
    streamName :: Prelude.Text
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
-- 'limit', 'listTagsForStream_limit' - The number of tags to return. If this number is less than the total
-- number of tags associated with the stream, @HasMoreTags@ is set to
-- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
-- key in the response.
--
-- 'exclusiveStartTagKey', 'listTagsForStream_exclusiveStartTagKey' - The key to use as the starting point for the list of tags. If this
-- parameter is set, @ListTagsForStream@ gets all tags that occur after
-- @ExclusiveStartTagKey@.
--
-- 'streamName', 'listTagsForStream_streamName' - The name of the stream.
newListTagsForStream ::
  -- | 'streamName'
  Prelude.Text ->
  ListTagsForStream
newListTagsForStream pStreamName_ =
  ListTagsForStream'
    { limit = Prelude.Nothing,
      exclusiveStartTagKey = Prelude.Nothing,
      streamName = pStreamName_
    }

-- | The number of tags to return. If this number is less than the total
-- number of tags associated with the stream, @HasMoreTags@ is set to
-- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
-- key in the response.
listTagsForStream_limit :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Natural)
listTagsForStream_limit = Lens.lens (\ListTagsForStream' {limit} -> limit) (\s@ListTagsForStream' {} a -> s {limit = a} :: ListTagsForStream)

-- | The key to use as the starting point for the list of tags. If this
-- parameter is set, @ListTagsForStream@ gets all tags that occur after
-- @ExclusiveStartTagKey@.
listTagsForStream_exclusiveStartTagKey :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_exclusiveStartTagKey = Lens.lens (\ListTagsForStream' {exclusiveStartTagKey} -> exclusiveStartTagKey) (\s@ListTagsForStream' {} a -> s {exclusiveStartTagKey = a} :: ListTagsForStream)

-- | The name of the stream.
listTagsForStream_streamName :: Lens.Lens' ListTagsForStream Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "HasMoreTags")
      )

instance Prelude.Hashable ListTagsForStream where
  hashWithSalt _salt ListTagsForStream' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` exclusiveStartTagKey
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData ListTagsForStream where
  rnf ListTagsForStream' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf exclusiveStartTagKey
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders ListTagsForStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.ListTagsForStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("ExclusiveStartTagKey" Data..=)
              Prelude.<$> exclusiveStartTagKey,
            Prelude.Just ("StreamName" Data..= streamName)
          ]
      )

instance Data.ToPath ListTagsForStream where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForStream where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output for @ListTagsForStream@.
--
-- /See:/ 'newListTagsForStreamResponse' smart constructor.
data ListTagsForStreamResponse = ListTagsForStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of tags associated with @StreamName@, starting with the first tag
    -- after @ExclusiveStartTagKey@ and up to the specified @Limit@.
    tags :: [Tag],
    -- | If set to @true@, more tags are available. To request additional tags,
    -- set @ExclusiveStartTagKey@ to the key of the last tag returned.
    hasMoreTags :: Prelude.Bool
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
-- 'httpStatus', 'listTagsForStreamResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listTagsForStreamResponse_tags' - A list of tags associated with @StreamName@, starting with the first tag
-- after @ExclusiveStartTagKey@ and up to the specified @Limit@.
--
-- 'hasMoreTags', 'listTagsForStreamResponse_hasMoreTags' - If set to @true@, more tags are available. To request additional tags,
-- set @ExclusiveStartTagKey@ to the key of the last tag returned.
newListTagsForStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hasMoreTags'
  Prelude.Bool ->
  ListTagsForStreamResponse
newListTagsForStreamResponse
  pHttpStatus_
  pHasMoreTags_ =
    ListTagsForStreamResponse'
      { httpStatus =
          pHttpStatus_,
        tags = Prelude.mempty,
        hasMoreTags = pHasMoreTags_
      }

-- | The response's http status code.
listTagsForStreamResponse_httpStatus :: Lens.Lens' ListTagsForStreamResponse Prelude.Int
listTagsForStreamResponse_httpStatus = Lens.lens (\ListTagsForStreamResponse' {httpStatus} -> httpStatus) (\s@ListTagsForStreamResponse' {} a -> s {httpStatus = a} :: ListTagsForStreamResponse)

-- | A list of tags associated with @StreamName@, starting with the first tag
-- after @ExclusiveStartTagKey@ and up to the specified @Limit@.
listTagsForStreamResponse_tags :: Lens.Lens' ListTagsForStreamResponse [Tag]
listTagsForStreamResponse_tags = Lens.lens (\ListTagsForStreamResponse' {tags} -> tags) (\s@ListTagsForStreamResponse' {} a -> s {tags = a} :: ListTagsForStreamResponse) Prelude.. Lens.coerced

-- | If set to @true@, more tags are available. To request additional tags,
-- set @ExclusiveStartTagKey@ to the key of the last tag returned.
listTagsForStreamResponse_hasMoreTags :: Lens.Lens' ListTagsForStreamResponse Prelude.Bool
listTagsForStreamResponse_hasMoreTags = Lens.lens (\ListTagsForStreamResponse' {hasMoreTags} -> hasMoreTags) (\s@ListTagsForStreamResponse' {} a -> s {hasMoreTags = a} :: ListTagsForStreamResponse)

instance Prelude.NFData ListTagsForStreamResponse where
  rnf ListTagsForStreamResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hasMoreTags
