{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Kinesis.ListTagsForStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified Kinesis data stream. This operation has
-- a limit of five transactions per second per account.
module Network.AWS.Kinesis.ListTagsForStream
  ( -- * Creating a Request
    ListTagsForStream (..),
    newListTagsForStream,

    -- * Request Lenses
    listTagsForStream_exclusiveStartTagKey,
    listTagsForStream_limit,
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

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @ListTagsForStream@.
--
-- /See:/ 'newListTagsForStream' smart constructor.
data ListTagsForStream = ListTagsForStream'
  { -- | The key to use as the starting point for the list of tags. If this
    -- parameter is set, @ListTagsForStream@ gets all tags that occur after
    -- @ExclusiveStartTagKey@.
    exclusiveStartTagKey :: Prelude.Maybe Prelude.Text,
    -- | The number of tags to return. If this number is less than the total
    -- number of tags associated with the stream, @HasMoreTags@ is set to
    -- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
    -- key in the response.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stream.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartTagKey', 'listTagsForStream_exclusiveStartTagKey' - The key to use as the starting point for the list of tags. If this
-- parameter is set, @ListTagsForStream@ gets all tags that occur after
-- @ExclusiveStartTagKey@.
--
-- 'limit', 'listTagsForStream_limit' - The number of tags to return. If this number is less than the total
-- number of tags associated with the stream, @HasMoreTags@ is set to
-- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
-- key in the response.
--
-- 'streamName', 'listTagsForStream_streamName' - The name of the stream.
newListTagsForStream ::
  -- | 'streamName'
  Prelude.Text ->
  ListTagsForStream
newListTagsForStream pStreamName_ =
  ListTagsForStream'
    { exclusiveStartTagKey =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      streamName = pStreamName_
    }

-- | The key to use as the starting point for the list of tags. If this
-- parameter is set, @ListTagsForStream@ gets all tags that occur after
-- @ExclusiveStartTagKey@.
listTagsForStream_exclusiveStartTagKey :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Text)
listTagsForStream_exclusiveStartTagKey = Lens.lens (\ListTagsForStream' {exclusiveStartTagKey} -> exclusiveStartTagKey) (\s@ListTagsForStream' {} a -> s {exclusiveStartTagKey = a} :: ListTagsForStream)

-- | The number of tags to return. If this number is less than the total
-- number of tags associated with the stream, @HasMoreTags@ is set to
-- @true@. To list additional tags, set @ExclusiveStartTagKey@ to the last
-- key in the response.
listTagsForStream_limit :: Lens.Lens' ListTagsForStream (Prelude.Maybe Prelude.Natural)
listTagsForStream_limit = Lens.lens (\ListTagsForStream' {limit} -> limit) (\s@ListTagsForStream' {} a -> s {limit = a} :: ListTagsForStream)

-- | The name of the stream.
listTagsForStream_streamName :: Lens.Lens' ListTagsForStream Prelude.Text
listTagsForStream_streamName = Lens.lens (\ListTagsForStream' {streamName} -> streamName) (\s@ListTagsForStream' {} a -> s {streamName = a} :: ListTagsForStream)

instance Prelude.AWSRequest ListTagsForStream where
  type Rs ListTagsForStream = ListTagsForStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..?> "Tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..:> "HasMoreTags")
      )

instance Prelude.Hashable ListTagsForStream

instance Prelude.NFData ListTagsForStream

instance Prelude.ToHeaders ListTagsForStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.ListTagsForStream" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListTagsForStream where
  toJSON ListTagsForStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartTagKey" Prelude..=)
              Prelude.<$> exclusiveStartTagKey,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just ("StreamName" Prelude..= streamName)
          ]
      )

instance Prelude.ToPath ListTagsForStream where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTagsForStream where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listTagsForStreamResponse_tags = Lens.lens (\ListTagsForStreamResponse' {tags} -> tags) (\s@ListTagsForStreamResponse' {} a -> s {tags = a} :: ListTagsForStreamResponse) Prelude.. Prelude._Coerce

-- | If set to @true@, more tags are available. To request additional tags,
-- set @ExclusiveStartTagKey@ to the key of the last tag returned.
listTagsForStreamResponse_hasMoreTags :: Lens.Lens' ListTagsForStreamResponse Prelude.Bool
listTagsForStreamResponse_hasMoreTags = Lens.lens (\ListTagsForStreamResponse' {hasMoreTags} -> hasMoreTags) (\s@ListTagsForStreamResponse' {} a -> s {hasMoreTags = a} :: ListTagsForStreamResponse)

instance Prelude.NFData ListTagsForStreamResponse
