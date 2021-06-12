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
-- Module      : Network.AWS.Firehose.ListTagsForDeliveryStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags for the specified delivery stream. This operation has a
-- limit of five transactions per second per account.
module Network.AWS.Firehose.ListTagsForDeliveryStream
  ( -- * Creating a Request
    ListTagsForDeliveryStream (..),
    newListTagsForDeliveryStream,

    -- * Request Lenses
    listTagsForDeliveryStream_exclusiveStartTagKey,
    listTagsForDeliveryStream_limit,
    listTagsForDeliveryStream_deliveryStreamName,

    -- * Destructuring the Response
    ListTagsForDeliveryStreamResponse (..),
    newListTagsForDeliveryStreamResponse,

    -- * Response Lenses
    listTagsForDeliveryStreamResponse_httpStatus,
    listTagsForDeliveryStreamResponse_tags,
    listTagsForDeliveryStreamResponse_hasMoreTags,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForDeliveryStream' smart constructor.
data ListTagsForDeliveryStream = ListTagsForDeliveryStream'
  { -- | The key to use as the starting point for the list of tags. If you set
    -- this parameter, @ListTagsForDeliveryStream@ gets all tags that occur
    -- after @ExclusiveStartTagKey@.
    exclusiveStartTagKey :: Core.Maybe Core.Text,
    -- | The number of tags to return. If this number is less than the total
    -- number of tags associated with the delivery stream, @HasMoreTags@ is set
    -- to @true@ in the response. To list additional tags, set
    -- @ExclusiveStartTagKey@ to the last key in the response.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the delivery stream whose tags you want to list.
    deliveryStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartTagKey', 'listTagsForDeliveryStream_exclusiveStartTagKey' - The key to use as the starting point for the list of tags. If you set
-- this parameter, @ListTagsForDeliveryStream@ gets all tags that occur
-- after @ExclusiveStartTagKey@.
--
-- 'limit', 'listTagsForDeliveryStream_limit' - The number of tags to return. If this number is less than the total
-- number of tags associated with the delivery stream, @HasMoreTags@ is set
-- to @true@ in the response. To list additional tags, set
-- @ExclusiveStartTagKey@ to the last key in the response.
--
-- 'deliveryStreamName', 'listTagsForDeliveryStream_deliveryStreamName' - The name of the delivery stream whose tags you want to list.
newListTagsForDeliveryStream ::
  -- | 'deliveryStreamName'
  Core.Text ->
  ListTagsForDeliveryStream
newListTagsForDeliveryStream pDeliveryStreamName_ =
  ListTagsForDeliveryStream'
    { exclusiveStartTagKey =
        Core.Nothing,
      limit = Core.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | The key to use as the starting point for the list of tags. If you set
-- this parameter, @ListTagsForDeliveryStream@ gets all tags that occur
-- after @ExclusiveStartTagKey@.
listTagsForDeliveryStream_exclusiveStartTagKey :: Lens.Lens' ListTagsForDeliveryStream (Core.Maybe Core.Text)
listTagsForDeliveryStream_exclusiveStartTagKey = Lens.lens (\ListTagsForDeliveryStream' {exclusiveStartTagKey} -> exclusiveStartTagKey) (\s@ListTagsForDeliveryStream' {} a -> s {exclusiveStartTagKey = a} :: ListTagsForDeliveryStream)

-- | The number of tags to return. If this number is less than the total
-- number of tags associated with the delivery stream, @HasMoreTags@ is set
-- to @true@ in the response. To list additional tags, set
-- @ExclusiveStartTagKey@ to the last key in the response.
listTagsForDeliveryStream_limit :: Lens.Lens' ListTagsForDeliveryStream (Core.Maybe Core.Natural)
listTagsForDeliveryStream_limit = Lens.lens (\ListTagsForDeliveryStream' {limit} -> limit) (\s@ListTagsForDeliveryStream' {} a -> s {limit = a} :: ListTagsForDeliveryStream)

-- | The name of the delivery stream whose tags you want to list.
listTagsForDeliveryStream_deliveryStreamName :: Lens.Lens' ListTagsForDeliveryStream Core.Text
listTagsForDeliveryStream_deliveryStreamName = Lens.lens (\ListTagsForDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@ListTagsForDeliveryStream' {} a -> s {deliveryStreamName = a} :: ListTagsForDeliveryStream)

instance Core.AWSRequest ListTagsForDeliveryStream where
  type
    AWSResponse ListTagsForDeliveryStream =
      ListTagsForDeliveryStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForDeliveryStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..:> "HasMoreTags")
      )

instance Core.Hashable ListTagsForDeliveryStream

instance Core.NFData ListTagsForDeliveryStream

instance Core.ToHeaders ListTagsForDeliveryStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.ListTagsForDeliveryStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTagsForDeliveryStream where
  toJSON ListTagsForDeliveryStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartTagKey" Core..=)
              Core.<$> exclusiveStartTagKey,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ("DeliveryStreamName" Core..= deliveryStreamName)
          ]
      )

instance Core.ToPath ListTagsForDeliveryStream where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForDeliveryStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTagsForDeliveryStreamResponse' smart constructor.
data ListTagsForDeliveryStreamResponse = ListTagsForDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of tags associated with @DeliveryStreamName@, starting with the
    -- first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@.
    tags :: [Tag],
    -- | If this is @true@ in the response, more tags are available. To list the
    -- remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag
    -- returned and call @ListTagsForDeliveryStream@ again.
    hasMoreTags :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTagsForDeliveryStreamResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listTagsForDeliveryStreamResponse_tags' - A list of tags associated with @DeliveryStreamName@, starting with the
-- first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@.
--
-- 'hasMoreTags', 'listTagsForDeliveryStreamResponse_hasMoreTags' - If this is @true@ in the response, more tags are available. To list the
-- remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag
-- returned and call @ListTagsForDeliveryStream@ again.
newListTagsForDeliveryStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'hasMoreTags'
  Core.Bool ->
  ListTagsForDeliveryStreamResponse
newListTagsForDeliveryStreamResponse
  pHttpStatus_
  pHasMoreTags_ =
    ListTagsForDeliveryStreamResponse'
      { httpStatus =
          pHttpStatus_,
        tags = Core.mempty,
        hasMoreTags = pHasMoreTags_
      }

-- | The response's http status code.
listTagsForDeliveryStreamResponse_httpStatus :: Lens.Lens' ListTagsForDeliveryStreamResponse Core.Int
listTagsForDeliveryStreamResponse_httpStatus = Lens.lens (\ListTagsForDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@ListTagsForDeliveryStreamResponse' {} a -> s {httpStatus = a} :: ListTagsForDeliveryStreamResponse)

-- | A list of tags associated with @DeliveryStreamName@, starting with the
-- first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@.
listTagsForDeliveryStreamResponse_tags :: Lens.Lens' ListTagsForDeliveryStreamResponse [Tag]
listTagsForDeliveryStreamResponse_tags = Lens.lens (\ListTagsForDeliveryStreamResponse' {tags} -> tags) (\s@ListTagsForDeliveryStreamResponse' {} a -> s {tags = a} :: ListTagsForDeliveryStreamResponse) Core.. Lens._Coerce

-- | If this is @true@ in the response, more tags are available. To list the
-- remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag
-- returned and call @ListTagsForDeliveryStream@ again.
listTagsForDeliveryStreamResponse_hasMoreTags :: Lens.Lens' ListTagsForDeliveryStreamResponse Core.Bool
listTagsForDeliveryStreamResponse_hasMoreTags = Lens.lens (\ListTagsForDeliveryStreamResponse' {hasMoreTags} -> hasMoreTags) (\s@ListTagsForDeliveryStreamResponse' {} a -> s {hasMoreTags = a} :: ListTagsForDeliveryStreamResponse)

instance
  Core.NFData
    ListTagsForDeliveryStreamResponse
