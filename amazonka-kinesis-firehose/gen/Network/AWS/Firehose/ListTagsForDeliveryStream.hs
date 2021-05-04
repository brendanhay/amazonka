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

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTagsForDeliveryStream' smart constructor.
data ListTagsForDeliveryStream = ListTagsForDeliveryStream'
  { -- | The key to use as the starting point for the list of tags. If you set
    -- this parameter, @ListTagsForDeliveryStream@ gets all tags that occur
    -- after @ExclusiveStartTagKey@.
    exclusiveStartTagKey :: Prelude.Maybe Prelude.Text,
    -- | The number of tags to return. If this number is less than the total
    -- number of tags associated with the delivery stream, @HasMoreTags@ is set
    -- to @true@ in the response. To list additional tags, set
    -- @ExclusiveStartTagKey@ to the last key in the response.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the delivery stream whose tags you want to list.
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListTagsForDeliveryStream
newListTagsForDeliveryStream pDeliveryStreamName_ =
  ListTagsForDeliveryStream'
    { exclusiveStartTagKey =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | The key to use as the starting point for the list of tags. If you set
-- this parameter, @ListTagsForDeliveryStream@ gets all tags that occur
-- after @ExclusiveStartTagKey@.
listTagsForDeliveryStream_exclusiveStartTagKey :: Lens.Lens' ListTagsForDeliveryStream (Prelude.Maybe Prelude.Text)
listTagsForDeliveryStream_exclusiveStartTagKey = Lens.lens (\ListTagsForDeliveryStream' {exclusiveStartTagKey} -> exclusiveStartTagKey) (\s@ListTagsForDeliveryStream' {} a -> s {exclusiveStartTagKey = a} :: ListTagsForDeliveryStream)

-- | The number of tags to return. If this number is less than the total
-- number of tags associated with the delivery stream, @HasMoreTags@ is set
-- to @true@ in the response. To list additional tags, set
-- @ExclusiveStartTagKey@ to the last key in the response.
listTagsForDeliveryStream_limit :: Lens.Lens' ListTagsForDeliveryStream (Prelude.Maybe Prelude.Natural)
listTagsForDeliveryStream_limit = Lens.lens (\ListTagsForDeliveryStream' {limit} -> limit) (\s@ListTagsForDeliveryStream' {} a -> s {limit = a} :: ListTagsForDeliveryStream)

-- | The name of the delivery stream whose tags you want to list.
listTagsForDeliveryStream_deliveryStreamName :: Lens.Lens' ListTagsForDeliveryStream Prelude.Text
listTagsForDeliveryStream_deliveryStreamName = Lens.lens (\ListTagsForDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@ListTagsForDeliveryStream' {} a -> s {deliveryStreamName = a} :: ListTagsForDeliveryStream)

instance Prelude.AWSRequest ListTagsForDeliveryStream where
  type
    Rs ListTagsForDeliveryStream =
      ListTagsForDeliveryStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForDeliveryStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..?> "Tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..:> "HasMoreTags")
      )

instance Prelude.Hashable ListTagsForDeliveryStream

instance Prelude.NFData ListTagsForDeliveryStream

instance Prelude.ToHeaders ListTagsForDeliveryStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Firehose_20150804.ListTagsForDeliveryStream" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListTagsForDeliveryStream where
  toJSON ListTagsForDeliveryStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartTagKey" Prelude..=)
              Prelude.<$> exclusiveStartTagKey,
            ("Limit" Prelude..=) Prelude.<$> limit,
            Prelude.Just
              ( "DeliveryStreamName"
                  Prelude..= deliveryStreamName
              )
          ]
      )

instance Prelude.ToPath ListTagsForDeliveryStream where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListTagsForDeliveryStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForDeliveryStreamResponse' smart constructor.
data ListTagsForDeliveryStreamResponse = ListTagsForDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of tags associated with @DeliveryStreamName@, starting with the
    -- first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@.
    tags :: [Tag],
    -- | If this is @true@ in the response, more tags are available. To list the
    -- remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag
    -- returned and call @ListTagsForDeliveryStream@ again.
    hasMoreTags :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'hasMoreTags'
  Prelude.Bool ->
  ListTagsForDeliveryStreamResponse
newListTagsForDeliveryStreamResponse
  pHttpStatus_
  pHasMoreTags_ =
    ListTagsForDeliveryStreamResponse'
      { httpStatus =
          pHttpStatus_,
        tags = Prelude.mempty,
        hasMoreTags = pHasMoreTags_
      }

-- | The response's http status code.
listTagsForDeliveryStreamResponse_httpStatus :: Lens.Lens' ListTagsForDeliveryStreamResponse Prelude.Int
listTagsForDeliveryStreamResponse_httpStatus = Lens.lens (\ListTagsForDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@ListTagsForDeliveryStreamResponse' {} a -> s {httpStatus = a} :: ListTagsForDeliveryStreamResponse)

-- | A list of tags associated with @DeliveryStreamName@, starting with the
-- first tag after @ExclusiveStartTagKey@ and up to the specified @Limit@.
listTagsForDeliveryStreamResponse_tags :: Lens.Lens' ListTagsForDeliveryStreamResponse [Tag]
listTagsForDeliveryStreamResponse_tags = Lens.lens (\ListTagsForDeliveryStreamResponse' {tags} -> tags) (\s@ListTagsForDeliveryStreamResponse' {} a -> s {tags = a} :: ListTagsForDeliveryStreamResponse) Prelude.. Prelude._Coerce

-- | If this is @true@ in the response, more tags are available. To list the
-- remaining tags, set @ExclusiveStartTagKey@ to the key of the last tag
-- returned and call @ListTagsForDeliveryStream@ again.
listTagsForDeliveryStreamResponse_hasMoreTags :: Lens.Lens' ListTagsForDeliveryStreamResponse Prelude.Bool
listTagsForDeliveryStreamResponse_hasMoreTags = Lens.lens (\ListTagsForDeliveryStreamResponse' {hasMoreTags} -> hasMoreTags) (\s@ListTagsForDeliveryStreamResponse' {} a -> s {hasMoreTags = a} :: ListTagsForDeliveryStreamResponse)

instance
  Prelude.NFData
    ListTagsForDeliveryStreamResponse
