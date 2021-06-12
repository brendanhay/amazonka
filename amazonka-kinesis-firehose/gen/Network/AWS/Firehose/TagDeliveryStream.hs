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
-- Module      : Network.AWS.Firehose.TagDeliveryStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified delivery stream. A tag is a
-- key-value pair that you can define and assign to AWS resources. If you
-- specify a tag that already exists, the tag value is replaced with the
-- value that you specify in the request. Tags are metadata. For example,
-- you can add friendly names and descriptions or other types of
-- information that can help you distinguish the delivery stream. For more
-- information about tags, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- Each delivery stream can have up to 50 tags.
--
-- This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.TagDeliveryStream
  ( -- * Creating a Request
    TagDeliveryStream (..),
    newTagDeliveryStream,

    -- * Request Lenses
    tagDeliveryStream_deliveryStreamName,
    tagDeliveryStream_tags,

    -- * Destructuring the Response
    TagDeliveryStreamResponse (..),
    newTagDeliveryStreamResponse,

    -- * Response Lenses
    tagDeliveryStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagDeliveryStream' smart constructor.
data TagDeliveryStream = TagDeliveryStream'
  { -- | The name of the delivery stream to which you want to add the tags.
    deliveryStreamName :: Core.Text,
    -- | A set of key-value pairs to use to create the tags.
    tags :: Core.NonEmpty Tag
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'tagDeliveryStream_deliveryStreamName' - The name of the delivery stream to which you want to add the tags.
--
-- 'tags', 'tagDeliveryStream_tags' - A set of key-value pairs to use to create the tags.
newTagDeliveryStream ::
  -- | 'deliveryStreamName'
  Core.Text ->
  -- | 'tags'
  Core.NonEmpty Tag ->
  TagDeliveryStream
newTagDeliveryStream pDeliveryStreamName_ pTags_ =
  TagDeliveryStream'
    { deliveryStreamName =
        pDeliveryStreamName_,
      tags = Lens._Coerce Lens.# pTags_
    }

-- | The name of the delivery stream to which you want to add the tags.
tagDeliveryStream_deliveryStreamName :: Lens.Lens' TagDeliveryStream Core.Text
tagDeliveryStream_deliveryStreamName = Lens.lens (\TagDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@TagDeliveryStream' {} a -> s {deliveryStreamName = a} :: TagDeliveryStream)

-- | A set of key-value pairs to use to create the tags.
tagDeliveryStream_tags :: Lens.Lens' TagDeliveryStream (Core.NonEmpty Tag)
tagDeliveryStream_tags = Lens.lens (\TagDeliveryStream' {tags} -> tags) (\s@TagDeliveryStream' {} a -> s {tags = a} :: TagDeliveryStream) Core.. Lens._Coerce

instance Core.AWSRequest TagDeliveryStream where
  type
    AWSResponse TagDeliveryStream =
      TagDeliveryStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          TagDeliveryStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable TagDeliveryStream

instance Core.NFData TagDeliveryStream

instance Core.ToHeaders TagDeliveryStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.TagDeliveryStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TagDeliveryStream where
  toJSON TagDeliveryStream' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DeliveryStreamName" Core..= deliveryStreamName),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.ToPath TagDeliveryStream where
  toPath = Core.const "/"

instance Core.ToQuery TagDeliveryStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTagDeliveryStreamResponse' smart constructor.
data TagDeliveryStreamResponse = TagDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'tagDeliveryStreamResponse_httpStatus' - The response's http status code.
newTagDeliveryStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  TagDeliveryStreamResponse
newTagDeliveryStreamResponse pHttpStatus_ =
  TagDeliveryStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
tagDeliveryStreamResponse_httpStatus :: Lens.Lens' TagDeliveryStreamResponse Core.Int
tagDeliveryStreamResponse_httpStatus = Lens.lens (\TagDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@TagDeliveryStreamResponse' {} a -> s {httpStatus = a} :: TagDeliveryStreamResponse)

instance Core.NFData TagDeliveryStreamResponse
