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
-- Module      : Network.AWS.Firehose.UntagDeliveryStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified delivery stream. Removed tags are
-- deleted, and you can\'t recover them after this operation successfully
-- completes.
--
-- If you specify a tag that doesn\'t exist, the operation ignores it.
--
-- This operation has a limit of five transactions per second per account.
module Network.AWS.Firehose.UntagDeliveryStream
  ( -- * Creating a Request
    UntagDeliveryStream (..),
    newUntagDeliveryStream,

    -- * Request Lenses
    untagDeliveryStream_deliveryStreamName,
    untagDeliveryStream_tagKeys,

    -- * Destructuring the Response
    UntagDeliveryStreamResponse (..),
    newUntagDeliveryStreamResponse,

    -- * Response Lenses
    untagDeliveryStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagDeliveryStream' smart constructor.
data UntagDeliveryStream = UntagDeliveryStream'
  { -- | The name of the delivery stream.
    deliveryStreamName :: Core.Text,
    -- | A list of tag keys. Each corresponding tag is removed from the delivery
    -- stream.
    tagKeys :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'untagDeliveryStream_deliveryStreamName' - The name of the delivery stream.
--
-- 'tagKeys', 'untagDeliveryStream_tagKeys' - A list of tag keys. Each corresponding tag is removed from the delivery
-- stream.
newUntagDeliveryStream ::
  -- | 'deliveryStreamName'
  Core.Text ->
  -- | 'tagKeys'
  Core.NonEmpty Core.Text ->
  UntagDeliveryStream
newUntagDeliveryStream pDeliveryStreamName_ pTagKeys_ =
  UntagDeliveryStream'
    { deliveryStreamName =
        pDeliveryStreamName_,
      tagKeys = Lens._Coerce Lens.# pTagKeys_
    }

-- | The name of the delivery stream.
untagDeliveryStream_deliveryStreamName :: Lens.Lens' UntagDeliveryStream Core.Text
untagDeliveryStream_deliveryStreamName = Lens.lens (\UntagDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@UntagDeliveryStream' {} a -> s {deliveryStreamName = a} :: UntagDeliveryStream)

-- | A list of tag keys. Each corresponding tag is removed from the delivery
-- stream.
untagDeliveryStream_tagKeys :: Lens.Lens' UntagDeliveryStream (Core.NonEmpty Core.Text)
untagDeliveryStream_tagKeys = Lens.lens (\UntagDeliveryStream' {tagKeys} -> tagKeys) (\s@UntagDeliveryStream' {} a -> s {tagKeys = a} :: UntagDeliveryStream) Core.. Lens._Coerce

instance Core.AWSRequest UntagDeliveryStream where
  type
    AWSResponse UntagDeliveryStream =
      UntagDeliveryStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagDeliveryStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UntagDeliveryStream

instance Core.NFData UntagDeliveryStream

instance Core.ToHeaders UntagDeliveryStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.UntagDeliveryStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UntagDeliveryStream where
  toJSON UntagDeliveryStream' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DeliveryStreamName" Core..= deliveryStreamName),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath UntagDeliveryStream where
  toPath = Core.const "/"

instance Core.ToQuery UntagDeliveryStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUntagDeliveryStreamResponse' smart constructor.
data UntagDeliveryStreamResponse = UntagDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UntagDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'untagDeliveryStreamResponse_httpStatus' - The response's http status code.
newUntagDeliveryStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UntagDeliveryStreamResponse
newUntagDeliveryStreamResponse pHttpStatus_ =
  UntagDeliveryStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
untagDeliveryStreamResponse_httpStatus :: Lens.Lens' UntagDeliveryStreamResponse Core.Int
untagDeliveryStreamResponse_httpStatus = Lens.lens (\UntagDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@UntagDeliveryStreamResponse' {} a -> s {httpStatus = a} :: UntagDeliveryStreamResponse)

instance Core.NFData UntagDeliveryStreamResponse
