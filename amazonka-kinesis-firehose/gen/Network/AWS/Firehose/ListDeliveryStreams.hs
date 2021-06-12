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
-- Module      : Network.AWS.Firehose.ListDeliveryStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your delivery streams in alphabetical order of their names.
--
-- The number of delivery streams might be too large to return using a
-- single call to @ListDeliveryStreams@. You can limit the number of
-- delivery streams returned, using the @Limit@ parameter. To determine
-- whether there are more delivery streams to list, check the value of
-- @HasMoreDeliveryStreams@ in the output. If there are more delivery
-- streams to list, you can request them by calling this operation again
-- and setting the @ExclusiveStartDeliveryStreamName@ parameter to the name
-- of the last delivery stream returned in the last call.
module Network.AWS.Firehose.ListDeliveryStreams
  ( -- * Creating a Request
    ListDeliveryStreams (..),
    newListDeliveryStreams,

    -- * Request Lenses
    listDeliveryStreams_exclusiveStartDeliveryStreamName,
    listDeliveryStreams_deliveryStreamType,
    listDeliveryStreams_limit,

    -- * Destructuring the Response
    ListDeliveryStreamsResponse (..),
    newListDeliveryStreamsResponse,

    -- * Response Lenses
    listDeliveryStreamsResponse_httpStatus,
    listDeliveryStreamsResponse_deliveryStreamNames,
    listDeliveryStreamsResponse_hasMoreDeliveryStreams,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeliveryStreams' smart constructor.
data ListDeliveryStreams = ListDeliveryStreams'
  { -- | The list of delivery streams returned by this call to
    -- @ListDeliveryStreams@ will start with the delivery stream whose name
    -- comes alphabetically immediately after the name you specify in
    -- @ExclusiveStartDeliveryStreamName@.
    exclusiveStartDeliveryStreamName :: Core.Maybe Core.Text,
    -- | The delivery stream type. This can be one of the following values:
    --
    -- -   @DirectPut@: Provider applications access the delivery stream
    --     directly.
    --
    -- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
    --     stream as a source.
    --
    -- This parameter is optional. If this parameter is omitted, delivery
    -- streams of all types are returned.
    deliveryStreamType :: Core.Maybe DeliveryStreamType,
    -- | The maximum number of delivery streams to list. The default value is 10.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeliveryStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartDeliveryStreamName', 'listDeliveryStreams_exclusiveStartDeliveryStreamName' - The list of delivery streams returned by this call to
-- @ListDeliveryStreams@ will start with the delivery stream whose name
-- comes alphabetically immediately after the name you specify in
-- @ExclusiveStartDeliveryStreamName@.
--
-- 'deliveryStreamType', 'listDeliveryStreams_deliveryStreamType' - The delivery stream type. This can be one of the following values:
--
-- -   @DirectPut@: Provider applications access the delivery stream
--     directly.
--
-- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
--     stream as a source.
--
-- This parameter is optional. If this parameter is omitted, delivery
-- streams of all types are returned.
--
-- 'limit', 'listDeliveryStreams_limit' - The maximum number of delivery streams to list. The default value is 10.
newListDeliveryStreams ::
  ListDeliveryStreams
newListDeliveryStreams =
  ListDeliveryStreams'
    { exclusiveStartDeliveryStreamName =
        Core.Nothing,
      deliveryStreamType = Core.Nothing,
      limit = Core.Nothing
    }

-- | The list of delivery streams returned by this call to
-- @ListDeliveryStreams@ will start with the delivery stream whose name
-- comes alphabetically immediately after the name you specify in
-- @ExclusiveStartDeliveryStreamName@.
listDeliveryStreams_exclusiveStartDeliveryStreamName :: Lens.Lens' ListDeliveryStreams (Core.Maybe Core.Text)
listDeliveryStreams_exclusiveStartDeliveryStreamName = Lens.lens (\ListDeliveryStreams' {exclusiveStartDeliveryStreamName} -> exclusiveStartDeliveryStreamName) (\s@ListDeliveryStreams' {} a -> s {exclusiveStartDeliveryStreamName = a} :: ListDeliveryStreams)

-- | The delivery stream type. This can be one of the following values:
--
-- -   @DirectPut@: Provider applications access the delivery stream
--     directly.
--
-- -   @KinesisStreamAsSource@: The delivery stream uses a Kinesis data
--     stream as a source.
--
-- This parameter is optional. If this parameter is omitted, delivery
-- streams of all types are returned.
listDeliveryStreams_deliveryStreamType :: Lens.Lens' ListDeliveryStreams (Core.Maybe DeliveryStreamType)
listDeliveryStreams_deliveryStreamType = Lens.lens (\ListDeliveryStreams' {deliveryStreamType} -> deliveryStreamType) (\s@ListDeliveryStreams' {} a -> s {deliveryStreamType = a} :: ListDeliveryStreams)

-- | The maximum number of delivery streams to list. The default value is 10.
listDeliveryStreams_limit :: Lens.Lens' ListDeliveryStreams (Core.Maybe Core.Natural)
listDeliveryStreams_limit = Lens.lens (\ListDeliveryStreams' {limit} -> limit) (\s@ListDeliveryStreams' {} a -> s {limit = a} :: ListDeliveryStreams)

instance Core.AWSRequest ListDeliveryStreams where
  type
    AWSResponse ListDeliveryStreams =
      ListDeliveryStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeliveryStreamsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "DeliveryStreamNames"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..:> "HasMoreDeliveryStreams")
      )

instance Core.Hashable ListDeliveryStreams

instance Core.NFData ListDeliveryStreams

instance Core.ToHeaders ListDeliveryStreams where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.ListDeliveryStreams" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDeliveryStreams where
  toJSON ListDeliveryStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExclusiveStartDeliveryStreamName" Core..=)
              Core.<$> exclusiveStartDeliveryStreamName,
            ("DeliveryStreamType" Core..=)
              Core.<$> deliveryStreamType,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListDeliveryStreams where
  toPath = Core.const "/"

instance Core.ToQuery ListDeliveryStreams where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDeliveryStreamsResponse' smart constructor.
data ListDeliveryStreamsResponse = ListDeliveryStreamsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The names of the delivery streams.
    deliveryStreamNames :: [Core.Text],
    -- | Indicates whether there are more delivery streams available to list.
    hasMoreDeliveryStreams :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeliveryStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listDeliveryStreamsResponse_httpStatus' - The response's http status code.
--
-- 'deliveryStreamNames', 'listDeliveryStreamsResponse_deliveryStreamNames' - The names of the delivery streams.
--
-- 'hasMoreDeliveryStreams', 'listDeliveryStreamsResponse_hasMoreDeliveryStreams' - Indicates whether there are more delivery streams available to list.
newListDeliveryStreamsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'hasMoreDeliveryStreams'
  Core.Bool ->
  ListDeliveryStreamsResponse
newListDeliveryStreamsResponse
  pHttpStatus_
  pHasMoreDeliveryStreams_ =
    ListDeliveryStreamsResponse'
      { httpStatus =
          pHttpStatus_,
        deliveryStreamNames = Core.mempty,
        hasMoreDeliveryStreams =
          pHasMoreDeliveryStreams_
      }

-- | The response's http status code.
listDeliveryStreamsResponse_httpStatus :: Lens.Lens' ListDeliveryStreamsResponse Core.Int
listDeliveryStreamsResponse_httpStatus = Lens.lens (\ListDeliveryStreamsResponse' {httpStatus} -> httpStatus) (\s@ListDeliveryStreamsResponse' {} a -> s {httpStatus = a} :: ListDeliveryStreamsResponse)

-- | The names of the delivery streams.
listDeliveryStreamsResponse_deliveryStreamNames :: Lens.Lens' ListDeliveryStreamsResponse [Core.Text]
listDeliveryStreamsResponse_deliveryStreamNames = Lens.lens (\ListDeliveryStreamsResponse' {deliveryStreamNames} -> deliveryStreamNames) (\s@ListDeliveryStreamsResponse' {} a -> s {deliveryStreamNames = a} :: ListDeliveryStreamsResponse) Core.. Lens._Coerce

-- | Indicates whether there are more delivery streams available to list.
listDeliveryStreamsResponse_hasMoreDeliveryStreams :: Lens.Lens' ListDeliveryStreamsResponse Core.Bool
listDeliveryStreamsResponse_hasMoreDeliveryStreams = Lens.lens (\ListDeliveryStreamsResponse' {hasMoreDeliveryStreams} -> hasMoreDeliveryStreams) (\s@ListDeliveryStreamsResponse' {} a -> s {hasMoreDeliveryStreams = a} :: ListDeliveryStreamsResponse)

instance Core.NFData ListDeliveryStreamsResponse
