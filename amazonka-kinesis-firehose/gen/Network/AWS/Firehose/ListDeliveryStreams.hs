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

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeliveryStreams' smart constructor.
data ListDeliveryStreams = ListDeliveryStreams'
  { -- | The list of delivery streams returned by this call to
    -- @ListDeliveryStreams@ will start with the delivery stream whose name
    -- comes alphabetically immediately after the name you specify in
    -- @ExclusiveStartDeliveryStreamName@.
    exclusiveStartDeliveryStreamName :: Prelude.Maybe Prelude.Text,
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
    deliveryStreamType :: Prelude.Maybe DeliveryStreamType,
    -- | The maximum number of delivery streams to list. The default value is 10.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      deliveryStreamType = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The list of delivery streams returned by this call to
-- @ListDeliveryStreams@ will start with the delivery stream whose name
-- comes alphabetically immediately after the name you specify in
-- @ExclusiveStartDeliveryStreamName@.
listDeliveryStreams_exclusiveStartDeliveryStreamName :: Lens.Lens' ListDeliveryStreams (Prelude.Maybe Prelude.Text)
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
listDeliveryStreams_deliveryStreamType :: Lens.Lens' ListDeliveryStreams (Prelude.Maybe DeliveryStreamType)
listDeliveryStreams_deliveryStreamType = Lens.lens (\ListDeliveryStreams' {deliveryStreamType} -> deliveryStreamType) (\s@ListDeliveryStreams' {} a -> s {deliveryStreamType = a} :: ListDeliveryStreams)

-- | The maximum number of delivery streams to list. The default value is 10.
listDeliveryStreams_limit :: Lens.Lens' ListDeliveryStreams (Prelude.Maybe Prelude.Natural)
listDeliveryStreams_limit = Lens.lens (\ListDeliveryStreams' {limit} -> limit) (\s@ListDeliveryStreams' {} a -> s {limit = a} :: ListDeliveryStreams)

instance Prelude.AWSRequest ListDeliveryStreams where
  type
    Rs ListDeliveryStreams =
      ListDeliveryStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeliveryStreamsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "DeliveryStreamNames"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:> "HasMoreDeliveryStreams")
      )

instance Prelude.Hashable ListDeliveryStreams

instance Prelude.NFData ListDeliveryStreams

instance Prelude.ToHeaders ListDeliveryStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Firehose_20150804.ListDeliveryStreams" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDeliveryStreams where
  toJSON ListDeliveryStreams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartDeliveryStreamName" Prelude..=)
              Prelude.<$> exclusiveStartDeliveryStreamName,
            ("DeliveryStreamType" Prelude..=)
              Prelude.<$> deliveryStreamType,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath ListDeliveryStreams where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDeliveryStreams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeliveryStreamsResponse' smart constructor.
data ListDeliveryStreamsResponse = ListDeliveryStreamsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The names of the delivery streams.
    deliveryStreamNames :: [Prelude.Text],
    -- | Indicates whether there are more delivery streams available to list.
    hasMoreDeliveryStreams :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'hasMoreDeliveryStreams'
  Prelude.Bool ->
  ListDeliveryStreamsResponse
newListDeliveryStreamsResponse
  pHttpStatus_
  pHasMoreDeliveryStreams_ =
    ListDeliveryStreamsResponse'
      { httpStatus =
          pHttpStatus_,
        deliveryStreamNames = Prelude.mempty,
        hasMoreDeliveryStreams =
          pHasMoreDeliveryStreams_
      }

-- | The response's http status code.
listDeliveryStreamsResponse_httpStatus :: Lens.Lens' ListDeliveryStreamsResponse Prelude.Int
listDeliveryStreamsResponse_httpStatus = Lens.lens (\ListDeliveryStreamsResponse' {httpStatus} -> httpStatus) (\s@ListDeliveryStreamsResponse' {} a -> s {httpStatus = a} :: ListDeliveryStreamsResponse)

-- | The names of the delivery streams.
listDeliveryStreamsResponse_deliveryStreamNames :: Lens.Lens' ListDeliveryStreamsResponse [Prelude.Text]
listDeliveryStreamsResponse_deliveryStreamNames = Lens.lens (\ListDeliveryStreamsResponse' {deliveryStreamNames} -> deliveryStreamNames) (\s@ListDeliveryStreamsResponse' {} a -> s {deliveryStreamNames = a} :: ListDeliveryStreamsResponse) Prelude.. Prelude._Coerce

-- | Indicates whether there are more delivery streams available to list.
listDeliveryStreamsResponse_hasMoreDeliveryStreams :: Lens.Lens' ListDeliveryStreamsResponse Prelude.Bool
listDeliveryStreamsResponse_hasMoreDeliveryStreams = Lens.lens (\ListDeliveryStreamsResponse' {hasMoreDeliveryStreams} -> hasMoreDeliveryStreams) (\s@ListDeliveryStreamsResponse' {} a -> s {hasMoreDeliveryStreams = a} :: ListDeliveryStreamsResponse)

instance Prelude.NFData ListDeliveryStreamsResponse
