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
-- Module      : Amazonka.Firehose.ListDeliveryStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Firehose.ListDeliveryStreams
  ( -- * Creating a Request
    ListDeliveryStreams (..),
    newListDeliveryStreams,

    -- * Request Lenses
    listDeliveryStreams_deliveryStreamType,
    listDeliveryStreams_exclusiveStartDeliveryStreamName,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeliveryStreams' smart constructor.
data ListDeliveryStreams = ListDeliveryStreams'
  { -- | The delivery stream type. This can be one of the following values:
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
    -- | The list of delivery streams returned by this call to
    -- @ListDeliveryStreams@ will start with the delivery stream whose name
    -- comes alphabetically immediately after the name you specify in
    -- @ExclusiveStartDeliveryStreamName@.
    exclusiveStartDeliveryStreamName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of delivery streams to list. The default value is 10.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeliveryStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'exclusiveStartDeliveryStreamName', 'listDeliveryStreams_exclusiveStartDeliveryStreamName' - The list of delivery streams returned by this call to
-- @ListDeliveryStreams@ will start with the delivery stream whose name
-- comes alphabetically immediately after the name you specify in
-- @ExclusiveStartDeliveryStreamName@.
--
-- 'limit', 'listDeliveryStreams_limit' - The maximum number of delivery streams to list. The default value is 10.
newListDeliveryStreams ::
  ListDeliveryStreams
newListDeliveryStreams =
  ListDeliveryStreams'
    { deliveryStreamType =
        Prelude.Nothing,
      exclusiveStartDeliveryStreamName = Prelude.Nothing,
      limit = Prelude.Nothing
    }

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

-- | The list of delivery streams returned by this call to
-- @ListDeliveryStreams@ will start with the delivery stream whose name
-- comes alphabetically immediately after the name you specify in
-- @ExclusiveStartDeliveryStreamName@.
listDeliveryStreams_exclusiveStartDeliveryStreamName :: Lens.Lens' ListDeliveryStreams (Prelude.Maybe Prelude.Text)
listDeliveryStreams_exclusiveStartDeliveryStreamName = Lens.lens (\ListDeliveryStreams' {exclusiveStartDeliveryStreamName} -> exclusiveStartDeliveryStreamName) (\s@ListDeliveryStreams' {} a -> s {exclusiveStartDeliveryStreamName = a} :: ListDeliveryStreams)

-- | The maximum number of delivery streams to list. The default value is 10.
listDeliveryStreams_limit :: Lens.Lens' ListDeliveryStreams (Prelude.Maybe Prelude.Natural)
listDeliveryStreams_limit = Lens.lens (\ListDeliveryStreams' {limit} -> limit) (\s@ListDeliveryStreams' {} a -> s {limit = a} :: ListDeliveryStreams)

instance Core.AWSRequest ListDeliveryStreams where
  type
    AWSResponse ListDeliveryStreams =
      ListDeliveryStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeliveryStreamsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "DeliveryStreamNames"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "HasMoreDeliveryStreams")
      )

instance Prelude.Hashable ListDeliveryStreams where
  hashWithSalt _salt ListDeliveryStreams' {..} =
    _salt
      `Prelude.hashWithSalt` deliveryStreamType
      `Prelude.hashWithSalt` exclusiveStartDeliveryStreamName
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListDeliveryStreams where
  rnf ListDeliveryStreams' {..} =
    Prelude.rnf deliveryStreamType `Prelude.seq`
      Prelude.rnf exclusiveStartDeliveryStreamName `Prelude.seq`
        Prelude.rnf limit

instance Data.ToHeaders ListDeliveryStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Firehose_20150804.ListDeliveryStreams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeliveryStreams where
  toJSON ListDeliveryStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryStreamType" Data..=)
              Prelude.<$> deliveryStreamType,
            ("ExclusiveStartDeliveryStreamName" Data..=)
              Prelude.<$> exclusiveStartDeliveryStreamName,
            ("Limit" Data..=) Prelude.<$> limit
          ]
      )

instance Data.ToPath ListDeliveryStreams where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeliveryStreams where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listDeliveryStreamsResponse_deliveryStreamNames = Lens.lens (\ListDeliveryStreamsResponse' {deliveryStreamNames} -> deliveryStreamNames) (\s@ListDeliveryStreamsResponse' {} a -> s {deliveryStreamNames = a} :: ListDeliveryStreamsResponse) Prelude.. Lens.coerced

-- | Indicates whether there are more delivery streams available to list.
listDeliveryStreamsResponse_hasMoreDeliveryStreams :: Lens.Lens' ListDeliveryStreamsResponse Prelude.Bool
listDeliveryStreamsResponse_hasMoreDeliveryStreams = Lens.lens (\ListDeliveryStreamsResponse' {hasMoreDeliveryStreams} -> hasMoreDeliveryStreams) (\s@ListDeliveryStreamsResponse' {} a -> s {hasMoreDeliveryStreams = a} :: ListDeliveryStreamsResponse)

instance Prelude.NFData ListDeliveryStreamsResponse where
  rnf ListDeliveryStreamsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf deliveryStreamNames `Prelude.seq`
        Prelude.rnf hasMoreDeliveryStreams
