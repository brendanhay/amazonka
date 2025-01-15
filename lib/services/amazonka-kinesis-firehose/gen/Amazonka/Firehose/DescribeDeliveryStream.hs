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
-- Module      : Amazonka.Firehose.DescribeDeliveryStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified delivery stream and its status. For example,
-- after your delivery stream is created, call @DescribeDeliveryStream@ to
-- see whether the delivery stream is @ACTIVE@ and therefore ready for data
-- to be sent to it.
--
-- If the status of a delivery stream is @CREATING_FAILED@, this status
-- doesn\'t change, and you can\'t invoke CreateDeliveryStream again on it.
-- However, you can invoke the DeleteDeliveryStream operation to delete it.
-- If the status is @DELETING_FAILED@, you can force deletion by invoking
-- DeleteDeliveryStream again but with
-- DeleteDeliveryStreamInput$AllowForceDelete set to true.
module Amazonka.Firehose.DescribeDeliveryStream
  ( -- * Creating a Request
    DescribeDeliveryStream (..),
    newDescribeDeliveryStream,

    -- * Request Lenses
    describeDeliveryStream_exclusiveStartDestinationId,
    describeDeliveryStream_limit,
    describeDeliveryStream_deliveryStreamName,

    -- * Destructuring the Response
    DescribeDeliveryStreamResponse (..),
    newDescribeDeliveryStreamResponse,

    -- * Response Lenses
    describeDeliveryStreamResponse_httpStatus,
    describeDeliveryStreamResponse_deliveryStreamDescription,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDeliveryStream' smart constructor.
data DescribeDeliveryStream = DescribeDeliveryStream'
  { -- | The ID of the destination to start returning the destination
    -- information. Kinesis Data Firehose supports one destination per delivery
    -- stream.
    exclusiveStartDestinationId :: Prelude.Maybe Prelude.Text,
    -- | The limit on the number of destinations to return. You can have one
    -- destination per delivery stream.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the delivery stream.
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartDestinationId', 'describeDeliveryStream_exclusiveStartDestinationId' - The ID of the destination to start returning the destination
-- information. Kinesis Data Firehose supports one destination per delivery
-- stream.
--
-- 'limit', 'describeDeliveryStream_limit' - The limit on the number of destinations to return. You can have one
-- destination per delivery stream.
--
-- 'deliveryStreamName', 'describeDeliveryStream_deliveryStreamName' - The name of the delivery stream.
newDescribeDeliveryStream ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  DescribeDeliveryStream
newDescribeDeliveryStream pDeliveryStreamName_ =
  DescribeDeliveryStream'
    { exclusiveStartDestinationId =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | The ID of the destination to start returning the destination
-- information. Kinesis Data Firehose supports one destination per delivery
-- stream.
describeDeliveryStream_exclusiveStartDestinationId :: Lens.Lens' DescribeDeliveryStream (Prelude.Maybe Prelude.Text)
describeDeliveryStream_exclusiveStartDestinationId = Lens.lens (\DescribeDeliveryStream' {exclusiveStartDestinationId} -> exclusiveStartDestinationId) (\s@DescribeDeliveryStream' {} a -> s {exclusiveStartDestinationId = a} :: DescribeDeliveryStream)

-- | The limit on the number of destinations to return. You can have one
-- destination per delivery stream.
describeDeliveryStream_limit :: Lens.Lens' DescribeDeliveryStream (Prelude.Maybe Prelude.Natural)
describeDeliveryStream_limit = Lens.lens (\DescribeDeliveryStream' {limit} -> limit) (\s@DescribeDeliveryStream' {} a -> s {limit = a} :: DescribeDeliveryStream)

-- | The name of the delivery stream.
describeDeliveryStream_deliveryStreamName :: Lens.Lens' DescribeDeliveryStream Prelude.Text
describeDeliveryStream_deliveryStreamName = Lens.lens (\DescribeDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@DescribeDeliveryStream' {} a -> s {deliveryStreamName = a} :: DescribeDeliveryStream)

instance Core.AWSRequest DescribeDeliveryStream where
  type
    AWSResponse DescribeDeliveryStream =
      DescribeDeliveryStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DeliveryStreamDescription")
      )

instance Prelude.Hashable DescribeDeliveryStream where
  hashWithSalt _salt DescribeDeliveryStream' {..} =
    _salt
      `Prelude.hashWithSalt` exclusiveStartDestinationId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` deliveryStreamName

instance Prelude.NFData DescribeDeliveryStream where
  rnf DescribeDeliveryStream' {..} =
    Prelude.rnf exclusiveStartDestinationId `Prelude.seq`
      Prelude.rnf limit `Prelude.seq`
        Prelude.rnf deliveryStreamName

instance Data.ToHeaders DescribeDeliveryStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Firehose_20150804.DescribeDeliveryStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDeliveryStream where
  toJSON DescribeDeliveryStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartDestinationId" Data..=)
              Prelude.<$> exclusiveStartDestinationId,
            ("Limit" Data..=) Prelude.<$> limit,
            Prelude.Just
              ("DeliveryStreamName" Data..= deliveryStreamName)
          ]
      )

instance Data.ToPath DescribeDeliveryStream where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDeliveryStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDeliveryStreamResponse' smart constructor.
data DescribeDeliveryStreamResponse = DescribeDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the delivery stream.
    deliveryStreamDescription :: DeliveryStreamDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeDeliveryStreamResponse_httpStatus' - The response's http status code.
--
-- 'deliveryStreamDescription', 'describeDeliveryStreamResponse_deliveryStreamDescription' - Information about the delivery stream.
newDescribeDeliveryStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deliveryStreamDescription'
  DeliveryStreamDescription ->
  DescribeDeliveryStreamResponse
newDescribeDeliveryStreamResponse
  pHttpStatus_
  pDeliveryStreamDescription_ =
    DescribeDeliveryStreamResponse'
      { httpStatus =
          pHttpStatus_,
        deliveryStreamDescription =
          pDeliveryStreamDescription_
      }

-- | The response's http status code.
describeDeliveryStreamResponse_httpStatus :: Lens.Lens' DescribeDeliveryStreamResponse Prelude.Int
describeDeliveryStreamResponse_httpStatus = Lens.lens (\DescribeDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryStreamResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryStreamResponse)

-- | Information about the delivery stream.
describeDeliveryStreamResponse_deliveryStreamDescription :: Lens.Lens' DescribeDeliveryStreamResponse DeliveryStreamDescription
describeDeliveryStreamResponse_deliveryStreamDescription = Lens.lens (\DescribeDeliveryStreamResponse' {deliveryStreamDescription} -> deliveryStreamDescription) (\s@DescribeDeliveryStreamResponse' {} a -> s {deliveryStreamDescription = a} :: DescribeDeliveryStreamResponse)

instance
  Prelude.NFData
    DescribeDeliveryStreamResponse
  where
  rnf DescribeDeliveryStreamResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf deliveryStreamDescription
