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
-- Module      : Amazonka.Firehose.StopDeliveryStreamEncryption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables server-side encryption (SSE) for the delivery stream.
--
-- This operation is asynchronous. It returns immediately. When you invoke
-- it, Kinesis Data Firehose first sets the encryption status of the stream
-- to @DISABLING@, and then to @DISABLED@. You can continue to read and
-- write data to your stream while its status is @DISABLING@. It can take
-- up to 5 seconds after the encryption status changes to @DISABLED@ before
-- all records written to the delivery stream are no longer subject to
-- encryption. To find out whether a record or a batch of records was
-- encrypted, check the response elements PutRecordOutput$Encrypted and
-- PutRecordBatchOutput$Encrypted, respectively.
--
-- To check the encryption state of a delivery stream, use
-- DescribeDeliveryStream.
--
-- If SSE is enabled using a customer managed CMK and then you invoke
-- @StopDeliveryStreamEncryption@, Kinesis Data Firehose schedules the
-- related KMS grant for retirement and then retires it after it ensures
-- that it is finished delivering records to the destination.
--
-- The @StartDeliveryStreamEncryption@ and @StopDeliveryStreamEncryption@
-- operations have a combined limit of 25 calls per delivery stream per 24
-- hours. For example, you reach the limit if you call
-- @StartDeliveryStreamEncryption@ 13 times and
-- @StopDeliveryStreamEncryption@ 12 times for the same delivery stream in
-- a 24-hour period.
module Amazonka.Firehose.StopDeliveryStreamEncryption
  ( -- * Creating a Request
    StopDeliveryStreamEncryption (..),
    newStopDeliveryStreamEncryption,

    -- * Request Lenses
    stopDeliveryStreamEncryption_deliveryStreamName,

    -- * Destructuring the Response
    StopDeliveryStreamEncryptionResponse (..),
    newStopDeliveryStreamEncryptionResponse,

    -- * Response Lenses
    stopDeliveryStreamEncryptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopDeliveryStreamEncryption' smart constructor.
data StopDeliveryStreamEncryption = StopDeliveryStreamEncryption'
  { -- | The name of the delivery stream for which you want to disable
    -- server-side encryption (SSE).
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDeliveryStreamEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamName', 'stopDeliveryStreamEncryption_deliveryStreamName' - The name of the delivery stream for which you want to disable
-- server-side encryption (SSE).
newStopDeliveryStreamEncryption ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  StopDeliveryStreamEncryption
newStopDeliveryStreamEncryption pDeliveryStreamName_ =
  StopDeliveryStreamEncryption'
    { deliveryStreamName =
        pDeliveryStreamName_
    }

-- | The name of the delivery stream for which you want to disable
-- server-side encryption (SSE).
stopDeliveryStreamEncryption_deliveryStreamName :: Lens.Lens' StopDeliveryStreamEncryption Prelude.Text
stopDeliveryStreamEncryption_deliveryStreamName = Lens.lens (\StopDeliveryStreamEncryption' {deliveryStreamName} -> deliveryStreamName) (\s@StopDeliveryStreamEncryption' {} a -> s {deliveryStreamName = a} :: StopDeliveryStreamEncryption)

instance Core.AWSRequest StopDeliveryStreamEncryption where
  type
    AWSResponse StopDeliveryStreamEncryption =
      StopDeliveryStreamEncryptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopDeliveryStreamEncryptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StopDeliveryStreamEncryption
  where
  hashWithSalt _salt StopDeliveryStreamEncryption' {..} =
    _salt `Prelude.hashWithSalt` deliveryStreamName

instance Prelude.NFData StopDeliveryStreamEncryption where
  rnf StopDeliveryStreamEncryption' {..} =
    Prelude.rnf deliveryStreamName

instance Data.ToHeaders StopDeliveryStreamEncryption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Firehose_20150804.StopDeliveryStreamEncryption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopDeliveryStreamEncryption where
  toJSON StopDeliveryStreamEncryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryStreamName" Data..= deliveryStreamName)
          ]
      )

instance Data.ToPath StopDeliveryStreamEncryption where
  toPath = Prelude.const "/"

instance Data.ToQuery StopDeliveryStreamEncryption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopDeliveryStreamEncryptionResponse' smart constructor.
data StopDeliveryStreamEncryptionResponse = StopDeliveryStreamEncryptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDeliveryStreamEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopDeliveryStreamEncryptionResponse_httpStatus' - The response's http status code.
newStopDeliveryStreamEncryptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDeliveryStreamEncryptionResponse
newStopDeliveryStreamEncryptionResponse pHttpStatus_ =
  StopDeliveryStreamEncryptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopDeliveryStreamEncryptionResponse_httpStatus :: Lens.Lens' StopDeliveryStreamEncryptionResponse Prelude.Int
stopDeliveryStreamEncryptionResponse_httpStatus = Lens.lens (\StopDeliveryStreamEncryptionResponse' {httpStatus} -> httpStatus) (\s@StopDeliveryStreamEncryptionResponse' {} a -> s {httpStatus = a} :: StopDeliveryStreamEncryptionResponse)

instance
  Prelude.NFData
    StopDeliveryStreamEncryptionResponse
  where
  rnf StopDeliveryStreamEncryptionResponse' {..} =
    Prelude.rnf httpStatus
