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
-- Module      : Network.AWS.Firehose.StartDeliveryStreamEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables server-side encryption (SSE) for the delivery stream.
--
-- This operation is asynchronous. It returns immediately. When you invoke
-- it, Kinesis Data Firehose first sets the encryption status of the stream
-- to @ENABLING@, and then to @ENABLED@. The encryption status of a
-- delivery stream is the @Status@ property in
-- DeliveryStreamEncryptionConfiguration. If the operation fails, the
-- encryption status changes to @ENABLING_FAILED@. You can continue to read
-- and write data to your delivery stream while the encryption status is
-- @ENABLING@, but the data is not encrypted. It can take up to 5 seconds
-- after the encryption status changes to @ENABLED@ before all records
-- written to the delivery stream are encrypted. To find out whether a
-- record or a batch of records was encrypted, check the response elements
-- PutRecordOutput$Encrypted and PutRecordBatchOutput$Encrypted,
-- respectively.
--
-- To check the encryption status of a delivery stream, use
-- DescribeDeliveryStream.
--
-- Even if encryption is currently enabled for a delivery stream, you can
-- still invoke this operation on it to change the ARN of the CMK or both
-- its type and ARN. If you invoke this method to change the CMK, and the
-- old CMK is of type @CUSTOMER_MANAGED_CMK@, Kinesis Data Firehose
-- schedules the grant it had on the old CMK for retirement. If the new CMK
-- is of type @CUSTOMER_MANAGED_CMK@, Kinesis Data Firehose creates a grant
-- that enables it to use the new CMK to encrypt and decrypt data and to
-- manage the grant.
--
-- If a delivery stream already has encryption enabled and then you invoke
-- this operation to change the ARN of the CMK or both its type and ARN and
-- you get @ENABLING_FAILED@, this only means that the attempt to change
-- the CMK failed. In this case, encryption remains enabled with the old
-- CMK.
--
-- If the encryption status of your delivery stream is @ENABLING_FAILED@,
-- you can invoke this operation again with a valid CMK. The CMK must be
-- enabled and the key policy mustn\'t explicitly deny the permission for
-- Kinesis Data Firehose to invoke KMS encrypt and decrypt operations.
--
-- You can enable SSE for a delivery stream only if it\'s a delivery stream
-- that uses @DirectPut@ as its source.
--
-- The @StartDeliveryStreamEncryption@ and @StopDeliveryStreamEncryption@
-- operations have a combined limit of 25 calls per delivery stream per 24
-- hours. For example, you reach the limit if you call
-- @StartDeliveryStreamEncryption@ 13 times and
-- @StopDeliveryStreamEncryption@ 12 times for the same delivery stream in
-- a 24-hour period.
module Network.AWS.Firehose.StartDeliveryStreamEncryption
  ( -- * Creating a Request
    StartDeliveryStreamEncryption (..),
    newStartDeliveryStreamEncryption,

    -- * Request Lenses
    startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput,
    startDeliveryStreamEncryption_deliveryStreamName,

    -- * Destructuring the Response
    StartDeliveryStreamEncryptionResponse (..),
    newStartDeliveryStreamEncryptionResponse,

    -- * Response Lenses
    startDeliveryStreamEncryptionResponse_httpStatus,
  )
where

import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartDeliveryStreamEncryption' smart constructor.
data StartDeliveryStreamEncryption = StartDeliveryStreamEncryption'
  { -- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key
    -- needed for Server-Side Encryption (SSE).
    deliveryStreamEncryptionConfigurationInput :: Prelude.Maybe DeliveryStreamEncryptionConfigurationInput,
    -- | The name of the delivery stream for which you want to enable server-side
    -- encryption (SSE).
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartDeliveryStreamEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStreamEncryptionConfigurationInput', 'startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput' - Used to specify the type and Amazon Resource Name (ARN) of the KMS key
-- needed for Server-Side Encryption (SSE).
--
-- 'deliveryStreamName', 'startDeliveryStreamEncryption_deliveryStreamName' - The name of the delivery stream for which you want to enable server-side
-- encryption (SSE).
newStartDeliveryStreamEncryption ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  StartDeliveryStreamEncryption
newStartDeliveryStreamEncryption pDeliveryStreamName_ =
  StartDeliveryStreamEncryption'
    { deliveryStreamEncryptionConfigurationInput =
        Prelude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key
-- needed for Server-Side Encryption (SSE).
startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput :: Lens.Lens' StartDeliveryStreamEncryption (Prelude.Maybe DeliveryStreamEncryptionConfigurationInput)
startDeliveryStreamEncryption_deliveryStreamEncryptionConfigurationInput = Lens.lens (\StartDeliveryStreamEncryption' {deliveryStreamEncryptionConfigurationInput} -> deliveryStreamEncryptionConfigurationInput) (\s@StartDeliveryStreamEncryption' {} a -> s {deliveryStreamEncryptionConfigurationInput = a} :: StartDeliveryStreamEncryption)

-- | The name of the delivery stream for which you want to enable server-side
-- encryption (SSE).
startDeliveryStreamEncryption_deliveryStreamName :: Lens.Lens' StartDeliveryStreamEncryption Prelude.Text
startDeliveryStreamEncryption_deliveryStreamName = Lens.lens (\StartDeliveryStreamEncryption' {deliveryStreamName} -> deliveryStreamName) (\s@StartDeliveryStreamEncryption' {} a -> s {deliveryStreamName = a} :: StartDeliveryStreamEncryption)

instance
  Prelude.AWSRequest
    StartDeliveryStreamEncryption
  where
  type
    Rs StartDeliveryStreamEncryption =
      StartDeliveryStreamEncryptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartDeliveryStreamEncryptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartDeliveryStreamEncryption

instance Prelude.NFData StartDeliveryStreamEncryption

instance
  Prelude.ToHeaders
    StartDeliveryStreamEncryption
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Firehose_20150804.StartDeliveryStreamEncryption" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartDeliveryStreamEncryption where
  toJSON StartDeliveryStreamEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ( "DeliveryStreamEncryptionConfigurationInput"
                Prelude..=
            )
              Prelude.<$> deliveryStreamEncryptionConfigurationInput,
            Prelude.Just
              ( "DeliveryStreamName"
                  Prelude..= deliveryStreamName
              )
          ]
      )

instance Prelude.ToPath StartDeliveryStreamEncryption where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    StartDeliveryStreamEncryption
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDeliveryStreamEncryptionResponse' smart constructor.
data StartDeliveryStreamEncryptionResponse = StartDeliveryStreamEncryptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartDeliveryStreamEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startDeliveryStreamEncryptionResponse_httpStatus' - The response's http status code.
newStartDeliveryStreamEncryptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDeliveryStreamEncryptionResponse
newStartDeliveryStreamEncryptionResponse pHttpStatus_ =
  StartDeliveryStreamEncryptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startDeliveryStreamEncryptionResponse_httpStatus :: Lens.Lens' StartDeliveryStreamEncryptionResponse Prelude.Int
startDeliveryStreamEncryptionResponse_httpStatus = Lens.lens (\StartDeliveryStreamEncryptionResponse' {httpStatus} -> httpStatus) (\s@StartDeliveryStreamEncryptionResponse' {} a -> s {httpStatus = a} :: StartDeliveryStreamEncryptionResponse)

instance
  Prelude.NFData
    StartDeliveryStreamEncryptionResponse
