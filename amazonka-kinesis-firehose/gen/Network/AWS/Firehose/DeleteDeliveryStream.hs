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
-- Module      : Network.AWS.Firehose.DeleteDeliveryStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a delivery stream and its data.
--
-- To check the state of a delivery stream, use DescribeDeliveryStream. You
-- can delete a delivery stream only if it is in one of the following
-- states: @ACTIVE@, @DELETING@, @CREATING_FAILED@, or @DELETING_FAILED@.
-- You can\'t delete a delivery stream that is in the @CREATING@ state.
-- While the deletion request is in process, the delivery stream is in the
-- @DELETING@ state.
--
-- While the delivery stream is in the @DELETING@ state, the service might
-- continue to accept records, but it doesn\'t make any guarantees with
-- respect to delivering the data. Therefore, as a best practice, first
-- stop any applications that are sending records before you delete a
-- delivery stream.
module Network.AWS.Firehose.DeleteDeliveryStream
  ( -- * Creating a Request
    DeleteDeliveryStream (..),
    newDeleteDeliveryStream,

    -- * Request Lenses
    deleteDeliveryStream_allowForceDelete,
    deleteDeliveryStream_deliveryStreamName,

    -- * Destructuring the Response
    DeleteDeliveryStreamResponse (..),
    newDeleteDeliveryStreamResponse,

    -- * Response Lenses
    deleteDeliveryStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Firehose.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDeliveryStream' smart constructor.
data DeleteDeliveryStream = DeleteDeliveryStream'
  { -- | Set this to true if you want to delete the delivery stream even if
    -- Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis
    -- Data Firehose might be unable to retire the grant due to a customer
    -- error, such as when the CMK or the grant are in an invalid state. If you
    -- force deletion, you can then use the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant>
    -- operation to revoke the grant you gave to Kinesis Data Firehose. If a
    -- failure to retire the grant happens due to an AWS KMS issue, Kinesis
    -- Data Firehose keeps retrying the delete operation.
    --
    -- The default value is false.
    allowForceDelete :: Core.Maybe Core.Bool,
    -- | The name of the delivery stream.
    deliveryStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeliveryStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowForceDelete', 'deleteDeliveryStream_allowForceDelete' - Set this to true if you want to delete the delivery stream even if
-- Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis
-- Data Firehose might be unable to retire the grant due to a customer
-- error, such as when the CMK or the grant are in an invalid state. If you
-- force deletion, you can then use the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant>
-- operation to revoke the grant you gave to Kinesis Data Firehose. If a
-- failure to retire the grant happens due to an AWS KMS issue, Kinesis
-- Data Firehose keeps retrying the delete operation.
--
-- The default value is false.
--
-- 'deliveryStreamName', 'deleteDeliveryStream_deliveryStreamName' - The name of the delivery stream.
newDeleteDeliveryStream ::
  -- | 'deliveryStreamName'
  Core.Text ->
  DeleteDeliveryStream
newDeleteDeliveryStream pDeliveryStreamName_ =
  DeleteDeliveryStream'
    { allowForceDelete =
        Core.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | Set this to true if you want to delete the delivery stream even if
-- Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis
-- Data Firehose might be unable to retire the grant due to a customer
-- error, such as when the CMK or the grant are in an invalid state. If you
-- force deletion, you can then use the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant>
-- operation to revoke the grant you gave to Kinesis Data Firehose. If a
-- failure to retire the grant happens due to an AWS KMS issue, Kinesis
-- Data Firehose keeps retrying the delete operation.
--
-- The default value is false.
deleteDeliveryStream_allowForceDelete :: Lens.Lens' DeleteDeliveryStream (Core.Maybe Core.Bool)
deleteDeliveryStream_allowForceDelete = Lens.lens (\DeleteDeliveryStream' {allowForceDelete} -> allowForceDelete) (\s@DeleteDeliveryStream' {} a -> s {allowForceDelete = a} :: DeleteDeliveryStream)

-- | The name of the delivery stream.
deleteDeliveryStream_deliveryStreamName :: Lens.Lens' DeleteDeliveryStream Core.Text
deleteDeliveryStream_deliveryStreamName = Lens.lens (\DeleteDeliveryStream' {deliveryStreamName} -> deliveryStreamName) (\s@DeleteDeliveryStream' {} a -> s {deliveryStreamName = a} :: DeleteDeliveryStream)

instance Core.AWSRequest DeleteDeliveryStream where
  type
    AWSResponse DeleteDeliveryStream =
      DeleteDeliveryStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDeliveryStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDeliveryStream

instance Core.NFData DeleteDeliveryStream

instance Core.ToHeaders DeleteDeliveryStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Firehose_20150804.DeleteDeliveryStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDeliveryStream where
  toJSON DeleteDeliveryStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowForceDelete" Core..=)
              Core.<$> allowForceDelete,
            Core.Just
              ("DeliveryStreamName" Core..= deliveryStreamName)
          ]
      )

instance Core.ToPath DeleteDeliveryStream where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDeliveryStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDeliveryStreamResponse' smart constructor.
data DeleteDeliveryStreamResponse = DeleteDeliveryStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeliveryStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDeliveryStreamResponse_httpStatus' - The response's http status code.
newDeleteDeliveryStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDeliveryStreamResponse
newDeleteDeliveryStreamResponse pHttpStatus_ =
  DeleteDeliveryStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDeliveryStreamResponse_httpStatus :: Lens.Lens' DeleteDeliveryStreamResponse Core.Int
deleteDeliveryStreamResponse_httpStatus = Lens.lens (\DeleteDeliveryStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteDeliveryStreamResponse' {} a -> s {httpStatus = a} :: DeleteDeliveryStreamResponse)

instance Core.NFData DeleteDeliveryStreamResponse
