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
-- Module      : Amazonka.Connect.TransferContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers contacts from one agent or queue to another agent or queue at
-- any point after a contact is created. You can transfer a contact to
-- another queue by providing the flow which orchestrates the contact to
-- the destination queue. This gives you more control over contact handling
-- and helps you adhere to the service level agreement (SLA) guaranteed to
-- your customers.
--
-- Note the following requirements:
--
-- -   Transfer is supported for only @TASK@ contacts.
--
-- -   Do not use both @QueueId@ and @UserId@ in the same call.
--
-- -   The following flow types are supported: Inbound flow, Transfer to
--     agent flow, and Transfer to queue flow.
--
-- -   The @TransferContact@ API can be called only on active contacts.
--
-- -   A contact cannot be transferred more than 11 times.
module Amazonka.Connect.TransferContact
  ( -- * Creating a Request
    TransferContact (..),
    newTransferContact,

    -- * Request Lenses
    transferContact_clientToken,
    transferContact_queueId,
    transferContact_userId,
    transferContact_instanceId,
    transferContact_contactId,
    transferContact_contactFlowId,

    -- * Destructuring the Response
    TransferContactResponse (..),
    newTransferContactResponse,

    -- * Response Lenses
    transferContactResponse_contactArn,
    transferContactResponse_contactId,
    transferContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTransferContact' smart constructor.
data TransferContact = TransferContact'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the queue.
    queueId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'transferContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'queueId', 'transferContact_queueId' - The identifier for the queue.
--
-- 'userId', 'transferContact_userId' - The identifier for the user.
--
-- 'instanceId', 'transferContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'transferContact_contactId' - The identifier of the contact in this instance of Amazon Connect.
--
-- 'contactFlowId', 'transferContact_contactFlowId' - The identifier of the flow.
newTransferContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  TransferContact
newTransferContact
  pInstanceId_
  pContactId_
  pContactFlowId_ =
    TransferContact'
      { clientToken = Prelude.Nothing,
        queueId = Prelude.Nothing,
        userId = Prelude.Nothing,
        instanceId = pInstanceId_,
        contactId = pContactId_,
        contactFlowId = pContactFlowId_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
transferContact_clientToken :: Lens.Lens' TransferContact (Prelude.Maybe Prelude.Text)
transferContact_clientToken = Lens.lens (\TransferContact' {clientToken} -> clientToken) (\s@TransferContact' {} a -> s {clientToken = a} :: TransferContact)

-- | The identifier for the queue.
transferContact_queueId :: Lens.Lens' TransferContact (Prelude.Maybe Prelude.Text)
transferContact_queueId = Lens.lens (\TransferContact' {queueId} -> queueId) (\s@TransferContact' {} a -> s {queueId = a} :: TransferContact)

-- | The identifier for the user.
transferContact_userId :: Lens.Lens' TransferContact (Prelude.Maybe Prelude.Text)
transferContact_userId = Lens.lens (\TransferContact' {userId} -> userId) (\s@TransferContact' {} a -> s {userId = a} :: TransferContact)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
transferContact_instanceId :: Lens.Lens' TransferContact Prelude.Text
transferContact_instanceId = Lens.lens (\TransferContact' {instanceId} -> instanceId) (\s@TransferContact' {} a -> s {instanceId = a} :: TransferContact)

-- | The identifier of the contact in this instance of Amazon Connect.
transferContact_contactId :: Lens.Lens' TransferContact Prelude.Text
transferContact_contactId = Lens.lens (\TransferContact' {contactId} -> contactId) (\s@TransferContact' {} a -> s {contactId = a} :: TransferContact)

-- | The identifier of the flow.
transferContact_contactFlowId :: Lens.Lens' TransferContact Prelude.Text
transferContact_contactFlowId = Lens.lens (\TransferContact' {contactFlowId} -> contactFlowId) (\s@TransferContact' {} a -> s {contactFlowId = a} :: TransferContact)

instance Core.AWSRequest TransferContact where
  type
    AWSResponse TransferContact =
      TransferContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TransferContactResponse'
            Prelude.<$> (x Data..?> "ContactArn")
            Prelude.<*> (x Data..?> "ContactId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TransferContact where
  hashWithSalt _salt TransferContact' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` queueId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` contactFlowId

instance Prelude.NFData TransferContact where
  rnf TransferContact' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf queueId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf contactFlowId

instance Data.ToHeaders TransferContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TransferContact where
  toJSON TransferContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("QueueId" Data..=) Prelude.<$> queueId,
            ("UserId" Data..=) Prelude.<$> userId,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just
              ("ContactFlowId" Data..= contactFlowId)
          ]
      )

instance Data.ToPath TransferContact where
  toPath = Prelude.const "/contact/transfer"

instance Data.ToQuery TransferContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTransferContactResponse' smart constructor.
data TransferContactResponse = TransferContactResponse'
  { -- | The Amazon Resource Name (ARN) of the contact.
    contactArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'transferContactResponse_contactArn' - The Amazon Resource Name (ARN) of the contact.
--
-- 'contactId', 'transferContactResponse_contactId' - The identifier of the contact in this instance of Amazon Connect.
--
-- 'httpStatus', 'transferContactResponse_httpStatus' - The response's http status code.
newTransferContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TransferContactResponse
newTransferContactResponse pHttpStatus_ =
  TransferContactResponse'
    { contactArn =
        Prelude.Nothing,
      contactId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the contact.
transferContactResponse_contactArn :: Lens.Lens' TransferContactResponse (Prelude.Maybe Prelude.Text)
transferContactResponse_contactArn = Lens.lens (\TransferContactResponse' {contactArn} -> contactArn) (\s@TransferContactResponse' {} a -> s {contactArn = a} :: TransferContactResponse)

-- | The identifier of the contact in this instance of Amazon Connect.
transferContactResponse_contactId :: Lens.Lens' TransferContactResponse (Prelude.Maybe Prelude.Text)
transferContactResponse_contactId = Lens.lens (\TransferContactResponse' {contactId} -> contactId) (\s@TransferContactResponse' {} a -> s {contactId = a} :: TransferContactResponse)

-- | The response's http status code.
transferContactResponse_httpStatus :: Lens.Lens' TransferContactResponse Prelude.Int
transferContactResponse_httpStatus = Lens.lens (\TransferContactResponse' {httpStatus} -> httpStatus) (\s@TransferContactResponse' {} a -> s {httpStatus = a} :: TransferContactResponse)

instance Prelude.NFData TransferContactResponse where
  rnf TransferContactResponse' {..} =
    Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf httpStatus
