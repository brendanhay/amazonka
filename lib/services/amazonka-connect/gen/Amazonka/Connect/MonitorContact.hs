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
-- Module      : Amazonka.Connect.MonitorContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates silent monitoring of a contact. The Contact Control Panel
-- (CCP) of the user specified by /userId/ will be set to silent monitoring
-- mode on the contact.
module Amazonka.Connect.MonitorContact
  ( -- * Creating a Request
    MonitorContact (..),
    newMonitorContact,

    -- * Request Lenses
    monitorContact_allowedMonitorCapabilities,
    monitorContact_clientToken,
    monitorContact_instanceId,
    monitorContact_contactId,
    monitorContact_userId,

    -- * Destructuring the Response
    MonitorContactResponse (..),
    newMonitorContactResponse,

    -- * Response Lenses
    monitorContactResponse_contactArn,
    monitorContactResponse_contactId,
    monitorContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMonitorContact' smart constructor.
data MonitorContact = MonitorContact'
  { -- | Specify which monitoring actions the user is allowed to take. For
    -- example, whether the user is allowed to escalate from silent monitoring
    -- to barge.
    allowedMonitorCapabilities :: Prelude.Maybe [MonitorCapability],
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text,
    -- | The identifier of the user account.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedMonitorCapabilities', 'monitorContact_allowedMonitorCapabilities' - Specify which monitoring actions the user is allowed to take. For
-- example, whether the user is allowed to escalate from silent monitoring
-- to barge.
--
-- 'clientToken', 'monitorContact_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'instanceId', 'monitorContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'monitorContact_contactId' - The identifier of the contact.
--
-- 'userId', 'monitorContact_userId' - The identifier of the user account.
newMonitorContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  MonitorContact
newMonitorContact pInstanceId_ pContactId_ pUserId_ =
  MonitorContact'
    { allowedMonitorCapabilities =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      instanceId = pInstanceId_,
      contactId = pContactId_,
      userId = pUserId_
    }

-- | Specify which monitoring actions the user is allowed to take. For
-- example, whether the user is allowed to escalate from silent monitoring
-- to barge.
monitorContact_allowedMonitorCapabilities :: Lens.Lens' MonitorContact (Prelude.Maybe [MonitorCapability])
monitorContact_allowedMonitorCapabilities = Lens.lens (\MonitorContact' {allowedMonitorCapabilities} -> allowedMonitorCapabilities) (\s@MonitorContact' {} a -> s {allowedMonitorCapabilities = a} :: MonitorContact) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
monitorContact_clientToken :: Lens.Lens' MonitorContact (Prelude.Maybe Prelude.Text)
monitorContact_clientToken = Lens.lens (\MonitorContact' {clientToken} -> clientToken) (\s@MonitorContact' {} a -> s {clientToken = a} :: MonitorContact)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
monitorContact_instanceId :: Lens.Lens' MonitorContact Prelude.Text
monitorContact_instanceId = Lens.lens (\MonitorContact' {instanceId} -> instanceId) (\s@MonitorContact' {} a -> s {instanceId = a} :: MonitorContact)

-- | The identifier of the contact.
monitorContact_contactId :: Lens.Lens' MonitorContact Prelude.Text
monitorContact_contactId = Lens.lens (\MonitorContact' {contactId} -> contactId) (\s@MonitorContact' {} a -> s {contactId = a} :: MonitorContact)

-- | The identifier of the user account.
monitorContact_userId :: Lens.Lens' MonitorContact Prelude.Text
monitorContact_userId = Lens.lens (\MonitorContact' {userId} -> userId) (\s@MonitorContact' {} a -> s {userId = a} :: MonitorContact)

instance Core.AWSRequest MonitorContact where
  type
    AWSResponse MonitorContact =
      MonitorContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MonitorContactResponse'
            Prelude.<$> (x Data..?> "ContactArn")
            Prelude.<*> (x Data..?> "ContactId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MonitorContact where
  hashWithSalt _salt MonitorContact' {..} =
    _salt
      `Prelude.hashWithSalt` allowedMonitorCapabilities
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData MonitorContact where
  rnf MonitorContact' {..} =
    Prelude.rnf allowedMonitorCapabilities
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders MonitorContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MonitorContact where
  toJSON MonitorContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowedMonitorCapabilities" Data..=)
              Prelude.<$> allowedMonitorCapabilities,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just ("UserId" Data..= userId)
          ]
      )

instance Data.ToPath MonitorContact where
  toPath = Prelude.const "/contact/monitor"

instance Data.ToQuery MonitorContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMonitorContactResponse' smart constructor.
data MonitorContactResponse = MonitorContactResponse'
  { -- | The ARN of the contact.
    contactArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'monitorContactResponse_contactArn' - The ARN of the contact.
--
-- 'contactId', 'monitorContactResponse_contactId' - The identifier of the contact.
--
-- 'httpStatus', 'monitorContactResponse_httpStatus' - The response's http status code.
newMonitorContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MonitorContactResponse
newMonitorContactResponse pHttpStatus_ =
  MonitorContactResponse'
    { contactArn =
        Prelude.Nothing,
      contactId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the contact.
monitorContactResponse_contactArn :: Lens.Lens' MonitorContactResponse (Prelude.Maybe Prelude.Text)
monitorContactResponse_contactArn = Lens.lens (\MonitorContactResponse' {contactArn} -> contactArn) (\s@MonitorContactResponse' {} a -> s {contactArn = a} :: MonitorContactResponse)

-- | The identifier of the contact.
monitorContactResponse_contactId :: Lens.Lens' MonitorContactResponse (Prelude.Maybe Prelude.Text)
monitorContactResponse_contactId = Lens.lens (\MonitorContactResponse' {contactId} -> contactId) (\s@MonitorContactResponse' {} a -> s {contactId = a} :: MonitorContactResponse)

-- | The response's http status code.
monitorContactResponse_httpStatus :: Lens.Lens' MonitorContactResponse Prelude.Int
monitorContactResponse_httpStatus = Lens.lens (\MonitorContactResponse' {httpStatus} -> httpStatus) (\s@MonitorContactResponse' {} a -> s {httpStatus = a} :: MonitorContactResponse)

instance Prelude.NFData MonitorContactResponse where
  rnf MonitorContactResponse' {..} =
    Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf httpStatus
