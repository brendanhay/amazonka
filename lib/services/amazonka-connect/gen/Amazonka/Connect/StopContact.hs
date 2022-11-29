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
-- Module      : Amazonka.Connect.StopContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends the specified contact. This call does not work for the following
-- initiation methods:
--
-- -   DISCONNECT
--
-- -   TRANSFER
--
-- -   QUEUE_TRANSFER
module Amazonka.Connect.StopContact
  ( -- * Creating a Request
    StopContact (..),
    newStopContact,

    -- * Request Lenses
    stopContact_contactId,
    stopContact_instanceId,

    -- * Destructuring the Response
    StopContactResponse (..),
    newStopContactResponse,

    -- * Response Lenses
    stopContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopContact' smart constructor.
data StopContact = StopContact'
  { -- | The ID of the contact.
    contactId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'stopContact_contactId' - The ID of the contact.
--
-- 'instanceId', 'stopContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newStopContact ::
  -- | 'contactId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  StopContact
newStopContact pContactId_ pInstanceId_ =
  StopContact'
    { contactId = pContactId_,
      instanceId = pInstanceId_
    }

-- | The ID of the contact.
stopContact_contactId :: Lens.Lens' StopContact Prelude.Text
stopContact_contactId = Lens.lens (\StopContact' {contactId} -> contactId) (\s@StopContact' {} a -> s {contactId = a} :: StopContact)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
stopContact_instanceId :: Lens.Lens' StopContact Prelude.Text
stopContact_instanceId = Lens.lens (\StopContact' {instanceId} -> instanceId) (\s@StopContact' {} a -> s {instanceId = a} :: StopContact)

instance Core.AWSRequest StopContact where
  type AWSResponse StopContact = StopContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopContact where
  hashWithSalt _salt StopContact' {..} =
    _salt `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData StopContact where
  rnf StopContact' {..} =
    Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders StopContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopContact where
  toJSON StopContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContactId" Core..= contactId),
            Prelude.Just ("InstanceId" Core..= instanceId)
          ]
      )

instance Core.ToPath StopContact where
  toPath = Prelude.const "/contact/stop"

instance Core.ToQuery StopContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopContactResponse' smart constructor.
data StopContactResponse = StopContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopContactResponse_httpStatus' - The response's http status code.
newStopContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopContactResponse
newStopContactResponse pHttpStatus_ =
  StopContactResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopContactResponse_httpStatus :: Lens.Lens' StopContactResponse Prelude.Int
stopContactResponse_httpStatus = Lens.lens (\StopContactResponse' {httpStatus} -> httpStatus) (\s@StopContactResponse' {} a -> s {httpStatus = a} :: StopContactResponse)

instance Prelude.NFData StopContactResponse where
  rnf StopContactResponse' {..} = Prelude.rnf httpStatus
