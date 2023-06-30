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
-- Module      : Amazonka.Connect.DismissUserContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dismisses contacts from an agent’s CCP and returns the agent to an
-- available state, which allows the agent to receive a new routed contact.
-- Contacts can only be dismissed if they are in a @MISSED@, @ERROR@,
-- @ENDED@, or @REJECTED@ state in the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html Agent Event Stream>.
module Amazonka.Connect.DismissUserContact
  ( -- * Creating a Request
    DismissUserContact (..),
    newDismissUserContact,

    -- * Request Lenses
    dismissUserContact_userId,
    dismissUserContact_instanceId,
    dismissUserContact_contactId,

    -- * Destructuring the Response
    DismissUserContactResponse (..),
    newDismissUserContactResponse,

    -- * Response Lenses
    dismissUserContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDismissUserContact' smart constructor.
data DismissUserContact = DismissUserContact'
  { -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DismissUserContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'dismissUserContact_userId' - The identifier of the user account.
--
-- 'instanceId', 'dismissUserContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'dismissUserContact_contactId' - The identifier of the contact.
newDismissUserContact ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  DismissUserContact
newDismissUserContact
  pUserId_
  pInstanceId_
  pContactId_ =
    DismissUserContact'
      { userId = pUserId_,
        instanceId = pInstanceId_,
        contactId = pContactId_
      }

-- | The identifier of the user account.
dismissUserContact_userId :: Lens.Lens' DismissUserContact Prelude.Text
dismissUserContact_userId = Lens.lens (\DismissUserContact' {userId} -> userId) (\s@DismissUserContact' {} a -> s {userId = a} :: DismissUserContact)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
dismissUserContact_instanceId :: Lens.Lens' DismissUserContact Prelude.Text
dismissUserContact_instanceId = Lens.lens (\DismissUserContact' {instanceId} -> instanceId) (\s@DismissUserContact' {} a -> s {instanceId = a} :: DismissUserContact)

-- | The identifier of the contact.
dismissUserContact_contactId :: Lens.Lens' DismissUserContact Prelude.Text
dismissUserContact_contactId = Lens.lens (\DismissUserContact' {contactId} -> contactId) (\s@DismissUserContact' {} a -> s {contactId = a} :: DismissUserContact)

instance Core.AWSRequest DismissUserContact where
  type
    AWSResponse DismissUserContact =
      DismissUserContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DismissUserContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DismissUserContact where
  hashWithSalt _salt DismissUserContact' {..} =
    _salt
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData DismissUserContact where
  rnf DismissUserContact' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId

instance Data.ToHeaders DismissUserContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DismissUserContact where
  toJSON DismissUserContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContactId" Data..= contactId)]
      )

instance Data.ToPath DismissUserContact where
  toPath DismissUserContact' {..} =
    Prelude.mconcat
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId,
        "/contact"
      ]

instance Data.ToQuery DismissUserContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDismissUserContactResponse' smart constructor.
data DismissUserContactResponse = DismissUserContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DismissUserContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'dismissUserContactResponse_httpStatus' - The response's http status code.
newDismissUserContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DismissUserContactResponse
newDismissUserContactResponse pHttpStatus_ =
  DismissUserContactResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
dismissUserContactResponse_httpStatus :: Lens.Lens' DismissUserContactResponse Prelude.Int
dismissUserContactResponse_httpStatus = Lens.lens (\DismissUserContactResponse' {httpStatus} -> httpStatus) (\s@DismissUserContactResponse' {} a -> s {httpStatus = a} :: DismissUserContactResponse)

instance Prelude.NFData DismissUserContactResponse where
  rnf DismissUserContactResponse' {..} =
    Prelude.rnf httpStatus
