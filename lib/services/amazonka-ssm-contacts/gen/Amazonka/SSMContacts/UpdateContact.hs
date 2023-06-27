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
-- Module      : Amazonka.SSMContacts.UpdateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contact or escalation plan specified.
module Amazonka.SSMContacts.UpdateContact
  ( -- * Creating a Request
    UpdateContact (..),
    newUpdateContact,

    -- * Request Lenses
    updateContact_displayName,
    updateContact_plan,
    updateContact_contactId,

    -- * Destructuring the Response
    UpdateContactResponse (..),
    newUpdateContactResponse,

    -- * Response Lenses
    updateContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newUpdateContact' smart constructor.
data UpdateContact = UpdateContact'
  { -- | The full name of the contact or escalation plan.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | A list of stages. A contact has an engagement plan with stages for
    -- specified contact channels. An escalation plan uses these stages to
    -- contact specified contacts.
    plan :: Prelude.Maybe Plan,
    -- | The Amazon Resource Name (ARN) of the contact or escalation plan you\'re
    -- updating.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updateContact_displayName' - The full name of the contact or escalation plan.
--
-- 'plan', 'updateContact_plan' - A list of stages. A contact has an engagement plan with stages for
-- specified contact channels. An escalation plan uses these stages to
-- contact specified contacts.
--
-- 'contactId', 'updateContact_contactId' - The Amazon Resource Name (ARN) of the contact or escalation plan you\'re
-- updating.
newUpdateContact ::
  -- | 'contactId'
  Prelude.Text ->
  UpdateContact
newUpdateContact pContactId_ =
  UpdateContact'
    { displayName = Prelude.Nothing,
      plan = Prelude.Nothing,
      contactId = pContactId_
    }

-- | The full name of the contact or escalation plan.
updateContact_displayName :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_displayName = Lens.lens (\UpdateContact' {displayName} -> displayName) (\s@UpdateContact' {} a -> s {displayName = a} :: UpdateContact)

-- | A list of stages. A contact has an engagement plan with stages for
-- specified contact channels. An escalation plan uses these stages to
-- contact specified contacts.
updateContact_plan :: Lens.Lens' UpdateContact (Prelude.Maybe Plan)
updateContact_plan = Lens.lens (\UpdateContact' {plan} -> plan) (\s@UpdateContact' {} a -> s {plan = a} :: UpdateContact)

-- | The Amazon Resource Name (ARN) of the contact or escalation plan you\'re
-- updating.
updateContact_contactId :: Lens.Lens' UpdateContact Prelude.Text
updateContact_contactId = Lens.lens (\UpdateContact' {contactId} -> contactId) (\s@UpdateContact' {} a -> s {contactId = a} :: UpdateContact)

instance Core.AWSRequest UpdateContact where
  type
    AWSResponse UpdateContact =
      UpdateContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContact where
  hashWithSalt _salt UpdateContact' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` plan
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData UpdateContact where
  rnf UpdateContact' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf plan
      `Prelude.seq` Prelude.rnf contactId

instance Data.ToHeaders UpdateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.UpdateContact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContact where
  toJSON UpdateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("Plan" Data..=) Prelude.<$> plan,
            Prelude.Just ("ContactId" Data..= contactId)
          ]
      )

instance Data.ToPath UpdateContact where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactResponse' smart constructor.
data UpdateContactResponse = UpdateContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactResponse_httpStatus' - The response's http status code.
newUpdateContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContactResponse
newUpdateContactResponse pHttpStatus_ =
  UpdateContactResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateContactResponse_httpStatus :: Lens.Lens' UpdateContactResponse Prelude.Int
updateContactResponse_httpStatus = Lens.lens (\UpdateContactResponse' {httpStatus} -> httpStatus) (\s@UpdateContactResponse' {} a -> s {httpStatus = a} :: UpdateContactResponse)

instance Prelude.NFData UpdateContactResponse where
  rnf UpdateContactResponse' {..} =
    Prelude.rnf httpStatus
