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
-- Module      : Amazonka.Connect.UpdateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Adds or updates user-defined contact information associated with the
-- specified contact. At least one field to be updated must be present in
-- the request.
--
-- You can add or update user-defined contact information for both ongoing
-- and completed contacts.
module Amazonka.Connect.UpdateContact
  ( -- * Creating a Request
    UpdateContact (..),
    newUpdateContact,

    -- * Request Lenses
    updateContact_description,
    updateContact_name,
    updateContact_references,
    updateContact_instanceId,
    updateContact_contactId,

    -- * Destructuring the Response
    UpdateContactResponse (..),
    newUpdateContactResponse,

    -- * Response Lenses
    updateContactResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContact' smart constructor.
data UpdateContact = UpdateContact'
  { -- | The description of the contact.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact.
    name :: Prelude.Maybe Prelude.Text,
    -- | Well-formed data on contact, shown to agents on Contact Control Panel
    -- (CCP).
    references :: Prelude.Maybe (Prelude.HashMap Prelude.Text Reference),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact. This is the identifier of the contact
    -- associated with the first interaction with your contact center.
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
-- 'description', 'updateContact_description' - The description of the contact.
--
-- 'name', 'updateContact_name' - The name of the contact.
--
-- 'references', 'updateContact_references' - Well-formed data on contact, shown to agents on Contact Control Panel
-- (CCP).
--
-- 'instanceId', 'updateContact_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'updateContact_contactId' - The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with your contact center.
newUpdateContact ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  UpdateContact
newUpdateContact pInstanceId_ pContactId_ =
  UpdateContact'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      references = Prelude.Nothing,
      instanceId = pInstanceId_,
      contactId = pContactId_
    }

-- | The description of the contact.
updateContact_description :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_description = Lens.lens (\UpdateContact' {description} -> description) (\s@UpdateContact' {} a -> s {description = a} :: UpdateContact)

-- | The name of the contact.
updateContact_name :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_name = Lens.lens (\UpdateContact' {name} -> name) (\s@UpdateContact' {} a -> s {name = a} :: UpdateContact)

-- | Well-formed data on contact, shown to agents on Contact Control Panel
-- (CCP).
updateContact_references :: Lens.Lens' UpdateContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Reference))
updateContact_references = Lens.lens (\UpdateContact' {references} -> references) (\s@UpdateContact' {} a -> s {references = a} :: UpdateContact) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateContact_instanceId :: Lens.Lens' UpdateContact Prelude.Text
updateContact_instanceId = Lens.lens (\UpdateContact' {instanceId} -> instanceId) (\s@UpdateContact' {} a -> s {instanceId = a} :: UpdateContact)

-- | The identifier of the contact. This is the identifier of the contact
-- associated with the first interaction with your contact center.
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
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` references
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData UpdateContact where
  rnf UpdateContact' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf references `Prelude.seq`
          Prelude.rnf instanceId `Prelude.seq`
            Prelude.rnf contactId

instance Data.ToHeaders UpdateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContact where
  toJSON UpdateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("References" Data..=) Prelude.<$> references
          ]
      )

instance Data.ToPath UpdateContact where
  toPath UpdateContact' {..} =
    Prelude.mconcat
      [ "/contacts/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactId
      ]

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
