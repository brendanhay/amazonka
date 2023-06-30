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
-- Module      : Amazonka.SSMContacts.CreateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contacts are either the contacts that Incident Manager engages during an
-- incident or the escalation plans that Incident Manager uses to engage
-- contacts in phases during an incident.
module Amazonka.SSMContacts.CreateContact
  ( -- * Creating a Request
    CreateContact (..),
    newCreateContact,

    -- * Request Lenses
    createContact_displayName,
    createContact_idempotencyToken,
    createContact_tags,
    createContact_alias,
    createContact_type,
    createContact_plan,

    -- * Destructuring the Response
    CreateContactResponse (..),
    newCreateContactResponse,

    -- * Response Lenses
    createContactResponse_httpStatus,
    createContactResponse_contactArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newCreateContact' smart constructor.
data CreateContact = CreateContact'
  { -- | The full name of the contact or escalation plan.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | A token ensuring that the operation is called only once with the
    -- specified details.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Adds a tag to the target. You can only tag resources created in the
    -- first Region of your replication set.
    tags :: Prelude.Maybe [Tag],
    -- | The short name to quickly identify a contact or escalation plan. The
    -- contact alias must be unique and identifiable.
    alias :: Prelude.Text,
    -- | To create an escalation plan use @ESCALATION@. To create a contact use
    -- @PERSONAL@.
    type' :: ContactType,
    -- | A list of stages. A contact has an engagement plan with stages that
    -- contact specified contact channels. An escalation plan uses stages that
    -- contact specified contacts.
    plan :: Plan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'createContact_displayName' - The full name of the contact or escalation plan.
--
-- 'idempotencyToken', 'createContact_idempotencyToken' - A token ensuring that the operation is called only once with the
-- specified details.
--
-- 'tags', 'createContact_tags' - Adds a tag to the target. You can only tag resources created in the
-- first Region of your replication set.
--
-- 'alias', 'createContact_alias' - The short name to quickly identify a contact or escalation plan. The
-- contact alias must be unique and identifiable.
--
-- 'type'', 'createContact_type' - To create an escalation plan use @ESCALATION@. To create a contact use
-- @PERSONAL@.
--
-- 'plan', 'createContact_plan' - A list of stages. A contact has an engagement plan with stages that
-- contact specified contact channels. An escalation plan uses stages that
-- contact specified contacts.
newCreateContact ::
  -- | 'alias'
  Prelude.Text ->
  -- | 'type''
  ContactType ->
  -- | 'plan'
  Plan ->
  CreateContact
newCreateContact pAlias_ pType_ pPlan_ =
  CreateContact'
    { displayName = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      alias = pAlias_,
      type' = pType_,
      plan = pPlan_
    }

-- | The full name of the contact or escalation plan.
createContact_displayName :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_displayName = Lens.lens (\CreateContact' {displayName} -> displayName) (\s@CreateContact' {} a -> s {displayName = a} :: CreateContact)

-- | A token ensuring that the operation is called only once with the
-- specified details.
createContact_idempotencyToken :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_idempotencyToken = Lens.lens (\CreateContact' {idempotencyToken} -> idempotencyToken) (\s@CreateContact' {} a -> s {idempotencyToken = a} :: CreateContact)

-- | Adds a tag to the target. You can only tag resources created in the
-- first Region of your replication set.
createContact_tags :: Lens.Lens' CreateContact (Prelude.Maybe [Tag])
createContact_tags = Lens.lens (\CreateContact' {tags} -> tags) (\s@CreateContact' {} a -> s {tags = a} :: CreateContact) Prelude.. Lens.mapping Lens.coerced

-- | The short name to quickly identify a contact or escalation plan. The
-- contact alias must be unique and identifiable.
createContact_alias :: Lens.Lens' CreateContact Prelude.Text
createContact_alias = Lens.lens (\CreateContact' {alias} -> alias) (\s@CreateContact' {} a -> s {alias = a} :: CreateContact)

-- | To create an escalation plan use @ESCALATION@. To create a contact use
-- @PERSONAL@.
createContact_type :: Lens.Lens' CreateContact ContactType
createContact_type = Lens.lens (\CreateContact' {type'} -> type') (\s@CreateContact' {} a -> s {type' = a} :: CreateContact)

-- | A list of stages. A contact has an engagement plan with stages that
-- contact specified contact channels. An escalation plan uses stages that
-- contact specified contacts.
createContact_plan :: Lens.Lens' CreateContact Plan
createContact_plan = Lens.lens (\CreateContact' {plan} -> plan) (\s@CreateContact' {} a -> s {plan = a} :: CreateContact)

instance Core.AWSRequest CreateContact where
  type
    AWSResponse CreateContact =
      CreateContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ContactArn")
      )

instance Prelude.Hashable CreateContact where
  hashWithSalt _salt CreateContact' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` plan

instance Prelude.NFData CreateContact where
  rnf CreateContact' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf plan

instance Data.ToHeaders CreateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.CreateContact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateContact where
  toJSON CreateContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Alias" Data..= alias),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Plan" Data..= plan)
          ]
      )

instance Data.ToPath CreateContact where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the created contact or escalation
    -- plan.
    contactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createContactResponse_httpStatus' - The response's http status code.
--
-- 'contactArn', 'createContactResponse_contactArn' - The Amazon Resource Name (ARN) of the created contact or escalation
-- plan.
newCreateContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'contactArn'
  Prelude.Text ->
  CreateContactResponse
newCreateContactResponse pHttpStatus_ pContactArn_ =
  CreateContactResponse'
    { httpStatus = pHttpStatus_,
      contactArn = pContactArn_
    }

-- | The response's http status code.
createContactResponse_httpStatus :: Lens.Lens' CreateContactResponse Prelude.Int
createContactResponse_httpStatus = Lens.lens (\CreateContactResponse' {httpStatus} -> httpStatus) (\s@CreateContactResponse' {} a -> s {httpStatus = a} :: CreateContactResponse)

-- | The Amazon Resource Name (ARN) of the created contact or escalation
-- plan.
createContactResponse_contactArn :: Lens.Lens' CreateContactResponse Prelude.Text
createContactResponse_contactArn = Lens.lens (\CreateContactResponse' {contactArn} -> contactArn) (\s@CreateContactResponse' {} a -> s {contactArn = a} :: CreateContactResponse)

instance Prelude.NFData CreateContactResponse where
  rnf CreateContactResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contactArn
