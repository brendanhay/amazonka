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
-- Module      : Amazonka.SSMContacts.GetContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified contact or escalation plan.
module Amazonka.SSMContacts.GetContact
  ( -- * Creating a Request
    GetContact (..),
    newGetContact,

    -- * Request Lenses
    getContact_contactId,

    -- * Destructuring the Response
    GetContactResponse (..),
    newGetContactResponse,

    -- * Response Lenses
    getContactResponse_displayName,
    getContactResponse_httpStatus,
    getContactResponse_contactArn,
    getContactResponse_alias,
    getContactResponse_type,
    getContactResponse_plan,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newGetContact' smart constructor.
data GetContact = GetContact'
  { -- | The Amazon Resource Name (ARN) of the contact or escalation plan.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'getContact_contactId' - The Amazon Resource Name (ARN) of the contact or escalation plan.
newGetContact ::
  -- | 'contactId'
  Prelude.Text ->
  GetContact
newGetContact pContactId_ =
  GetContact' {contactId = pContactId_}

-- | The Amazon Resource Name (ARN) of the contact or escalation plan.
getContact_contactId :: Lens.Lens' GetContact Prelude.Text
getContact_contactId = Lens.lens (\GetContact' {contactId} -> contactId) (\s@GetContact' {} a -> s {contactId = a} :: GetContact)

instance Core.AWSRequest GetContact where
  type AWSResponse GetContact = GetContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactResponse'
            Prelude.<$> (x Data..?> "DisplayName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ContactArn")
            Prelude.<*> (x Data..:> "Alias")
            Prelude.<*> (x Data..:> "Type")
            Prelude.<*> (x Data..:> "Plan")
      )

instance Prelude.Hashable GetContact where
  hashWithSalt _salt GetContact' {..} =
    _salt `Prelude.hashWithSalt` contactId

instance Prelude.NFData GetContact where
  rnf GetContact' {..} = Prelude.rnf contactId

instance Data.ToHeaders GetContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.GetContact" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContact where
  toJSON GetContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ContactId" Data..= contactId)]
      )

instance Data.ToPath GetContact where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { -- | The full name of the contact or escalation plan.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the contact or escalation plan.
    contactArn :: Prelude.Text,
    -- | The alias of the contact or escalation plan. The alias is unique and
    -- identifiable.
    alias :: Prelude.Text,
    -- | The type of contact, either @PERSONAL@ or @ESCALATION@.
    type' :: ContactType,
    -- | Details about the specific timing or stages and targets of the
    -- escalation plan or engagement plan.
    plan :: Plan
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'getContactResponse_displayName' - The full name of the contact or escalation plan.
--
-- 'httpStatus', 'getContactResponse_httpStatus' - The response's http status code.
--
-- 'contactArn', 'getContactResponse_contactArn' - The ARN of the contact or escalation plan.
--
-- 'alias', 'getContactResponse_alias' - The alias of the contact or escalation plan. The alias is unique and
-- identifiable.
--
-- 'type'', 'getContactResponse_type' - The type of contact, either @PERSONAL@ or @ESCALATION@.
--
-- 'plan', 'getContactResponse_plan' - Details about the specific timing or stages and targets of the
-- escalation plan or engagement plan.
newGetContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  -- | 'type''
  ContactType ->
  -- | 'plan'
  Plan ->
  GetContactResponse
newGetContactResponse
  pHttpStatus_
  pContactArn_
  pAlias_
  pType_
  pPlan_ =
    GetContactResponse'
      { displayName = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        contactArn = pContactArn_,
        alias = pAlias_,
        type' = pType_,
        plan = pPlan_
      }

-- | The full name of the contact or escalation plan.
getContactResponse_displayName :: Lens.Lens' GetContactResponse (Prelude.Maybe Prelude.Text)
getContactResponse_displayName = Lens.lens (\GetContactResponse' {displayName} -> displayName) (\s@GetContactResponse' {} a -> s {displayName = a} :: GetContactResponse)

-- | The response's http status code.
getContactResponse_httpStatus :: Lens.Lens' GetContactResponse Prelude.Int
getContactResponse_httpStatus = Lens.lens (\GetContactResponse' {httpStatus} -> httpStatus) (\s@GetContactResponse' {} a -> s {httpStatus = a} :: GetContactResponse)

-- | The ARN of the contact or escalation plan.
getContactResponse_contactArn :: Lens.Lens' GetContactResponse Prelude.Text
getContactResponse_contactArn = Lens.lens (\GetContactResponse' {contactArn} -> contactArn) (\s@GetContactResponse' {} a -> s {contactArn = a} :: GetContactResponse)

-- | The alias of the contact or escalation plan. The alias is unique and
-- identifiable.
getContactResponse_alias :: Lens.Lens' GetContactResponse Prelude.Text
getContactResponse_alias = Lens.lens (\GetContactResponse' {alias} -> alias) (\s@GetContactResponse' {} a -> s {alias = a} :: GetContactResponse)

-- | The type of contact, either @PERSONAL@ or @ESCALATION@.
getContactResponse_type :: Lens.Lens' GetContactResponse ContactType
getContactResponse_type = Lens.lens (\GetContactResponse' {type'} -> type') (\s@GetContactResponse' {} a -> s {type' = a} :: GetContactResponse)

-- | Details about the specific timing or stages and targets of the
-- escalation plan or engagement plan.
getContactResponse_plan :: Lens.Lens' GetContactResponse Plan
getContactResponse_plan = Lens.lens (\GetContactResponse' {plan} -> plan) (\s@GetContactResponse' {} a -> s {plan = a} :: GetContactResponse)

instance Prelude.NFData GetContactResponse where
  rnf GetContactResponse' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf plan
