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
-- Module      : Amazonka.AlexaBusiness.CreateContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact with the specified details.
module Amazonka.AlexaBusiness.CreateContact
  ( -- * Creating a Request
    CreateContact (..),
    newCreateContact,

    -- * Request Lenses
    createContact_tags,
    createContact_clientRequestToken,
    createContact_sipAddresses,
    createContact_displayName,
    createContact_lastName,
    createContact_phoneNumber,
    createContact_phoneNumbers,
    createContact_firstName,

    -- * Destructuring the Response
    CreateContactResponse (..),
    newCreateContactResponse,

    -- * Response Lenses
    createContactResponse_contactArn,
    createContactResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContact' smart constructor.
data CreateContact = CreateContact'
  { -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Prelude.Maybe [Tag],
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Prelude.Maybe [SipAddress],
    -- | The name of the contact to display on the console.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The last name of the contact that is used to call the contact on the
    -- device.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The phone number of the contact in E.164 format. The phone number type
    -- defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We
    -- recommend that you use PhoneNumbers, which lets you specify the phone
    -- number type and multiple numbers.
    phoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Prelude.Maybe [PhoneNumber],
    -- | The first name of the contact that is used to call the contact on the
    -- device.
    firstName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createContact_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'clientRequestToken', 'createContact_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'sipAddresses', 'createContact_sipAddresses' - The list of SIP addresses for the contact.
--
-- 'displayName', 'createContact_displayName' - The name of the contact to display on the console.
--
-- 'lastName', 'createContact_lastName' - The last name of the contact that is used to call the contact on the
-- device.
--
-- 'phoneNumber', 'createContact_phoneNumber' - The phone number of the contact in E.164 format. The phone number type
-- defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
--
-- 'phoneNumbers', 'createContact_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'firstName', 'createContact_firstName' - The first name of the contact that is used to call the contact on the
-- device.
newCreateContact ::
  -- | 'firstName'
  Prelude.Text ->
  CreateContact
newCreateContact pFirstName_ =
  CreateContact'
    { tags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      sipAddresses = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      firstName = pFirstName_
    }

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createContact_tags :: Lens.Lens' CreateContact (Prelude.Maybe [Tag])
createContact_tags = Lens.lens (\CreateContact' {tags} -> tags) (\s@CreateContact' {} a -> s {tags = a} :: CreateContact) Prelude.. Lens.mapping Lens.coerced

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createContact_clientRequestToken :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_clientRequestToken = Lens.lens (\CreateContact' {clientRequestToken} -> clientRequestToken) (\s@CreateContact' {} a -> s {clientRequestToken = a} :: CreateContact)

-- | The list of SIP addresses for the contact.
createContact_sipAddresses :: Lens.Lens' CreateContact (Prelude.Maybe [SipAddress])
createContact_sipAddresses = Lens.lens (\CreateContact' {sipAddresses} -> sipAddresses) (\s@CreateContact' {} a -> s {sipAddresses = a} :: CreateContact) Prelude.. Lens.mapping Lens.coerced

-- | The name of the contact to display on the console.
createContact_displayName :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_displayName = Lens.lens (\CreateContact' {displayName} -> displayName) (\s@CreateContact' {} a -> s {displayName = a} :: CreateContact)

-- | The last name of the contact that is used to call the contact on the
-- device.
createContact_lastName :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_lastName = Lens.lens (\CreateContact' {lastName} -> lastName) (\s@CreateContact' {} a -> s {lastName = a} :: CreateContact)

-- | The phone number of the contact in E.164 format. The phone number type
-- defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
createContact_phoneNumber :: Lens.Lens' CreateContact (Prelude.Maybe Prelude.Text)
createContact_phoneNumber = Lens.lens (\CreateContact' {phoneNumber} -> phoneNumber) (\s@CreateContact' {} a -> s {phoneNumber = a} :: CreateContact) Prelude.. Lens.mapping Data._Sensitive

-- | The list of phone numbers for the contact.
createContact_phoneNumbers :: Lens.Lens' CreateContact (Prelude.Maybe [PhoneNumber])
createContact_phoneNumbers = Lens.lens (\CreateContact' {phoneNumbers} -> phoneNumbers) (\s@CreateContact' {} a -> s {phoneNumbers = a} :: CreateContact) Prelude.. Lens.mapping Lens.coerced

-- | The first name of the contact that is used to call the contact on the
-- device.
createContact_firstName :: Lens.Lens' CreateContact Prelude.Text
createContact_firstName = Lens.lens (\CreateContact' {firstName} -> firstName) (\s@CreateContact' {} a -> s {firstName = a} :: CreateContact)

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
            Prelude.<$> (x Data..?> "ContactArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContact where
  hashWithSalt _salt CreateContact' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` sipAddresses
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` phoneNumbers
      `Prelude.hashWithSalt` firstName

instance Prelude.NFData CreateContact where
  rnf CreateContact' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf sipAddresses
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf firstName

instance Data.ToHeaders CreateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateContact" ::
                          Prelude.ByteString
                      ),
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
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("SipAddresses" Data..=) Prelude.<$> sipAddresses,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber,
            ("PhoneNumbers" Data..=) Prelude.<$> phoneNumbers,
            Prelude.Just ("FirstName" Data..= firstName)
          ]
      )

instance Data.ToPath CreateContact where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateContact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { -- | The ARN of the newly created address book.
    contactArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'contactArn', 'createContactResponse_contactArn' - The ARN of the newly created address book.
--
-- 'httpStatus', 'createContactResponse_httpStatus' - The response's http status code.
newCreateContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContactResponse
newCreateContactResponse pHttpStatus_ =
  CreateContactResponse'
    { contactArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created address book.
createContactResponse_contactArn :: Lens.Lens' CreateContactResponse (Prelude.Maybe Prelude.Text)
createContactResponse_contactArn = Lens.lens (\CreateContactResponse' {contactArn} -> contactArn) (\s@CreateContactResponse' {} a -> s {contactArn = a} :: CreateContactResponse)

-- | The response's http status code.
createContactResponse_httpStatus :: Lens.Lens' CreateContactResponse Prelude.Int
createContactResponse_httpStatus = Lens.lens (\CreateContactResponse' {httpStatus} -> httpStatus) (\s@CreateContactResponse' {} a -> s {httpStatus = a} :: CreateContactResponse)

instance Prelude.NFData CreateContactResponse where
  rnf CreateContactResponse' {..} =
    Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf httpStatus
