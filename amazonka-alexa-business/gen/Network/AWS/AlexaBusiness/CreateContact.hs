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
-- Module      : Network.AWS.AlexaBusiness.CreateContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact with the specified details.
module Network.AWS.AlexaBusiness.CreateContact
  ( -- * Creating a Request
    CreateContact (..),
    newCreateContact,

    -- * Request Lenses
    createContact_phoneNumber,
    createContact_phoneNumbers,
    createContact_tags,
    createContact_clientRequestToken,
    createContact_displayName,
    createContact_lastName,
    createContact_sipAddresses,
    createContact_firstName,

    -- * Destructuring the Response
    CreateContactResponse (..),
    newCreateContactResponse,

    -- * Response Lenses
    createContactResponse_contactArn,
    createContactResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateContact' smart constructor.
data CreateContact = CreateContact'
  { -- | The phone number of the contact in E.164 format. The phone number type
    -- defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We
    -- recommend that you use PhoneNumbers, which lets you specify the phone
    -- number type and multiple numbers.
    phoneNumber :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Core.Maybe [PhoneNumber],
    -- | The tags to be added to the specified resource. Do not provide system
    -- tags.
    tags :: Core.Maybe [Tag],
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name of the contact to display on the console.
    displayName :: Core.Maybe Core.Text,
    -- | The last name of the contact that is used to call the contact on the
    -- device.
    lastName :: Core.Maybe Core.Text,
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Core.Maybe [SipAddress],
    -- | The first name of the contact that is used to call the contact on the
    -- device.
    firstName :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'createContact_phoneNumber' - The phone number of the contact in E.164 format. The phone number type
-- defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
--
-- 'phoneNumbers', 'createContact_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'tags', 'createContact_tags' - The tags to be added to the specified resource. Do not provide system
-- tags.
--
-- 'clientRequestToken', 'createContact_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'displayName', 'createContact_displayName' - The name of the contact to display on the console.
--
-- 'lastName', 'createContact_lastName' - The last name of the contact that is used to call the contact on the
-- device.
--
-- 'sipAddresses', 'createContact_sipAddresses' - The list of SIP addresses for the contact.
--
-- 'firstName', 'createContact_firstName' - The first name of the contact that is used to call the contact on the
-- device.
newCreateContact ::
  -- | 'firstName'
  Core.Text ->
  CreateContact
newCreateContact pFirstName_ =
  CreateContact'
    { phoneNumber = Core.Nothing,
      phoneNumbers = Core.Nothing,
      tags = Core.Nothing,
      clientRequestToken = Core.Nothing,
      displayName = Core.Nothing,
      lastName = Core.Nothing,
      sipAddresses = Core.Nothing,
      firstName = pFirstName_
    }

-- | The phone number of the contact in E.164 format. The phone number type
-- defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
createContact_phoneNumber :: Lens.Lens' CreateContact (Core.Maybe Core.Text)
createContact_phoneNumber = Lens.lens (\CreateContact' {phoneNumber} -> phoneNumber) (\s@CreateContact' {} a -> s {phoneNumber = a} :: CreateContact) Core.. Lens.mapping Core._Sensitive

-- | The list of phone numbers for the contact.
createContact_phoneNumbers :: Lens.Lens' CreateContact (Core.Maybe [PhoneNumber])
createContact_phoneNumbers = Lens.lens (\CreateContact' {phoneNumbers} -> phoneNumbers) (\s@CreateContact' {} a -> s {phoneNumbers = a} :: CreateContact) Core.. Lens.mapping Lens._Coerce

-- | The tags to be added to the specified resource. Do not provide system
-- tags.
createContact_tags :: Lens.Lens' CreateContact (Core.Maybe [Tag])
createContact_tags = Lens.lens (\CreateContact' {tags} -> tags) (\s@CreateContact' {} a -> s {tags = a} :: CreateContact) Core.. Lens.mapping Lens._Coerce

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createContact_clientRequestToken :: Lens.Lens' CreateContact (Core.Maybe Core.Text)
createContact_clientRequestToken = Lens.lens (\CreateContact' {clientRequestToken} -> clientRequestToken) (\s@CreateContact' {} a -> s {clientRequestToken = a} :: CreateContact)

-- | The name of the contact to display on the console.
createContact_displayName :: Lens.Lens' CreateContact (Core.Maybe Core.Text)
createContact_displayName = Lens.lens (\CreateContact' {displayName} -> displayName) (\s@CreateContact' {} a -> s {displayName = a} :: CreateContact)

-- | The last name of the contact that is used to call the contact on the
-- device.
createContact_lastName :: Lens.Lens' CreateContact (Core.Maybe Core.Text)
createContact_lastName = Lens.lens (\CreateContact' {lastName} -> lastName) (\s@CreateContact' {} a -> s {lastName = a} :: CreateContact)

-- | The list of SIP addresses for the contact.
createContact_sipAddresses :: Lens.Lens' CreateContact (Core.Maybe [SipAddress])
createContact_sipAddresses = Lens.lens (\CreateContact' {sipAddresses} -> sipAddresses) (\s@CreateContact' {} a -> s {sipAddresses = a} :: CreateContact) Core.. Lens.mapping Lens._Coerce

-- | The first name of the contact that is used to call the contact on the
-- device.
createContact_firstName :: Lens.Lens' CreateContact Core.Text
createContact_firstName = Lens.lens (\CreateContact' {firstName} -> firstName) (\s@CreateContact' {} a -> s {firstName = a} :: CreateContact)

instance Core.AWSRequest CreateContact where
  type
    AWSResponse CreateContact =
      CreateContactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactResponse'
            Core.<$> (x Core..?> "ContactArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateContact

instance Core.NFData CreateContact

instance Core.ToHeaders CreateContact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateContact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateContact where
  toJSON CreateContact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("PhoneNumbers" Core..=) Core.<$> phoneNumbers,
            ("Tags" Core..=) Core.<$> tags,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("LastName" Core..=) Core.<$> lastName,
            ("SipAddresses" Core..=) Core.<$> sipAddresses,
            Core.Just ("FirstName" Core..= firstName)
          ]
      )

instance Core.ToPath CreateContact where
  toPath = Core.const "/"

instance Core.ToQuery CreateContact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { -- | The ARN of the newly created address book.
    contactArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateContactResponse
newCreateContactResponse pHttpStatus_ =
  CreateContactResponse'
    { contactArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created address book.
createContactResponse_contactArn :: Lens.Lens' CreateContactResponse (Core.Maybe Core.Text)
createContactResponse_contactArn = Lens.lens (\CreateContactResponse' {contactArn} -> contactArn) (\s@CreateContactResponse' {} a -> s {contactArn = a} :: CreateContactResponse)

-- | The response's http status code.
createContactResponse_httpStatus :: Lens.Lens' CreateContactResponse Core.Int
createContactResponse_httpStatus = Lens.lens (\CreateContactResponse' {httpStatus} -> httpStatus) (\s@CreateContactResponse' {} a -> s {httpStatus = a} :: CreateContactResponse)

instance Core.NFData CreateContactResponse
