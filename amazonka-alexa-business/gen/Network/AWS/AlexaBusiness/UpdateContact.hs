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
-- Module      : Network.AWS.AlexaBusiness.UpdateContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contact details by the contact ARN.
module Network.AWS.AlexaBusiness.UpdateContact
  ( -- * Creating a Request
    UpdateContact (..),
    newUpdateContact,

    -- * Request Lenses
    updateContact_phoneNumber,
    updateContact_phoneNumbers,
    updateContact_displayName,
    updateContact_firstName,
    updateContact_lastName,
    updateContact_sipAddresses,
    updateContact_contactArn,

    -- * Destructuring the Response
    UpdateContactResponse (..),
    newUpdateContactResponse,

    -- * Response Lenses
    updateContactResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContact' smart constructor.
data UpdateContact = UpdateContact'
  { -- | The updated phone number of the contact. The phone number type defaults
    -- to WORK. You can either specify PhoneNumber or PhoneNumbers. We
    -- recommend that you use PhoneNumbers, which lets you specify the phone
    -- number type and multiple numbers.
    phoneNumber :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Core.Maybe [PhoneNumber],
    -- | The updated display name of the contact.
    displayName :: Core.Maybe Core.Text,
    -- | The updated first name of the contact.
    firstName :: Core.Maybe Core.Text,
    -- | The updated last name of the contact.
    lastName :: Core.Maybe Core.Text,
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Core.Maybe [SipAddress],
    -- | The ARN of the contact to update.
    contactArn :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'updateContact_phoneNumber' - The updated phone number of the contact. The phone number type defaults
-- to WORK. You can either specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
--
-- 'phoneNumbers', 'updateContact_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'displayName', 'updateContact_displayName' - The updated display name of the contact.
--
-- 'firstName', 'updateContact_firstName' - The updated first name of the contact.
--
-- 'lastName', 'updateContact_lastName' - The updated last name of the contact.
--
-- 'sipAddresses', 'updateContact_sipAddresses' - The list of SIP addresses for the contact.
--
-- 'contactArn', 'updateContact_contactArn' - The ARN of the contact to update.
newUpdateContact ::
  -- | 'contactArn'
  Core.Text ->
  UpdateContact
newUpdateContact pContactArn_ =
  UpdateContact'
    { phoneNumber = Core.Nothing,
      phoneNumbers = Core.Nothing,
      displayName = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      sipAddresses = Core.Nothing,
      contactArn = pContactArn_
    }

-- | The updated phone number of the contact. The phone number type defaults
-- to WORK. You can either specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
updateContact_phoneNumber :: Lens.Lens' UpdateContact (Core.Maybe Core.Text)
updateContact_phoneNumber = Lens.lens (\UpdateContact' {phoneNumber} -> phoneNumber) (\s@UpdateContact' {} a -> s {phoneNumber = a} :: UpdateContact) Core.. Lens.mapping Core._Sensitive

-- | The list of phone numbers for the contact.
updateContact_phoneNumbers :: Lens.Lens' UpdateContact (Core.Maybe [PhoneNumber])
updateContact_phoneNumbers = Lens.lens (\UpdateContact' {phoneNumbers} -> phoneNumbers) (\s@UpdateContact' {} a -> s {phoneNumbers = a} :: UpdateContact) Core.. Lens.mapping Lens._Coerce

-- | The updated display name of the contact.
updateContact_displayName :: Lens.Lens' UpdateContact (Core.Maybe Core.Text)
updateContact_displayName = Lens.lens (\UpdateContact' {displayName} -> displayName) (\s@UpdateContact' {} a -> s {displayName = a} :: UpdateContact)

-- | The updated first name of the contact.
updateContact_firstName :: Lens.Lens' UpdateContact (Core.Maybe Core.Text)
updateContact_firstName = Lens.lens (\UpdateContact' {firstName} -> firstName) (\s@UpdateContact' {} a -> s {firstName = a} :: UpdateContact)

-- | The updated last name of the contact.
updateContact_lastName :: Lens.Lens' UpdateContact (Core.Maybe Core.Text)
updateContact_lastName = Lens.lens (\UpdateContact' {lastName} -> lastName) (\s@UpdateContact' {} a -> s {lastName = a} :: UpdateContact)

-- | The list of SIP addresses for the contact.
updateContact_sipAddresses :: Lens.Lens' UpdateContact (Core.Maybe [SipAddress])
updateContact_sipAddresses = Lens.lens (\UpdateContact' {sipAddresses} -> sipAddresses) (\s@UpdateContact' {} a -> s {sipAddresses = a} :: UpdateContact) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the contact to update.
updateContact_contactArn :: Lens.Lens' UpdateContact Core.Text
updateContact_contactArn = Lens.lens (\UpdateContact' {contactArn} -> contactArn) (\s@UpdateContact' {} a -> s {contactArn = a} :: UpdateContact)

instance Core.AWSRequest UpdateContact where
  type
    AWSResponse UpdateContact =
      UpdateContactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateContact

instance Core.NFData UpdateContact

instance Core.ToHeaders UpdateContact where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateContact" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContact where
  toJSON UpdateContact' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("PhoneNumbers" Core..=) Core.<$> phoneNumbers,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName,
            ("SipAddresses" Core..=) Core.<$> sipAddresses,
            Core.Just ("ContactArn" Core..= contactArn)
          ]
      )

instance Core.ToPath UpdateContact where
  toPath = Core.const "/"

instance Core.ToQuery UpdateContact where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContactResponse' smart constructor.
data UpdateContactResponse = UpdateContactResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateContactResponse
newUpdateContactResponse pHttpStatus_ =
  UpdateContactResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateContactResponse_httpStatus :: Lens.Lens' UpdateContactResponse Core.Int
updateContactResponse_httpStatus = Lens.lens (\UpdateContactResponse' {httpStatus} -> httpStatus) (\s@UpdateContactResponse' {} a -> s {httpStatus = a} :: UpdateContactResponse)

instance Core.NFData UpdateContactResponse
