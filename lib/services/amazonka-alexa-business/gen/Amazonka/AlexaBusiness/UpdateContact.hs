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
-- Module      : Amazonka.AlexaBusiness.UpdateContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contact details by the contact ARN.
module Amazonka.AlexaBusiness.UpdateContact
  ( -- * Creating a Request
    UpdateContact (..),
    newUpdateContact,

    -- * Request Lenses
    updateContact_displayName,
    updateContact_firstName,
    updateContact_lastName,
    updateContact_phoneNumber,
    updateContact_phoneNumbers,
    updateContact_sipAddresses,
    updateContact_contactArn,

    -- * Destructuring the Response
    UpdateContactResponse (..),
    newUpdateContactResponse,

    -- * Response Lenses
    updateContactResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContact' smart constructor.
data UpdateContact = UpdateContact'
  { -- | The updated display name of the contact.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The updated first name of the contact.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The updated last name of the contact.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The updated phone number of the contact. The phone number type defaults
    -- to WORK. You can either specify PhoneNumber or PhoneNumbers. We
    -- recommend that you use PhoneNumbers, which lets you specify the phone
    -- number type and multiple numbers.
    phoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The list of phone numbers for the contact.
    phoneNumbers :: Prelude.Maybe [PhoneNumber],
    -- | The list of SIP addresses for the contact.
    sipAddresses :: Prelude.Maybe [SipAddress],
    -- | The ARN of the contact to update.
    contactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'updateContact_displayName' - The updated display name of the contact.
--
-- 'firstName', 'updateContact_firstName' - The updated first name of the contact.
--
-- 'lastName', 'updateContact_lastName' - The updated last name of the contact.
--
-- 'phoneNumber', 'updateContact_phoneNumber' - The updated phone number of the contact. The phone number type defaults
-- to WORK. You can either specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
--
-- 'phoneNumbers', 'updateContact_phoneNumbers' - The list of phone numbers for the contact.
--
-- 'sipAddresses', 'updateContact_sipAddresses' - The list of SIP addresses for the contact.
--
-- 'contactArn', 'updateContact_contactArn' - The ARN of the contact to update.
newUpdateContact ::
  -- | 'contactArn'
  Prelude.Text ->
  UpdateContact
newUpdateContact pContactArn_ =
  UpdateContact'
    { displayName = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      phoneNumbers = Prelude.Nothing,
      sipAddresses = Prelude.Nothing,
      contactArn = pContactArn_
    }

-- | The updated display name of the contact.
updateContact_displayName :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_displayName = Lens.lens (\UpdateContact' {displayName} -> displayName) (\s@UpdateContact' {} a -> s {displayName = a} :: UpdateContact)

-- | The updated first name of the contact.
updateContact_firstName :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_firstName = Lens.lens (\UpdateContact' {firstName} -> firstName) (\s@UpdateContact' {} a -> s {firstName = a} :: UpdateContact)

-- | The updated last name of the contact.
updateContact_lastName :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_lastName = Lens.lens (\UpdateContact' {lastName} -> lastName) (\s@UpdateContact' {} a -> s {lastName = a} :: UpdateContact)

-- | The updated phone number of the contact. The phone number type defaults
-- to WORK. You can either specify PhoneNumber or PhoneNumbers. We
-- recommend that you use PhoneNumbers, which lets you specify the phone
-- number type and multiple numbers.
updateContact_phoneNumber :: Lens.Lens' UpdateContact (Prelude.Maybe Prelude.Text)
updateContact_phoneNumber = Lens.lens (\UpdateContact' {phoneNumber} -> phoneNumber) (\s@UpdateContact' {} a -> s {phoneNumber = a} :: UpdateContact) Prelude.. Lens.mapping Data._Sensitive

-- | The list of phone numbers for the contact.
updateContact_phoneNumbers :: Lens.Lens' UpdateContact (Prelude.Maybe [PhoneNumber])
updateContact_phoneNumbers = Lens.lens (\UpdateContact' {phoneNumbers} -> phoneNumbers) (\s@UpdateContact' {} a -> s {phoneNumbers = a} :: UpdateContact) Prelude.. Lens.mapping Lens.coerced

-- | The list of SIP addresses for the contact.
updateContact_sipAddresses :: Lens.Lens' UpdateContact (Prelude.Maybe [SipAddress])
updateContact_sipAddresses = Lens.lens (\UpdateContact' {sipAddresses} -> sipAddresses) (\s@UpdateContact' {} a -> s {sipAddresses = a} :: UpdateContact) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the contact to update.
updateContact_contactArn :: Lens.Lens' UpdateContact Prelude.Text
updateContact_contactArn = Lens.lens (\UpdateContact' {contactArn} -> contactArn) (\s@UpdateContact' {} a -> s {contactArn = a} :: UpdateContact)

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
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` phoneNumbers
      `Prelude.hashWithSalt` sipAddresses
      `Prelude.hashWithSalt` contactArn

instance Prelude.NFData UpdateContact where
  rnf UpdateContact' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf phoneNumbers
      `Prelude.seq` Prelude.rnf sipAddresses
      `Prelude.seq` Prelude.rnf contactArn

instance Data.ToHeaders UpdateContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.UpdateContact" ::
                          Prelude.ByteString
                      ),
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
            ("FirstName" Data..=) Prelude.<$> firstName,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber,
            ("PhoneNumbers" Data..=) Prelude.<$> phoneNumbers,
            ("SipAddresses" Data..=) Prelude.<$> sipAddresses,
            Prelude.Just ("ContactArn" Data..= contactArn)
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
