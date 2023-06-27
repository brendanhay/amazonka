{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.Types.RequestInspectionACFP
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RequestInspectionACFP where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.AddressField
import Amazonka.WAFV2.Types.EmailField
import Amazonka.WAFV2.Types.PasswordField
import Amazonka.WAFV2.Types.PayloadType
import Amazonka.WAFV2.Types.PhoneNumberField
import Amazonka.WAFV2.Types.UsernameField

-- | The criteria for inspecting account creation requests, used by the ACFP
-- rule group to validate and track account creation attempts.
--
-- This is part of the @AWSManagedRulesACFPRuleSet@ configuration in
-- @ManagedRuleGroupConfig@.
--
-- In these settings, you specify how your application accepts account
-- creation attempts by providing the request payload type and the names of
-- the fields within the request body where the username, password, email,
-- and primary address and phone number fields are provided.
--
-- /See:/ 'newRequestInspectionACFP' smart constructor.
data RequestInspectionACFP = RequestInspectionACFP'
  { -- | The names of the fields in the request payload that contain your
    -- customer\'s primary physical address.
    --
    -- Order the address fields in the array exactly as they are ordered in the
    -- request payload.
    --
    -- How you specify the address fields depends on the request inspection
    -- payload type.
    --
    -- -   For JSON payloads, specify the field identifiers in JSON pointer
    --     syntax. For information about the JSON Pointer syntax, see the
    --     Internet Engineering Task Force (IETF) documentation
    --     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
    --
    --     For example, for the JSON payload
    --     @{ \"form\": { \"primaryaddressline1\": \"THE_ADDRESS1\", \"primaryaddressline2\": \"THE_ADDRESS2\", \"primaryaddressline3\": \"THE_ADDRESS3\" } }@,
    --     the address field idenfiers are @\/form\/primaryaddressline1@,
    --     @\/form\/primaryaddressline2@, and @\/form\/primaryaddressline3@.
    --
    -- -   For form encoded payload types, use the HTML form names.
    --
    --     For example, for an HTML form with input elements named
    --     @primaryaddressline1@, @primaryaddressline2@, and
    --     @primaryaddressline3@, the address fields identifiers are
    --     @primaryaddressline1@, @primaryaddressline2@, and
    --     @primaryaddressline3@.
    addressFields :: Prelude.Maybe [AddressField],
    -- | The name of the field in the request payload that contains your
    -- customer\'s email.
    --
    -- How you specify this depends on the request inspection payload type.
    --
    -- -   For JSON payloads, specify the field name in JSON pointer syntax.
    --     For information about the JSON Pointer syntax, see the Internet
    --     Engineering Task Force (IETF) documentation
    --     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
    --
    --     For example, for the JSON payload
    --     @{ \"form\": { \"email\": \"THE_EMAIL\" } }@, the email field
    --     specification is @\/form\/email@.
    --
    -- -   For form encoded payload types, use the HTML form names.
    --
    --     For example, for an HTML form with the input element named @email1@,
    --     the email field specification is @email1@.
    emailField :: Prelude.Maybe EmailField,
    -- | The name of the field in the request payload that contains your
    -- customer\'s password.
    --
    -- How you specify this depends on the request inspection payload type.
    --
    -- -   For JSON payloads, specify the field name in JSON pointer syntax.
    --     For information about the JSON Pointer syntax, see the Internet
    --     Engineering Task Force (IETF) documentation
    --     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
    --
    --     For example, for the JSON payload
    --     @{ \"form\": { \"password\": \"THE_PASSWORD\" } }@, the password
    --     field specification is @\/form\/password@.
    --
    -- -   For form encoded payload types, use the HTML form names.
    --
    --     For example, for an HTML form with the input element named
    --     @password1@, the password field specification is @password1@.
    passwordField :: Prelude.Maybe PasswordField,
    -- | The names of the fields in the request payload that contain your
    -- customer\'s primary phone number.
    --
    -- Order the phone number fields in the array exactly as they are ordered
    -- in the request payload.
    --
    -- How you specify the phone number fields depends on the request
    -- inspection payload type.
    --
    -- -   For JSON payloads, specify the field identifiers in JSON pointer
    --     syntax. For information about the JSON Pointer syntax, see the
    --     Internet Engineering Task Force (IETF) documentation
    --     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
    --
    --     For example, for the JSON payload
    --     @{ \"form\": { \"primaryphoneline1\": \"THE_PHONE1\", \"primaryphoneline2\": \"THE_PHONE2\", \"primaryphoneline3\": \"THE_PHONE3\" } }@,
    --     the phone number field identifiers are @\/form\/primaryphoneline1@,
    --     @\/form\/primaryphoneline2@, and @\/form\/primaryphoneline3@.
    --
    -- -   For form encoded payload types, use the HTML form names.
    --
    --     For example, for an HTML form with input elements named
    --     @primaryphoneline1@, @primaryphoneline2@, and @primaryphoneline3@,
    --     the phone number field identifiers are @primaryphoneline1@,
    --     @primaryphoneline2@, and @primaryphoneline3@.
    phoneNumberFields :: Prelude.Maybe [PhoneNumberField],
    -- | The name of the field in the request payload that contains your
    -- customer\'s username.
    --
    -- How you specify this depends on the request inspection payload type.
    --
    -- -   For JSON payloads, specify the field name in JSON pointer syntax.
    --     For information about the JSON Pointer syntax, see the Internet
    --     Engineering Task Force (IETF) documentation
    --     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
    --
    --     For example, for the JSON payload
    --     @{ \"form\": { \"username\": \"THE_USERNAME\" } }@, the username
    --     field specification is @\/form\/username@.
    --
    -- -   For form encoded payload types, use the HTML form names.
    --
    --     For example, for an HTML form with the input element named
    --     @username1@, the username field specification is @username1@
    usernameField :: Prelude.Maybe UsernameField,
    -- | The payload type for your account creation endpoint, either JSON or form
    -- encoded.
    payloadType :: PayloadType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestInspectionACFP' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressFields', 'requestInspectionACFP_addressFields' - The names of the fields in the request payload that contain your
-- customer\'s primary physical address.
--
-- Order the address fields in the array exactly as they are ordered in the
-- request payload.
--
-- How you specify the address fields depends on the request inspection
-- payload type.
--
-- -   For JSON payloads, specify the field identifiers in JSON pointer
--     syntax. For information about the JSON Pointer syntax, see the
--     Internet Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"primaryaddressline1\": \"THE_ADDRESS1\", \"primaryaddressline2\": \"THE_ADDRESS2\", \"primaryaddressline3\": \"THE_ADDRESS3\" } }@,
--     the address field idenfiers are @\/form\/primaryaddressline1@,
--     @\/form\/primaryaddressline2@, and @\/form\/primaryaddressline3@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with input elements named
--     @primaryaddressline1@, @primaryaddressline2@, and
--     @primaryaddressline3@, the address fields identifiers are
--     @primaryaddressline1@, @primaryaddressline2@, and
--     @primaryaddressline3@.
--
-- 'emailField', 'requestInspectionACFP_emailField' - The name of the field in the request payload that contains your
-- customer\'s email.
--
-- How you specify this depends on the request inspection payload type.
--
-- -   For JSON payloads, specify the field name in JSON pointer syntax.
--     For information about the JSON Pointer syntax, see the Internet
--     Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"email\": \"THE_EMAIL\" } }@, the email field
--     specification is @\/form\/email@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with the input element named @email1@,
--     the email field specification is @email1@.
--
-- 'passwordField', 'requestInspectionACFP_passwordField' - The name of the field in the request payload that contains your
-- customer\'s password.
--
-- How you specify this depends on the request inspection payload type.
--
-- -   For JSON payloads, specify the field name in JSON pointer syntax.
--     For information about the JSON Pointer syntax, see the Internet
--     Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"password\": \"THE_PASSWORD\" } }@, the password
--     field specification is @\/form\/password@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with the input element named
--     @password1@, the password field specification is @password1@.
--
-- 'phoneNumberFields', 'requestInspectionACFP_phoneNumberFields' - The names of the fields in the request payload that contain your
-- customer\'s primary phone number.
--
-- Order the phone number fields in the array exactly as they are ordered
-- in the request payload.
--
-- How you specify the phone number fields depends on the request
-- inspection payload type.
--
-- -   For JSON payloads, specify the field identifiers in JSON pointer
--     syntax. For information about the JSON Pointer syntax, see the
--     Internet Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"primaryphoneline1\": \"THE_PHONE1\", \"primaryphoneline2\": \"THE_PHONE2\", \"primaryphoneline3\": \"THE_PHONE3\" } }@,
--     the phone number field identifiers are @\/form\/primaryphoneline1@,
--     @\/form\/primaryphoneline2@, and @\/form\/primaryphoneline3@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with input elements named
--     @primaryphoneline1@, @primaryphoneline2@, and @primaryphoneline3@,
--     the phone number field identifiers are @primaryphoneline1@,
--     @primaryphoneline2@, and @primaryphoneline3@.
--
-- 'usernameField', 'requestInspectionACFP_usernameField' - The name of the field in the request payload that contains your
-- customer\'s username.
--
-- How you specify this depends on the request inspection payload type.
--
-- -   For JSON payloads, specify the field name in JSON pointer syntax.
--     For information about the JSON Pointer syntax, see the Internet
--     Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"username\": \"THE_USERNAME\" } }@, the username
--     field specification is @\/form\/username@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with the input element named
--     @username1@, the username field specification is @username1@
--
-- 'payloadType', 'requestInspectionACFP_payloadType' - The payload type for your account creation endpoint, either JSON or form
-- encoded.
newRequestInspectionACFP ::
  -- | 'payloadType'
  PayloadType ->
  RequestInspectionACFP
newRequestInspectionACFP pPayloadType_ =
  RequestInspectionACFP'
    { addressFields =
        Prelude.Nothing,
      emailField = Prelude.Nothing,
      passwordField = Prelude.Nothing,
      phoneNumberFields = Prelude.Nothing,
      usernameField = Prelude.Nothing,
      payloadType = pPayloadType_
    }

-- | The names of the fields in the request payload that contain your
-- customer\'s primary physical address.
--
-- Order the address fields in the array exactly as they are ordered in the
-- request payload.
--
-- How you specify the address fields depends on the request inspection
-- payload type.
--
-- -   For JSON payloads, specify the field identifiers in JSON pointer
--     syntax. For information about the JSON Pointer syntax, see the
--     Internet Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"primaryaddressline1\": \"THE_ADDRESS1\", \"primaryaddressline2\": \"THE_ADDRESS2\", \"primaryaddressline3\": \"THE_ADDRESS3\" } }@,
--     the address field idenfiers are @\/form\/primaryaddressline1@,
--     @\/form\/primaryaddressline2@, and @\/form\/primaryaddressline3@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with input elements named
--     @primaryaddressline1@, @primaryaddressline2@, and
--     @primaryaddressline3@, the address fields identifiers are
--     @primaryaddressline1@, @primaryaddressline2@, and
--     @primaryaddressline3@.
requestInspectionACFP_addressFields :: Lens.Lens' RequestInspectionACFP (Prelude.Maybe [AddressField])
requestInspectionACFP_addressFields = Lens.lens (\RequestInspectionACFP' {addressFields} -> addressFields) (\s@RequestInspectionACFP' {} a -> s {addressFields = a} :: RequestInspectionACFP) Prelude.. Lens.mapping Lens.coerced

-- | The name of the field in the request payload that contains your
-- customer\'s email.
--
-- How you specify this depends on the request inspection payload type.
--
-- -   For JSON payloads, specify the field name in JSON pointer syntax.
--     For information about the JSON Pointer syntax, see the Internet
--     Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"email\": \"THE_EMAIL\" } }@, the email field
--     specification is @\/form\/email@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with the input element named @email1@,
--     the email field specification is @email1@.
requestInspectionACFP_emailField :: Lens.Lens' RequestInspectionACFP (Prelude.Maybe EmailField)
requestInspectionACFP_emailField = Lens.lens (\RequestInspectionACFP' {emailField} -> emailField) (\s@RequestInspectionACFP' {} a -> s {emailField = a} :: RequestInspectionACFP)

-- | The name of the field in the request payload that contains your
-- customer\'s password.
--
-- How you specify this depends on the request inspection payload type.
--
-- -   For JSON payloads, specify the field name in JSON pointer syntax.
--     For information about the JSON Pointer syntax, see the Internet
--     Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"password\": \"THE_PASSWORD\" } }@, the password
--     field specification is @\/form\/password@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with the input element named
--     @password1@, the password field specification is @password1@.
requestInspectionACFP_passwordField :: Lens.Lens' RequestInspectionACFP (Prelude.Maybe PasswordField)
requestInspectionACFP_passwordField = Lens.lens (\RequestInspectionACFP' {passwordField} -> passwordField) (\s@RequestInspectionACFP' {} a -> s {passwordField = a} :: RequestInspectionACFP)

-- | The names of the fields in the request payload that contain your
-- customer\'s primary phone number.
--
-- Order the phone number fields in the array exactly as they are ordered
-- in the request payload.
--
-- How you specify the phone number fields depends on the request
-- inspection payload type.
--
-- -   For JSON payloads, specify the field identifiers in JSON pointer
--     syntax. For information about the JSON Pointer syntax, see the
--     Internet Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"primaryphoneline1\": \"THE_PHONE1\", \"primaryphoneline2\": \"THE_PHONE2\", \"primaryphoneline3\": \"THE_PHONE3\" } }@,
--     the phone number field identifiers are @\/form\/primaryphoneline1@,
--     @\/form\/primaryphoneline2@, and @\/form\/primaryphoneline3@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with input elements named
--     @primaryphoneline1@, @primaryphoneline2@, and @primaryphoneline3@,
--     the phone number field identifiers are @primaryphoneline1@,
--     @primaryphoneline2@, and @primaryphoneline3@.
requestInspectionACFP_phoneNumberFields :: Lens.Lens' RequestInspectionACFP (Prelude.Maybe [PhoneNumberField])
requestInspectionACFP_phoneNumberFields = Lens.lens (\RequestInspectionACFP' {phoneNumberFields} -> phoneNumberFields) (\s@RequestInspectionACFP' {} a -> s {phoneNumberFields = a} :: RequestInspectionACFP) Prelude.. Lens.mapping Lens.coerced

-- | The name of the field in the request payload that contains your
-- customer\'s username.
--
-- How you specify this depends on the request inspection payload type.
--
-- -   For JSON payloads, specify the field name in JSON pointer syntax.
--     For information about the JSON Pointer syntax, see the Internet
--     Engineering Task Force (IETF) documentation
--     <https://tools.ietf.org/html/rfc6901 JavaScript Object Notation (JSON) Pointer>.
--
--     For example, for the JSON payload
--     @{ \"form\": { \"username\": \"THE_USERNAME\" } }@, the username
--     field specification is @\/form\/username@.
--
-- -   For form encoded payload types, use the HTML form names.
--
--     For example, for an HTML form with the input element named
--     @username1@, the username field specification is @username1@
requestInspectionACFP_usernameField :: Lens.Lens' RequestInspectionACFP (Prelude.Maybe UsernameField)
requestInspectionACFP_usernameField = Lens.lens (\RequestInspectionACFP' {usernameField} -> usernameField) (\s@RequestInspectionACFP' {} a -> s {usernameField = a} :: RequestInspectionACFP)

-- | The payload type for your account creation endpoint, either JSON or form
-- encoded.
requestInspectionACFP_payloadType :: Lens.Lens' RequestInspectionACFP PayloadType
requestInspectionACFP_payloadType = Lens.lens (\RequestInspectionACFP' {payloadType} -> payloadType) (\s@RequestInspectionACFP' {} a -> s {payloadType = a} :: RequestInspectionACFP)

instance Data.FromJSON RequestInspectionACFP where
  parseJSON =
    Data.withObject
      "RequestInspectionACFP"
      ( \x ->
          RequestInspectionACFP'
            Prelude.<$> (x Data..:? "AddressFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EmailField")
            Prelude.<*> (x Data..:? "PasswordField")
            Prelude.<*> ( x
                            Data..:? "PhoneNumberFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UsernameField")
            Prelude.<*> (x Data..: "PayloadType")
      )

instance Prelude.Hashable RequestInspectionACFP where
  hashWithSalt _salt RequestInspectionACFP' {..} =
    _salt
      `Prelude.hashWithSalt` addressFields
      `Prelude.hashWithSalt` emailField
      `Prelude.hashWithSalt` passwordField
      `Prelude.hashWithSalt` phoneNumberFields
      `Prelude.hashWithSalt` usernameField
      `Prelude.hashWithSalt` payloadType

instance Prelude.NFData RequestInspectionACFP where
  rnf RequestInspectionACFP' {..} =
    Prelude.rnf addressFields
      `Prelude.seq` Prelude.rnf emailField
      `Prelude.seq` Prelude.rnf passwordField
      `Prelude.seq` Prelude.rnf phoneNumberFields
      `Prelude.seq` Prelude.rnf usernameField
      `Prelude.seq` Prelude.rnf payloadType

instance Data.ToJSON RequestInspectionACFP where
  toJSON RequestInspectionACFP' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddressFields" Data..=) Prelude.<$> addressFields,
            ("EmailField" Data..=) Prelude.<$> emailField,
            ("PasswordField" Data..=) Prelude.<$> passwordField,
            ("PhoneNumberFields" Data..=)
              Prelude.<$> phoneNumberFields,
            ("UsernameField" Data..=) Prelude.<$> usernameField,
            Prelude.Just ("PayloadType" Data..= payloadType)
          ]
      )
