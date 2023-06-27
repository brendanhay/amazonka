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
-- Module      : Amazonka.WAFV2.Types.PhoneNumberField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.PhoneNumberField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of a field in the request payload that contains part or all of
-- your customer\'s primary phone number.
--
-- This data type is used in the @RequestInspectionACFP@ data type.
--
-- /See:/ 'newPhoneNumberField' smart constructor.
data PhoneNumberField = PhoneNumberField'
  { -- | The name of a single primary phone number field.
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
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'phoneNumberField_identifier' - The name of a single primary phone number field.
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
newPhoneNumberField ::
  -- | 'identifier'
  Prelude.Text ->
  PhoneNumberField
newPhoneNumberField pIdentifier_ =
  PhoneNumberField' {identifier = pIdentifier_}

-- | The name of a single primary phone number field.
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
phoneNumberField_identifier :: Lens.Lens' PhoneNumberField Prelude.Text
phoneNumberField_identifier = Lens.lens (\PhoneNumberField' {identifier} -> identifier) (\s@PhoneNumberField' {} a -> s {identifier = a} :: PhoneNumberField)

instance Data.FromJSON PhoneNumberField where
  parseJSON =
    Data.withObject
      "PhoneNumberField"
      ( \x ->
          PhoneNumberField'
            Prelude.<$> (x Data..: "Identifier")
      )

instance Prelude.Hashable PhoneNumberField where
  hashWithSalt _salt PhoneNumberField' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData PhoneNumberField where
  rnf PhoneNumberField' {..} = Prelude.rnf identifier

instance Data.ToJSON PhoneNumberField where
  toJSON PhoneNumberField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )
