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
-- Module      : Amazonka.WAFV2.Types.RequestInspection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RequestInspection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.PasswordField
import Amazonka.WAFV2.Types.PayloadType
import Amazonka.WAFV2.Types.UsernameField

-- | The criteria for inspecting login requests, used by the ATP rule group
-- to validate credentials usage.
--
-- This is part of the @AWSManagedRulesATPRuleSet@ configuration in
-- @ManagedRuleGroupConfig@.
--
-- In these settings, you specify how your application accepts login
-- attempts by providing the request payload type and the names of the
-- fields within the request body where the username and password are
-- provided.
--
-- /See:/ 'newRequestInspection' smart constructor.
data RequestInspection = RequestInspection'
  { -- | The payload type for your login endpoint, either JSON or form encoded.
    payloadType :: PayloadType,
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
    usernameField :: UsernameField,
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
    passwordField :: PasswordField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestInspection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payloadType', 'requestInspection_payloadType' - The payload type for your login endpoint, either JSON or form encoded.
--
-- 'usernameField', 'requestInspection_usernameField' - The name of the field in the request payload that contains your
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
-- 'passwordField', 'requestInspection_passwordField' - The name of the field in the request payload that contains your
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
newRequestInspection ::
  -- | 'payloadType'
  PayloadType ->
  -- | 'usernameField'
  UsernameField ->
  -- | 'passwordField'
  PasswordField ->
  RequestInspection
newRequestInspection
  pPayloadType_
  pUsernameField_
  pPasswordField_ =
    RequestInspection'
      { payloadType = pPayloadType_,
        usernameField = pUsernameField_,
        passwordField = pPasswordField_
      }

-- | The payload type for your login endpoint, either JSON or form encoded.
requestInspection_payloadType :: Lens.Lens' RequestInspection PayloadType
requestInspection_payloadType = Lens.lens (\RequestInspection' {payloadType} -> payloadType) (\s@RequestInspection' {} a -> s {payloadType = a} :: RequestInspection)

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
requestInspection_usernameField :: Lens.Lens' RequestInspection UsernameField
requestInspection_usernameField = Lens.lens (\RequestInspection' {usernameField} -> usernameField) (\s@RequestInspection' {} a -> s {usernameField = a} :: RequestInspection)

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
requestInspection_passwordField :: Lens.Lens' RequestInspection PasswordField
requestInspection_passwordField = Lens.lens (\RequestInspection' {passwordField} -> passwordField) (\s@RequestInspection' {} a -> s {passwordField = a} :: RequestInspection)

instance Data.FromJSON RequestInspection where
  parseJSON =
    Data.withObject
      "RequestInspection"
      ( \x ->
          RequestInspection'
            Prelude.<$> (x Data..: "PayloadType")
            Prelude.<*> (x Data..: "UsernameField")
            Prelude.<*> (x Data..: "PasswordField")
      )

instance Prelude.Hashable RequestInspection where
  hashWithSalt _salt RequestInspection' {..} =
    _salt
      `Prelude.hashWithSalt` payloadType
      `Prelude.hashWithSalt` usernameField
      `Prelude.hashWithSalt` passwordField

instance Prelude.NFData RequestInspection where
  rnf RequestInspection' {..} =
    Prelude.rnf payloadType
      `Prelude.seq` Prelude.rnf usernameField
      `Prelude.seq` Prelude.rnf passwordField

instance Data.ToJSON RequestInspection where
  toJSON RequestInspection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PayloadType" Data..= payloadType),
            Prelude.Just ("UsernameField" Data..= usernameField),
            Prelude.Just
              ("PasswordField" Data..= passwordField)
          ]
      )
