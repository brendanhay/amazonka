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
-- Module      : Amazonka.WAFV2.Types.UsernameField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.UsernameField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of the field in the request payload that contains your
-- customer\'s username.
--
-- This data type is used in the @RequestInspection@ and
-- @RequestInspectionACFP@ data types.
--
-- /See:/ 'newUsernameField' smart constructor.
data UsernameField = UsernameField'
  { -- | The name of the username field.
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
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsernameField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'usernameField_identifier' - The name of the username field.
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
newUsernameField ::
  -- | 'identifier'
  Prelude.Text ->
  UsernameField
newUsernameField pIdentifier_ =
  UsernameField' {identifier = pIdentifier_}

-- | The name of the username field.
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
usernameField_identifier :: Lens.Lens' UsernameField Prelude.Text
usernameField_identifier = Lens.lens (\UsernameField' {identifier} -> identifier) (\s@UsernameField' {} a -> s {identifier = a} :: UsernameField)

instance Data.FromJSON UsernameField where
  parseJSON =
    Data.withObject
      "UsernameField"
      ( \x ->
          UsernameField' Prelude.<$> (x Data..: "Identifier")
      )

instance Prelude.Hashable UsernameField where
  hashWithSalt _salt UsernameField' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData UsernameField where
  rnf UsernameField' {..} = Prelude.rnf identifier

instance Data.ToJSON UsernameField where
  toJSON UsernameField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )
