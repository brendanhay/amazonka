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
-- Module      : Amazonka.WAFV2.Types.EmailField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.EmailField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of the field in the request payload that contains your
-- customer\'s email.
--
-- This data type is used in the @RequestInspectionACFP@ data type.
--
-- /See:/ 'newEmailField' smart constructor.
data EmailField = EmailField'
  { -- | The name of the email field.
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
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'emailField_identifier' - The name of the email field.
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
newEmailField ::
  -- | 'identifier'
  Prelude.Text ->
  EmailField
newEmailField pIdentifier_ =
  EmailField' {identifier = pIdentifier_}

-- | The name of the email field.
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
emailField_identifier :: Lens.Lens' EmailField Prelude.Text
emailField_identifier = Lens.lens (\EmailField' {identifier} -> identifier) (\s@EmailField' {} a -> s {identifier = a} :: EmailField)

instance Data.FromJSON EmailField where
  parseJSON =
    Data.withObject
      "EmailField"
      ( \x ->
          EmailField' Prelude.<$> (x Data..: "Identifier")
      )

instance Prelude.Hashable EmailField where
  hashWithSalt _salt EmailField' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData EmailField where
  rnf EmailField' {..} = Prelude.rnf identifier

instance Data.ToJSON EmailField where
  toJSON EmailField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )
