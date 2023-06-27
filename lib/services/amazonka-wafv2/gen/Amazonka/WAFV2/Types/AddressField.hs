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
-- Module      : Amazonka.WAFV2.Types.AddressField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AddressField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name of a field in the request payload that contains part or all of
-- your customer\'s primary physical address.
--
-- This data type is used in the @RequestInspectionACFP@ data type.
--
-- /See:/ 'newAddressField' smart constructor.
data AddressField = AddressField'
  { -- | The name of a single primary address field.
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
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddressField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'addressField_identifier' - The name of a single primary address field.
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
newAddressField ::
  -- | 'identifier'
  Prelude.Text ->
  AddressField
newAddressField pIdentifier_ =
  AddressField' {identifier = pIdentifier_}

-- | The name of a single primary address field.
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
addressField_identifier :: Lens.Lens' AddressField Prelude.Text
addressField_identifier = Lens.lens (\AddressField' {identifier} -> identifier) (\s@AddressField' {} a -> s {identifier = a} :: AddressField)

instance Data.FromJSON AddressField where
  parseJSON =
    Data.withObject
      "AddressField"
      ( \x ->
          AddressField' Prelude.<$> (x Data..: "Identifier")
      )

instance Prelude.Hashable AddressField where
  hashWithSalt _salt AddressField' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData AddressField where
  rnf AddressField' {..} = Prelude.rnf identifier

instance Data.ToJSON AddressField where
  toJSON AddressField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )
