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
-- Module      : Amazonka.VerifiedPermissions.Types.AttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.AttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.EntityIdentifier

-- | The value of an attribute.
--
-- Contains information about the runtime context for a request for which
-- an authorization decision is made.
--
-- This data type is used as a member of the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ContextDefinition.html ContextDefinition>
-- structure which is uses as a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorized.html IsAuthorized>
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>
-- operations.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | An attribute value of
    -- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-boolean Boolean>
    -- type.
    --
    -- Example: @{\"boolean\": true}@
    boolean :: Prelude.Maybe Prelude.Bool,
    -- | An attribute value of type
    -- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_EntityIdentifier.html EntityIdentifier>.
    --
    -- Example:
    -- @\"entityIdentifier\": { \"entityId\": \"\<id>\", \"entityType\": \"\<entity type>\"}@
    entityIdentifier :: Prelude.Maybe EntityIdentifier,
    -- | An attribute value of
    -- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-long Long>
    -- type.
    --
    -- Example: @{\"long\": 0}@
    long :: Prelude.Maybe Prelude.Integer,
    -- | An attribute value of
    -- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-record Record>
    -- type.
    --
    -- Example: @{\"record\": { \"keyName\": {} } }@
    record :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | An attribute value of
    -- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-set Set> type.
    --
    -- Example: @{\"set\": [ {} ] }@
    set :: Prelude.Maybe [AttributeValue],
    -- | An attribute value of
    -- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-string String>
    -- type.
    --
    -- Example: @{\"string\": \"abc\"}@
    string :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boolean', 'attributeValue_boolean' - An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-boolean Boolean>
-- type.
--
-- Example: @{\"boolean\": true}@
--
-- 'entityIdentifier', 'attributeValue_entityIdentifier' - An attribute value of type
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_EntityIdentifier.html EntityIdentifier>.
--
-- Example:
-- @\"entityIdentifier\": { \"entityId\": \"\<id>\", \"entityType\": \"\<entity type>\"}@
--
-- 'long', 'attributeValue_long' - An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-long Long>
-- type.
--
-- Example: @{\"long\": 0}@
--
-- 'record', 'attributeValue_record' - An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-record Record>
-- type.
--
-- Example: @{\"record\": { \"keyName\": {} } }@
--
-- 'set', 'attributeValue_set' - An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-set Set> type.
--
-- Example: @{\"set\": [ {} ] }@
--
-- 'string', 'attributeValue_string' - An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-string String>
-- type.
--
-- Example: @{\"string\": \"abc\"}@
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue'
    { boolean = Prelude.Nothing,
      entityIdentifier = Prelude.Nothing,
      long = Prelude.Nothing,
      record = Prelude.Nothing,
      set = Prelude.Nothing,
      string = Prelude.Nothing
    }

-- | An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-boolean Boolean>
-- type.
--
-- Example: @{\"boolean\": true}@
attributeValue_boolean :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Bool)
attributeValue_boolean = Lens.lens (\AttributeValue' {boolean} -> boolean) (\s@AttributeValue' {} a -> s {boolean = a} :: AttributeValue)

-- | An attribute value of type
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_EntityIdentifier.html EntityIdentifier>.
--
-- Example:
-- @\"entityIdentifier\": { \"entityId\": \"\<id>\", \"entityType\": \"\<entity type>\"}@
attributeValue_entityIdentifier :: Lens.Lens' AttributeValue (Prelude.Maybe EntityIdentifier)
attributeValue_entityIdentifier = Lens.lens (\AttributeValue' {entityIdentifier} -> entityIdentifier) (\s@AttributeValue' {} a -> s {entityIdentifier = a} :: AttributeValue)

-- | An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-long Long>
-- type.
--
-- Example: @{\"long\": 0}@
attributeValue_long :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Integer)
attributeValue_long = Lens.lens (\AttributeValue' {long} -> long) (\s@AttributeValue' {} a -> s {long = a} :: AttributeValue)

-- | An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-record Record>
-- type.
--
-- Example: @{\"record\": { \"keyName\": {} } }@
attributeValue_record :: Lens.Lens' AttributeValue (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
attributeValue_record = Lens.lens (\AttributeValue' {record} -> record) (\s@AttributeValue' {} a -> s {record = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-set Set> type.
--
-- Example: @{\"set\": [ {} ] }@
attributeValue_set :: Lens.Lens' AttributeValue (Prelude.Maybe [AttributeValue])
attributeValue_set = Lens.lens (\AttributeValue' {set} -> set) (\s@AttributeValue' {} a -> s {set = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | An attribute value of
-- <docs.cedarpolicy.comcedar-syntax-datatypes.html#datatype-string String>
-- type.
--
-- Example: @{\"string\": \"abc\"}@
attributeValue_string :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_string = Lens.lens (\AttributeValue' {string} -> string) (\s@AttributeValue' {} a -> s {string = a} :: AttributeValue)

instance Prelude.Hashable AttributeValue where
  hashWithSalt _salt AttributeValue' {..} =
    _salt
      `Prelude.hashWithSalt` boolean
      `Prelude.hashWithSalt` entityIdentifier
      `Prelude.hashWithSalt` long
      `Prelude.hashWithSalt` record
      `Prelude.hashWithSalt` set
      `Prelude.hashWithSalt` string

instance Prelude.NFData AttributeValue where
  rnf AttributeValue' {..} =
    Prelude.rnf boolean
      `Prelude.seq` Prelude.rnf entityIdentifier
      `Prelude.seq` Prelude.rnf long
      `Prelude.seq` Prelude.rnf record
      `Prelude.seq` Prelude.rnf set
      `Prelude.seq` Prelude.rnf string

instance Data.ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("boolean" Data..=) Prelude.<$> boolean,
            ("entityIdentifier" Data..=)
              Prelude.<$> entityIdentifier,
            ("long" Data..=) Prelude.<$> long,
            ("record" Data..=) Prelude.<$> record,
            ("set" Data..=) Prelude.<$> set,
            ("string" Data..=) Prelude.<$> string
          ]
      )
