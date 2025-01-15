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
-- Module      : Amazonka.DynamoDB.Types.AttributeValueUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.AttributeValueUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeAction
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | For the @UpdateItem@ operation, represents the attributes to be
-- modified, the action to perform on each, and the new value for each.
--
-- You cannot use @UpdateItem@ to update any primary key attributes.
-- Instead, you will need to delete the item, and then use @PutItem@ to
-- create a new item with new attributes.
--
-- Attribute values cannot be null; string and binary type attributes must
-- have lengths greater than zero; and set type attributes must not be
-- empty. Requests with empty values will be rejected with a
-- @ValidationException@ exception.
--
-- /See:/ 'newAttributeValueUpdate' smart constructor.
data AttributeValueUpdate = AttributeValueUpdate'
  { -- | Specifies how to perform the update. Valid values are @PUT@ (default),
    -- @DELETE@, and @ADD@. The behavior depends on whether the specified
    -- primary key already exists in the table.
    --
    -- __If an item with the specified /Key/ is found in the table:__
    --
    -- -   @PUT@ - Adds the specified attribute to the item. If the attribute
    --     already exists, it is replaced by the new value.
    --
    -- -   @DELETE@ - If no value is specified, the attribute and its value are
    --     removed from the item. The data type of the specified value must
    --     match the existing value\'s data type.
    --
    --     If a /set/ of values is specified, then those values are subtracted
    --     from the old set. For example, if the attribute value was the set
    --     @[a,b,c]@ and the @DELETE@ action specified @[a,c]@, then the final
    --     attribute value would be @[b]@. Specifying an empty set is an error.
    --
    -- -   @ADD@ - If the attribute does not already exist, then the attribute
    --     and its values are added to the item. If the attribute does exist,
    --     then the behavior of @ADD@ depends on the data type of the
    --     attribute:
    --
    --     -   If the existing attribute is a number, and if @Value@ is also a
    --         number, then the @Value@ is mathematically added to the existing
    --         attribute. If @Value@ is a negative number, then it is
    --         subtracted from the existing attribute.
    --
    --         If you use @ADD@ to increment or decrement a number value for an
    --         item that doesn\'t exist before the update, DynamoDB uses 0 as
    --         the initial value.
    --
    --         In addition, if you use @ADD@ to update an existing item, and
    --         intend to increment or decrement an attribute value which does
    --         not yet exist, DynamoDB uses @0@ as the initial value. For
    --         example, suppose that the item you want to update does not yet
    --         have an attribute named /itemcount/, but you decide to @ADD@ the
    --         number @3@ to this attribute anyway, even though it currently
    --         does not exist. DynamoDB will create the /itemcount/ attribute,
    --         set its initial value to @0@, and finally add @3@ to it. The
    --         result will be a new /itemcount/ attribute in the item, with a
    --         value of @3@.
    --
    --     -   If the existing data type is a set, and if the @Value@ is also a
    --         set, then the @Value@ is added to the existing set. (This is a
    --         /set/ operation, not mathematical addition.) For example, if the
    --         attribute value was the set @[1,2]@, and the @ADD@ action
    --         specified @[3]@, then the final attribute value would be
    --         @[1,2,3]@. An error occurs if an Add action is specified for a
    --         set attribute and the attribute type specified does not match
    --         the existing set type.
    --
    --         Both sets must have the same primitive data type. For example,
    --         if the existing data type is a set of strings, the @Value@ must
    --         also be a set of strings. The same holds true for number sets
    --         and binary sets.
    --
    --     This action is only valid for an existing attribute whose data type
    --     is number or is a set. Do not use @ADD@ for any other data types.
    --
    -- __If no item with the specified /Key/ is found:__
    --
    -- -   @PUT@ - DynamoDB creates a new item with the specified primary key,
    --     and then adds the attribute.
    --
    -- -   @DELETE@ - Nothing happens; there is no attribute to delete.
    --
    -- -   @ADD@ - DynamoDB creates a new item with the supplied primary key
    --     and number (or set) for the attribute value. The only data types
    --     allowed are number, number set, string set or binary set.
    action :: Prelude.Maybe AttributeAction,
    -- | Represents the data for an attribute.
    --
    -- Each attribute value is described as a name-value pair. The name is the
    -- data type, and the value is the data itself.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types>
    -- in the /Amazon DynamoDB Developer Guide/.
    value :: Prelude.Maybe AttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeValueUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'attributeValueUpdate_action' - Specifies how to perform the update. Valid values are @PUT@ (default),
-- @DELETE@, and @ADD@. The behavior depends on whether the specified
-- primary key already exists in the table.
--
-- __If an item with the specified /Key/ is found in the table:__
--
-- -   @PUT@ - Adds the specified attribute to the item. If the attribute
--     already exists, it is replaced by the new value.
--
-- -   @DELETE@ - If no value is specified, the attribute and its value are
--     removed from the item. The data type of the specified value must
--     match the existing value\'s data type.
--
--     If a /set/ of values is specified, then those values are subtracted
--     from the old set. For example, if the attribute value was the set
--     @[a,b,c]@ and the @DELETE@ action specified @[a,c]@, then the final
--     attribute value would be @[b]@. Specifying an empty set is an error.
--
-- -   @ADD@ - If the attribute does not already exist, then the attribute
--     and its values are added to the item. If the attribute does exist,
--     then the behavior of @ADD@ depends on the data type of the
--     attribute:
--
--     -   If the existing attribute is a number, and if @Value@ is also a
--         number, then the @Value@ is mathematically added to the existing
--         attribute. If @Value@ is a negative number, then it is
--         subtracted from the existing attribute.
--
--         If you use @ADD@ to increment or decrement a number value for an
--         item that doesn\'t exist before the update, DynamoDB uses 0 as
--         the initial value.
--
--         In addition, if you use @ADD@ to update an existing item, and
--         intend to increment or decrement an attribute value which does
--         not yet exist, DynamoDB uses @0@ as the initial value. For
--         example, suppose that the item you want to update does not yet
--         have an attribute named /itemcount/, but you decide to @ADD@ the
--         number @3@ to this attribute anyway, even though it currently
--         does not exist. DynamoDB will create the /itemcount/ attribute,
--         set its initial value to @0@, and finally add @3@ to it. The
--         result will be a new /itemcount/ attribute in the item, with a
--         value of @3@.
--
--     -   If the existing data type is a set, and if the @Value@ is also a
--         set, then the @Value@ is added to the existing set. (This is a
--         /set/ operation, not mathematical addition.) For example, if the
--         attribute value was the set @[1,2]@, and the @ADD@ action
--         specified @[3]@, then the final attribute value would be
--         @[1,2,3]@. An error occurs if an Add action is specified for a
--         set attribute and the attribute type specified does not match
--         the existing set type.
--
--         Both sets must have the same primitive data type. For example,
--         if the existing data type is a set of strings, the @Value@ must
--         also be a set of strings. The same holds true for number sets
--         and binary sets.
--
--     This action is only valid for an existing attribute whose data type
--     is number or is a set. Do not use @ADD@ for any other data types.
--
-- __If no item with the specified /Key/ is found:__
--
-- -   @PUT@ - DynamoDB creates a new item with the specified primary key,
--     and then adds the attribute.
--
-- -   @DELETE@ - Nothing happens; there is no attribute to delete.
--
-- -   @ADD@ - DynamoDB creates a new item with the supplied primary key
--     and number (or set) for the attribute value. The only data types
--     allowed are number, number set, string set or binary set.
--
-- 'value', 'attributeValueUpdate_value' - Represents the data for an attribute.
--
-- Each attribute value is described as a name-value pair. The name is the
-- data type, and the value is the data itself.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types>
-- in the /Amazon DynamoDB Developer Guide/.
newAttributeValueUpdate ::
  AttributeValueUpdate
newAttributeValueUpdate =
  AttributeValueUpdate'
    { action = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specifies how to perform the update. Valid values are @PUT@ (default),
-- @DELETE@, and @ADD@. The behavior depends on whether the specified
-- primary key already exists in the table.
--
-- __If an item with the specified /Key/ is found in the table:__
--
-- -   @PUT@ - Adds the specified attribute to the item. If the attribute
--     already exists, it is replaced by the new value.
--
-- -   @DELETE@ - If no value is specified, the attribute and its value are
--     removed from the item. The data type of the specified value must
--     match the existing value\'s data type.
--
--     If a /set/ of values is specified, then those values are subtracted
--     from the old set. For example, if the attribute value was the set
--     @[a,b,c]@ and the @DELETE@ action specified @[a,c]@, then the final
--     attribute value would be @[b]@. Specifying an empty set is an error.
--
-- -   @ADD@ - If the attribute does not already exist, then the attribute
--     and its values are added to the item. If the attribute does exist,
--     then the behavior of @ADD@ depends on the data type of the
--     attribute:
--
--     -   If the existing attribute is a number, and if @Value@ is also a
--         number, then the @Value@ is mathematically added to the existing
--         attribute. If @Value@ is a negative number, then it is
--         subtracted from the existing attribute.
--
--         If you use @ADD@ to increment or decrement a number value for an
--         item that doesn\'t exist before the update, DynamoDB uses 0 as
--         the initial value.
--
--         In addition, if you use @ADD@ to update an existing item, and
--         intend to increment or decrement an attribute value which does
--         not yet exist, DynamoDB uses @0@ as the initial value. For
--         example, suppose that the item you want to update does not yet
--         have an attribute named /itemcount/, but you decide to @ADD@ the
--         number @3@ to this attribute anyway, even though it currently
--         does not exist. DynamoDB will create the /itemcount/ attribute,
--         set its initial value to @0@, and finally add @3@ to it. The
--         result will be a new /itemcount/ attribute in the item, with a
--         value of @3@.
--
--     -   If the existing data type is a set, and if the @Value@ is also a
--         set, then the @Value@ is added to the existing set. (This is a
--         /set/ operation, not mathematical addition.) For example, if the
--         attribute value was the set @[1,2]@, and the @ADD@ action
--         specified @[3]@, then the final attribute value would be
--         @[1,2,3]@. An error occurs if an Add action is specified for a
--         set attribute and the attribute type specified does not match
--         the existing set type.
--
--         Both sets must have the same primitive data type. For example,
--         if the existing data type is a set of strings, the @Value@ must
--         also be a set of strings. The same holds true for number sets
--         and binary sets.
--
--     This action is only valid for an existing attribute whose data type
--     is number or is a set. Do not use @ADD@ for any other data types.
--
-- __If no item with the specified /Key/ is found:__
--
-- -   @PUT@ - DynamoDB creates a new item with the specified primary key,
--     and then adds the attribute.
--
-- -   @DELETE@ - Nothing happens; there is no attribute to delete.
--
-- -   @ADD@ - DynamoDB creates a new item with the supplied primary key
--     and number (or set) for the attribute value. The only data types
--     allowed are number, number set, string set or binary set.
attributeValueUpdate_action :: Lens.Lens' AttributeValueUpdate (Prelude.Maybe AttributeAction)
attributeValueUpdate_action = Lens.lens (\AttributeValueUpdate' {action} -> action) (\s@AttributeValueUpdate' {} a -> s {action = a} :: AttributeValueUpdate)

-- | Represents the data for an attribute.
--
-- Each attribute value is described as a name-value pair. The name is the
-- data type, and the value is the data itself.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types>
-- in the /Amazon DynamoDB Developer Guide/.
attributeValueUpdate_value :: Lens.Lens' AttributeValueUpdate (Prelude.Maybe AttributeValue)
attributeValueUpdate_value = Lens.lens (\AttributeValueUpdate' {value} -> value) (\s@AttributeValueUpdate' {} a -> s {value = a} :: AttributeValueUpdate)

instance Prelude.Hashable AttributeValueUpdate where
  hashWithSalt _salt AttributeValueUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` value

instance Prelude.NFData AttributeValueUpdate where
  rnf AttributeValueUpdate' {..} =
    Prelude.rnf action `Prelude.seq` Prelude.rnf value

instance Data.ToJSON AttributeValueUpdate where
  toJSON AttributeValueUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
