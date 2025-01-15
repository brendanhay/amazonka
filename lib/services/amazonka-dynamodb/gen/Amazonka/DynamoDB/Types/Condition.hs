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
-- Module      : Amazonka.DynamoDB.Types.Condition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ComparisonOperator
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the selection criteria for a @Query@ or @Scan@ operation:
--
-- -   For a @Query@ operation, @Condition@ is used for specifying the
--     @KeyConditions@ to use when querying a table or an index. For
--     @KeyConditions@, only the following comparison operators are
--     supported:
--
--     @EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN@
--
--     @Condition@ is also used in a @QueryFilter@, which evaluates the
--     query results and returns only the desired values.
--
-- -   For a @Scan@ operation, @Condition@ is used in a @ScanFilter@, which
--     evaluates the scan results and returns only the desired values.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | One or more values to evaluate against the supplied attribute. The
    -- number of values in the list depends on the @ComparisonOperator@ being
    -- used.
    --
    -- For type Number, value comparisons are numeric.
    --
    -- String value comparisons for greater than, equals, or less than are
    -- based on ASCII character code values. For example, @a@ is greater than
    -- @A@, and @a@ is greater than @B@. For a list of code values, see
    -- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
    --
    -- For Binary, DynamoDB treats each byte of the binary data as unsigned
    -- when it compares binary values.
    attributeValueList :: Prelude.Maybe [AttributeValue],
    -- | A comparator for evaluating attributes. For example, equals, greater
    -- than, less than, etc.
    --
    -- The following comparison operators are available:
    --
    -- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
    --
    -- The following are descriptions of each comparison operator.
    --
    -- -   @EQ@ : Equal. @EQ@ is supported for all data types, including lists
    --     and maps.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ element
    --     of type String, Number, Binary, String Set, Number Set, or Binary
    --     Set. If an item contains an @AttributeValue@ element of a different
    --     type than the one provided in the request, the value does not match.
    --     For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
    --     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
    --
    -- -   @NE@ : Not equal. @NE@ is supported for all data types, including
    --     lists and maps.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ of type
    --     String, Number, Binary, String Set, Number Set, or Binary Set. If an
    --     item contains an @AttributeValue@ of a different type than the one
    --     provided in the request, the value does not match. For example,
    --     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
    --     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
    --
    -- -   @LE@ : Less than or equal.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ element
    --     of type String, Number, or Binary (not a set type). If an item
    --     contains an @AttributeValue@ element of a different type than the
    --     one provided in the request, the value does not match. For example,
    --     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
    --     @{\"N\":\"6\"}@ does not compare to
    --     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
    --
    -- -   @LT@ : Less than.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ of type
    --     String, Number, or Binary (not a set type). If an item contains an
    --     @AttributeValue@ element of a different type than the one provided
    --     in the request, the value does not match. For example,
    --     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
    --     @{\"N\":\"6\"}@ does not compare to
    --     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
    --
    -- -   @GE@ : Greater than or equal.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ element
    --     of type String, Number, or Binary (not a set type). If an item
    --     contains an @AttributeValue@ element of a different type than the
    --     one provided in the request, the value does not match. For example,
    --     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
    --     @{\"N\":\"6\"}@ does not compare to
    --     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
    --
    -- -   @GT@ : Greater than.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ element
    --     of type String, Number, or Binary (not a set type). If an item
    --     contains an @AttributeValue@ element of a different type than the
    --     one provided in the request, the value does not match. For example,
    --     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
    --     @{\"N\":\"6\"}@ does not compare to
    --     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
    --
    -- -   @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all
    --     data types, including lists and maps.
    --
    --     This operator tests for the existence of an attribute, not its data
    --     type. If the data type of attribute \"@a@\" is null, and you
    --     evaluate it using @NOT_NULL@, the result is a Boolean @true@. This
    --     result is because the attribute \"@a@\" exists; its data type is not
    --     relevant to the @NOT_NULL@ comparison operator.
    --
    -- -   @NULL@ : The attribute does not exist. @NULL@ is supported for all
    --     data types, including lists and maps.
    --
    --     This operator tests for the nonexistence of an attribute, not its
    --     data type. If the data type of attribute \"@a@\" is null, and you
    --     evaluate it using @NULL@, the result is a Boolean @false@. This is
    --     because the attribute \"@a@\" exists; its data type is not relevant
    --     to the @NULL@ comparison operator.
    --
    -- -   @CONTAINS@ : Checks for a subsequence, or value in a set.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ element
    --     of type String, Number, or Binary (not a set type). If the target
    --     attribute of the comparison is of type String, then the operator
    --     checks for a substring match. If the target attribute of the
    --     comparison is of type Binary, then the operator looks for a
    --     subsequence of the target that matches the input. If the target
    --     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
    --     \"@BS@\"), then the operator evaluates to true if it finds an exact
    --     match with any member of the set.
    --
    --     CONTAINS is supported for lists: When evaluating \"@a CONTAINS b@\",
    --     \"@a@\" can be a list; however, \"@b@\" cannot be a set, a map, or a
    --     list.
    --
    -- -   @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of
    --     a value in a set.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ element
    --     of type String, Number, or Binary (not a set type). If the target
    --     attribute of the comparison is a String, then the operator checks
    --     for the absence of a substring match. If the target attribute of the
    --     comparison is Binary, then the operator checks for the absence of a
    --     subsequence of the target that matches the input. If the target
    --     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
    --     \"@BS@\"), then the operator evaluates to true if it /does not/ find
    --     an exact match with any member of the set.
    --
    --     NOT_CONTAINS is supported for lists: When evaluating
    --     \"@a NOT CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
    --     cannot be a set, a map, or a list.
    --
    -- -   @BEGINS_WITH@ : Checks for a prefix.
    --
    --     @AttributeValueList@ can contain only one @AttributeValue@ of type
    --     String or Binary (not a Number or a set type). The target attribute
    --     of the comparison must be of type String or Binary (not a Number or
    --     a set type).
    --
    -- -   @IN@ : Checks for matching elements in a list.
    --
    --     @AttributeValueList@ can contain one or more @AttributeValue@
    --     elements of type String, Number, or Binary. These attributes are
    --     compared against an existing attribute of an item. If any elements
    --     of the input are equal to the item attribute, the expression
    --     evaluates to true.
    --
    -- -   @BETWEEN@ : Greater than or equal to the first value, and less than
    --     or equal to the second value.
    --
    --     @AttributeValueList@ must contain two @AttributeValue@ elements of
    --     the same type, either String, Number, or Binary (not a set type). A
    --     target attribute matches if the target value is greater than, or
    --     equal to, the first element and less than, or equal to, the second
    --     element. If an item contains an @AttributeValue@ element of a
    --     different type than the one provided in the request, the value does
    --     not match. For example, @{\"S\":\"6\"}@ does not compare to
    --     @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does not compare to
    --     @{\"NS\":[\"6\", \"2\", \"1\"]}@
    --
    -- For usage examples of @AttributeValueList@ and @ComparisonOperator@, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
    -- in the /Amazon DynamoDB Developer Guide/.
    comparisonOperator :: ComparisonOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValueList', 'condition_attributeValueList' - One or more values to evaluate against the supplied attribute. The
-- number of values in the list depends on the @ComparisonOperator@ being
-- used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are
-- based on ASCII character code values. For example, @a@ is greater than
-- @A@, and @a@ is greater than @B@. For a list of code values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For Binary, DynamoDB treats each byte of the binary data as unsigned
-- when it compares binary values.
--
-- 'comparisonOperator', 'condition_comparisonOperator' - A comparator for evaluating attributes. For example, equals, greater
-- than, less than, etc.
--
-- The following comparison operators are available:
--
-- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
--
-- The following are descriptions of each comparison operator.
--
-- -   @EQ@ : Equal. @EQ@ is supported for all data types, including lists
--     and maps.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, Binary, String Set, Number Set, or Binary
--     Set. If an item contains an @AttributeValue@ element of a different
--     type than the one provided in the request, the value does not match.
--     For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NE@ : Not equal. @NE@ is supported for all data types, including
--     lists and maps.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ of type
--     String, Number, Binary, String Set, Number Set, or Binary Set. If an
--     item contains an @AttributeValue@ of a different type than the one
--     provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LE@ : Less than or equal.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an @AttributeValue@ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LT@ : Less than.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ of type
--     String, Number, or Binary (not a set type). If an item contains an
--     @AttributeValue@ element of a different type than the one provided
--     in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GE@ : Greater than or equal.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an @AttributeValue@ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GT@ : Greater than.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an @AttributeValue@ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all
--     data types, including lists and maps.
--
--     This operator tests for the existence of an attribute, not its data
--     type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NOT_NULL@, the result is a Boolean @true@. This
--     result is because the attribute \"@a@\" exists; its data type is not
--     relevant to the @NOT_NULL@ comparison operator.
--
-- -   @NULL@ : The attribute does not exist. @NULL@ is supported for all
--     data types, including lists and maps.
--
--     This operator tests for the nonexistence of an attribute, not its
--     data type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NULL@, the result is a Boolean @false@. This is
--     because the attribute \"@a@\" exists; its data type is not relevant
--     to the @NULL@ comparison operator.
--
-- -   @CONTAINS@ : Checks for a subsequence, or value in a set.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is of type String, then the operator
--     checks for a substring match. If the target attribute of the
--     comparison is of type Binary, then the operator looks for a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it finds an exact
--     match with any member of the set.
--
--     CONTAINS is supported for lists: When evaluating \"@a CONTAINS b@\",
--     \"@a@\" can be a list; however, \"@b@\" cannot be a set, a map, or a
--     list.
--
-- -   @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of
--     a value in a set.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is a String, then the operator checks
--     for the absence of a substring match. If the target attribute of the
--     comparison is Binary, then the operator checks for the absence of a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it /does not/ find
--     an exact match with any member of the set.
--
--     NOT_CONTAINS is supported for lists: When evaluating
--     \"@a NOT CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
--     cannot be a set, a map, or a list.
--
-- -   @BEGINS_WITH@ : Checks for a prefix.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ of type
--     String or Binary (not a Number or a set type). The target attribute
--     of the comparison must be of type String or Binary (not a Number or
--     a set type).
--
-- -   @IN@ : Checks for matching elements in a list.
--
--     @AttributeValueList@ can contain one or more @AttributeValue@
--     elements of type String, Number, or Binary. These attributes are
--     compared against an existing attribute of an item. If any elements
--     of the input are equal to the item attribute, the expression
--     evaluates to true.
--
-- -   @BETWEEN@ : Greater than or equal to the first value, and less than
--     or equal to the second value.
--
--     @AttributeValueList@ must contain two @AttributeValue@ elements of
--     the same type, either String, Number, or Binary (not a set type). A
--     target attribute matches if the target value is greater than, or
--     equal to, the first element and less than, or equal to, the second
--     element. If an item contains an @AttributeValue@ element of a
--     different type than the one provided in the request, the value does
--     not match. For example, @{\"S\":\"6\"}@ does not compare to
--     @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@
--
-- For usage examples of @AttributeValueList@ and @ComparisonOperator@, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
newCondition ::
  -- | 'comparisonOperator'
  ComparisonOperator ->
  Condition
newCondition pComparisonOperator_ =
  Condition'
    { attributeValueList = Prelude.Nothing,
      comparisonOperator = pComparisonOperator_
    }

-- | One or more values to evaluate against the supplied attribute. The
-- number of values in the list depends on the @ComparisonOperator@ being
-- used.
--
-- For type Number, value comparisons are numeric.
--
-- String value comparisons for greater than, equals, or less than are
-- based on ASCII character code values. For example, @a@ is greater than
-- @A@, and @a@ is greater than @B@. For a list of code values, see
-- <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters>.
--
-- For Binary, DynamoDB treats each byte of the binary data as unsigned
-- when it compares binary values.
condition_attributeValueList :: Lens.Lens' Condition (Prelude.Maybe [AttributeValue])
condition_attributeValueList = Lens.lens (\Condition' {attributeValueList} -> attributeValueList) (\s@Condition' {} a -> s {attributeValueList = a} :: Condition) Prelude.. Lens.mapping Lens.coerced

-- | A comparator for evaluating attributes. For example, equals, greater
-- than, less than, etc.
--
-- The following comparison operators are available:
--
-- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
--
-- The following are descriptions of each comparison operator.
--
-- -   @EQ@ : Equal. @EQ@ is supported for all data types, including lists
--     and maps.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, Binary, String Set, Number Set, or Binary
--     Set. If an item contains an @AttributeValue@ element of a different
--     type than the one provided in the request, the value does not match.
--     For example, @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NE@ : Not equal. @NE@ is supported for all data types, including
--     lists and maps.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ of type
--     String, Number, Binary, String Set, Number Set, or Binary Set. If an
--     item contains an @AttributeValue@ of a different type than the one
--     provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not equal @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LE@ : Less than or equal.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an @AttributeValue@ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @LT@ : Less than.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ of type
--     String, Number, or Binary (not a set type). If an item contains an
--     @AttributeValue@ element of a different type than the one provided
--     in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GE@ : Greater than or equal.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an @AttributeValue@ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @GT@ : Greater than.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If an item
--     contains an @AttributeValue@ element of a different type than the
--     one provided in the request, the value does not match. For example,
--     @{\"S\":\"6\"}@ does not equal @{\"N\":\"6\"}@. Also,
--     @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@.
--
-- -   @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all
--     data types, including lists and maps.
--
--     This operator tests for the existence of an attribute, not its data
--     type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NOT_NULL@, the result is a Boolean @true@. This
--     result is because the attribute \"@a@\" exists; its data type is not
--     relevant to the @NOT_NULL@ comparison operator.
--
-- -   @NULL@ : The attribute does not exist. @NULL@ is supported for all
--     data types, including lists and maps.
--
--     This operator tests for the nonexistence of an attribute, not its
--     data type. If the data type of attribute \"@a@\" is null, and you
--     evaluate it using @NULL@, the result is a Boolean @false@. This is
--     because the attribute \"@a@\" exists; its data type is not relevant
--     to the @NULL@ comparison operator.
--
-- -   @CONTAINS@ : Checks for a subsequence, or value in a set.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is of type String, then the operator
--     checks for a substring match. If the target attribute of the
--     comparison is of type Binary, then the operator looks for a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it finds an exact
--     match with any member of the set.
--
--     CONTAINS is supported for lists: When evaluating \"@a CONTAINS b@\",
--     \"@a@\" can be a list; however, \"@b@\" cannot be a set, a map, or a
--     list.
--
-- -   @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of
--     a value in a set.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ element
--     of type String, Number, or Binary (not a set type). If the target
--     attribute of the comparison is a String, then the operator checks
--     for the absence of a substring match. If the target attribute of the
--     comparison is Binary, then the operator checks for the absence of a
--     subsequence of the target that matches the input. If the target
--     attribute of the comparison is a set (\"@SS@\", \"@NS@\", or
--     \"@BS@\"), then the operator evaluates to true if it /does not/ find
--     an exact match with any member of the set.
--
--     NOT_CONTAINS is supported for lists: When evaluating
--     \"@a NOT CONTAINS b@\", \"@a@\" can be a list; however, \"@b@\"
--     cannot be a set, a map, or a list.
--
-- -   @BEGINS_WITH@ : Checks for a prefix.
--
--     @AttributeValueList@ can contain only one @AttributeValue@ of type
--     String or Binary (not a Number or a set type). The target attribute
--     of the comparison must be of type String or Binary (not a Number or
--     a set type).
--
-- -   @IN@ : Checks for matching elements in a list.
--
--     @AttributeValueList@ can contain one or more @AttributeValue@
--     elements of type String, Number, or Binary. These attributes are
--     compared against an existing attribute of an item. If any elements
--     of the input are equal to the item attribute, the expression
--     evaluates to true.
--
-- -   @BETWEEN@ : Greater than or equal to the first value, and less than
--     or equal to the second value.
--
--     @AttributeValueList@ must contain two @AttributeValue@ elements of
--     the same type, either String, Number, or Binary (not a set type). A
--     target attribute matches if the target value is greater than, or
--     equal to, the first element and less than, or equal to, the second
--     element. If an item contains an @AttributeValue@ element of a
--     different type than the one provided in the request, the value does
--     not match. For example, @{\"S\":\"6\"}@ does not compare to
--     @{\"N\":\"6\"}@. Also, @{\"N\":\"6\"}@ does not compare to
--     @{\"NS\":[\"6\", \"2\", \"1\"]}@
--
-- For usage examples of @AttributeValueList@ and @ComparisonOperator@, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters>
-- in the /Amazon DynamoDB Developer Guide/.
condition_comparisonOperator :: Lens.Lens' Condition ComparisonOperator
condition_comparisonOperator = Lens.lens (\Condition' {comparisonOperator} -> comparisonOperator) (\s@Condition' {} a -> s {comparisonOperator = a} :: Condition)

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt
      `Prelude.hashWithSalt` attributeValueList
      `Prelude.hashWithSalt` comparisonOperator

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf attributeValueList `Prelude.seq`
      Prelude.rnf comparisonOperator

instance Data.ToJSON Condition where
  toJSON Condition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeValueList" Data..=)
              Prelude.<$> attributeValueList,
            Prelude.Just
              ("ComparisonOperator" Data..= comparisonOperator)
          ]
      )
