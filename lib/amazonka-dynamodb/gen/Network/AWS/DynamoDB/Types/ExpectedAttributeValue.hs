-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExpectedAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExpectedAttributeValue
  ( ExpectedAttributeValue (..),

    -- * Smart constructor
    mkExpectedAttributeValue,

    -- * Lenses
    eavAttributeValueList,
    eavExists,
    eavValue,
    eavComparisonOperator,
  )
where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ComparisonOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a condition to be compared with an attribute value. This condition can be used with @DeleteItem@ , @PutItem@ , or @UpdateItem@ operations; if the comparison evaluates to true, the operation succeeds; if not, the operation fails. You can use @ExpectedAttributeValue@ in one of two different ways:
--
--
--     * Use @AttributeValueList@ to specify one or more values to compare against an attribute. Use @ComparisonOperator@ to specify how you want to perform the comparison. If the comparison evaluates to true, then the conditional operation succeeds.
--
--
--     * Use @Value@ to specify a value that DynamoDB will compare against an attribute. If the values match, then @ExpectedAttributeValue@ evaluates to true and the conditional operation succeeds. Optionally, you can also set @Exists@ to false, indicating that you /do not/ expect to find the attribute value in the table. In this case, the conditional operation succeeds only if the comparison evaluates to false.
--
--
-- @Value@ and @Exists@ are incompatible with @AttributeValueList@ and @ComparisonOperator@ . Note that if you use both sets of parameters at once, DynamoDB will return a @ValidationException@ exception.
--
-- /See:/ 'mkExpectedAttributeValue' smart constructor.
data ExpectedAttributeValue = ExpectedAttributeValue'
  { attributeValueList ::
      Lude.Maybe [AttributeValue],
    exists :: Lude.Maybe Lude.Bool,
    value :: Lude.Maybe AttributeValue,
    comparisonOperator ::
      Lude.Maybe ComparisonOperator
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExpectedAttributeValue' with the minimum fields required to make a request.
--
-- * 'attributeValueList' - One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used.
--
-- For type Number, value comparisons are numeric.
-- String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> .
-- For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
-- For information on specifying data types in JSON, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format> in the /Amazon DynamoDB Developer Guide/ .
-- * 'comparisonOperator' - A comparator for evaluating attributes in the @AttributeValueList@ . For example, equals, greater than, less than, etc.
--
-- The following comparison operators are available:
-- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
-- The following are descriptions of each comparison operator.
--
--     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps.
-- @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @LE@ : Less than or equal.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @LT@ : Less than.
-- @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @GE@ : Greater than or equal.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @GT@ : Greater than.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.
--
--
--     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.
--
--
--     * @CONTAINS@ : Checks for a subsequence, or value in a set.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set.
-- CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.
--
--
--     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set.
-- NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.
--
--
--     * @BEGINS_WITH@ : Checks for a prefix.
-- @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).
--
--
--
--     * @IN@ : Checks for matching elements in a list.
-- @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.
--
--
--     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.
-- @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@
--
--
-- * 'exists' - Causes DynamoDB to evaluate the value before attempting a conditional operation:
--
--
--     * If @Exists@ is @true@ , DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the operation succeeds. If it is not found, the operation fails with a @ConditionCheckFailedException@ .
--
--
--     * If @Exists@ is @false@ , DynamoDB assumes that the attribute value does not exist in the table. If in fact the value does not exist, then the assumption is valid and the operation succeeds. If the value is found, despite the assumption that it does not exist, the operation fails with a @ConditionCheckFailedException@ .
--
--
-- The default setting for @Exists@ is @true@ . If you supply a @Value@ all by itself, DynamoDB assumes the attribute exists: You don't have to set @Exists@ to @true@ , because it is implied.
-- DynamoDB returns a @ValidationException@ if:
--
--     * @Exists@ is @true@ but there is no @Value@ to check. (You expect a value to exist, but don't specify what that value is.)
--
--
--     * @Exists@ is @false@ but you also provide a @Value@ . (You cannot expect an attribute to have a value, while also expecting it not to exist.)
--
--
-- * 'value' - Represents the data for the expected attribute.
--
-- Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
mkExpectedAttributeValue ::
  ExpectedAttributeValue
mkExpectedAttributeValue =
  ExpectedAttributeValue'
    { attributeValueList = Lude.Nothing,
      exists = Lude.Nothing,
      value = Lude.Nothing,
      comparisonOperator = Lude.Nothing
    }

-- | One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used.
--
-- For type Number, value comparisons are numeric.
-- String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> .
-- For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
-- For information on specifying data types in JSON, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataFormat.html JSON Data Format> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'attributeValueList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eavAttributeValueList :: Lens.Lens' ExpectedAttributeValue (Lude.Maybe [AttributeValue])
eavAttributeValueList = Lens.lens (attributeValueList :: ExpectedAttributeValue -> Lude.Maybe [AttributeValue]) (\s a -> s {attributeValueList = a} :: ExpectedAttributeValue)
{-# DEPRECATED eavAttributeValueList "Use generic-lens or generic-optics with 'attributeValueList' instead." #-}

-- | Causes DynamoDB to evaluate the value before attempting a conditional operation:
--
--
--     * If @Exists@ is @true@ , DynamoDB will check to see if that attribute value already exists in the table. If it is found, then the operation succeeds. If it is not found, the operation fails with a @ConditionCheckFailedException@ .
--
--
--     * If @Exists@ is @false@ , DynamoDB assumes that the attribute value does not exist in the table. If in fact the value does not exist, then the assumption is valid and the operation succeeds. If the value is found, despite the assumption that it does not exist, the operation fails with a @ConditionCheckFailedException@ .
--
--
-- The default setting for @Exists@ is @true@ . If you supply a @Value@ all by itself, DynamoDB assumes the attribute exists: You don't have to set @Exists@ to @true@ , because it is implied.
-- DynamoDB returns a @ValidationException@ if:
--
--     * @Exists@ is @true@ but there is no @Value@ to check. (You expect a value to exist, but don't specify what that value is.)
--
--
--     * @Exists@ is @false@ but you also provide a @Value@ . (You cannot expect an attribute to have a value, while also expecting it not to exist.)
--
--
--
-- /Note:/ Consider using 'exists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eavExists :: Lens.Lens' ExpectedAttributeValue (Lude.Maybe Lude.Bool)
eavExists = Lens.lens (exists :: ExpectedAttributeValue -> Lude.Maybe Lude.Bool) (\s a -> s {exists = a} :: ExpectedAttributeValue)
{-# DEPRECATED eavExists "Use generic-lens or generic-optics with 'exists' instead." #-}

-- | Represents the data for the expected attribute.
--
-- Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eavValue :: Lens.Lens' ExpectedAttributeValue (Lude.Maybe AttributeValue)
eavValue = Lens.lens (value :: ExpectedAttributeValue -> Lude.Maybe AttributeValue) (\s a -> s {value = a} :: ExpectedAttributeValue)
{-# DEPRECATED eavValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A comparator for evaluating attributes in the @AttributeValueList@ . For example, equals, greater than, less than, etc.
--
-- The following comparison operators are available:
-- @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@
-- The following are descriptions of each comparison operator.
--
--     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps.
-- @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @LE@ : Less than or equal.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @LT@ : Less than.
-- @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @GE@ : Greater than or equal.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @GT@ : Greater than.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .
--
--
--
--     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.
--
--
--     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.
--
--
--     * @CONTAINS@ : Checks for a subsequence, or value in a set.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set.
-- CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.
--
--
--     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set.
-- @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set.
-- NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.
--
--
--     * @BEGINS_WITH@ : Checks for a prefix.
-- @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).
--
--
--
--     * @IN@ : Checks for matching elements in a list.
-- @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.
--
--
--     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.
-- @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@
--
--
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eavComparisonOperator :: Lens.Lens' ExpectedAttributeValue (Lude.Maybe ComparisonOperator)
eavComparisonOperator = Lens.lens (comparisonOperator :: ExpectedAttributeValue -> Lude.Maybe ComparisonOperator) (\s a -> s {comparisonOperator = a} :: ExpectedAttributeValue)
{-# DEPRECATED eavComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

instance Lude.ToJSON ExpectedAttributeValue where
  toJSON ExpectedAttributeValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeValueList" Lude..=) Lude.<$> attributeValueList,
            ("Exists" Lude..=) Lude.<$> exists,
            ("Value" Lude..=) Lude.<$> value,
            ("ComparisonOperator" Lude..=) Lude.<$> comparisonOperator
          ]
      )
