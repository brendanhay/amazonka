{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Condition where

import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.ComparisonOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the selection criteria for a @Query@ or @Scan@ operation:
--
--
--     * For a @Query@ operation, @Condition@ is used for specifying the @KeyConditions@ to use when querying a table or an index. For @KeyConditions@ , only the following comparison operators are supported:
--
-- @EQ | LE | LT | GE | GT | BEGINS_WITH | BETWEEN@
--
-- @Condition@ is also used in a @QueryFilter@ , which evaluates the query results and returns only the desired values.
--
--     * For a @Scan@ operation, @Condition@ is used in a @ScanFilter@ , which evaluates the scan results and returns only the desired values.
--
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cAttributeValueList ::
      !(Maybe [AttributeValue]),
    _cComparisonOperator :: !ComparisonOperator
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAttributeValueList' - One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used. For type Number, value comparisons are numeric. String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> . For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
--
-- * 'cComparisonOperator' - A comparator for evaluating attributes. For example, equals, greater than, less than, etc. The following comparison operators are available: @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@  The following are descriptions of each comparison operator.     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @LE@ : Less than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @LT@ : Less than.  @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GE@ : Greater than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GT@ : Greater than.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.     * @CONTAINS@ : Checks for a subsequence, or value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set. CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set. NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @BEGINS_WITH@ : Checks for a prefix.  @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).     * @IN@ : Checks for matching elements in a list. @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.  @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@  For usage examples of @AttributeValueList@ and @ComparisonOperator@ , see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
condition ::
  -- | 'cComparisonOperator'
  ComparisonOperator ->
  Condition
condition pComparisonOperator_ =
  Condition'
    { _cAttributeValueList = Nothing,
      _cComparisonOperator = pComparisonOperator_
    }

-- | One or more values to evaluate against the supplied attribute. The number of values in the list depends on the @ComparisonOperator@ being used. For type Number, value comparisons are numeric. String value comparisons for greater than, equals, or less than are based on ASCII character code values. For example, @a@ is greater than @A@ , and @a@ is greater than @B@ . For a list of code values, see <http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters> . For Binary, DynamoDB treats each byte of the binary data as unsigned when it compares binary values.
cAttributeValueList :: Lens' Condition [AttributeValue]
cAttributeValueList = lens _cAttributeValueList (\s a -> s {_cAttributeValueList = a}) . _Default . _Coerce

-- | A comparator for evaluating attributes. For example, equals, greater than, less than, etc. The following comparison operators are available: @EQ | NE | LE | LT | GE | GT | NOT_NULL | NULL | CONTAINS | NOT_CONTAINS | BEGINS_WITH | IN | BETWEEN@  The following are descriptions of each comparison operator.     * @EQ@ : Equal. @EQ@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @NE@ : Not equal. @NE@ is supported for all data types, including lists and maps. @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, Binary, String Set, Number Set, or Binary Set. If an item contains an @AttributeValue@ of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not equal @{"NS":["6", "2", "1"]}@ .     * @LE@ : Less than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @LT@ : Less than.  @AttributeValueList@ can contain only one @AttributeValue@ of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GE@ : Greater than or equal.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @GT@ : Greater than.  @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not equal @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@ .     * @NOT_NULL@ : The attribute exists. @NOT_NULL@ is supported for all data types, including lists and maps.     * @NULL@ : The attribute does not exist. @NULL@ is supported for all data types, including lists and maps.     * @CONTAINS@ : Checks for a subsequence, or value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is of type String, then the operator checks for a substring match. If the target attribute of the comparison is of type Binary, then the operator looks for a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it finds an exact match with any member of the set. CONTAINS is supported for lists: When evaluating "@a CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @NOT_CONTAINS@ : Checks for absence of a subsequence, or absence of a value in a set. @AttributeValueList@ can contain only one @AttributeValue@ element of type String, Number, or Binary (not a set type). If the target attribute of the comparison is a String, then the operator checks for the absence of a substring match. If the target attribute of the comparison is Binary, then the operator checks for the absence of a subsequence of the target that matches the input. If the target attribute of the comparison is a set ("@SS@ ", "@NS@ ", or "@BS@ "), then the operator evaluates to true if it /does not/ find an exact match with any member of the set. NOT_CONTAINS is supported for lists: When evaluating "@a NOT CONTAINS b@ ", "@a@ " can be a list; however, "@b@ " cannot be a set, a map, or a list.     * @BEGINS_WITH@ : Checks for a prefix.  @AttributeValueList@ can contain only one @AttributeValue@ of type String or Binary (not a Number or a set type). The target attribute of the comparison must be of type String or Binary (not a Number or a set type).     * @IN@ : Checks for matching elements in a list. @AttributeValueList@ can contain one or more @AttributeValue@ elements of type String, Number, or Binary. These attributes are compared against an existing attribute of an item. If any elements of the input are equal to the item attribute, the expression evaluates to true.     * @BETWEEN@ : Greater than or equal to the first value, and less than or equal to the second value.  @AttributeValueList@ must contain two @AttributeValue@ elements of the same type, either String, Number, or Binary (not a set type). A target attribute matches if the target value is greater than, or equal to, the first element and less than, or equal to, the second element. If an item contains an @AttributeValue@ element of a different type than the one provided in the request, the value does not match. For example, @{"S":"6"}@ does not compare to @{"N":"6"}@ . Also, @{"N":"6"}@ does not compare to @{"NS":["6", "2", "1"]}@  For usage examples of @AttributeValueList@ and @ComparisonOperator@ , see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/LegacyConditionalParameters.html Legacy Conditional Parameters> in the /Amazon DynamoDB Developer Guide/ .
cComparisonOperator :: Lens' Condition ComparisonOperator
cComparisonOperator = lens _cComparisonOperator (\s a -> s {_cComparisonOperator = a})

instance Hashable Condition

instance NFData Condition

instance ToJSON Condition where
  toJSON Condition' {..} =
    object
      ( catMaybes
          [ ("AttributeValueList" .=) <$> _cAttributeValueList,
            Just ("ComparisonOperator" .= _cComparisonOperator)
          ]
      )
