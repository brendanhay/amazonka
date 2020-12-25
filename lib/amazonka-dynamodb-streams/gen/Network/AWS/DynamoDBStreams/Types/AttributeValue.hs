{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.AttributeValue
  ( AttributeValue (..),

    -- * Smart constructor
    mkAttributeValue,

    -- * Lenses
    avB,
    avBOOL,
    avBS,
    avL,
    avM,
    avN,
    avNS,
    avNULL,
    avS,
    avSS,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types.AttributeName as Types
import qualified Network.AWS.DynamoDBStreams.Types.N as Types
import qualified Network.AWS.DynamoDBStreams.Types.NumberAttributeValue as Types
import qualified Network.AWS.DynamoDBStreams.Types.S as Types
import qualified Network.AWS.DynamoDBStreams.Types.StringAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the data for an attribute.
--
-- Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
--
-- /See:/ 'mkAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | An attribute of type Binary. For example:
    --
    -- @"B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"@
    b :: Core.Maybe Core.Base64,
    -- | An attribute of type Boolean. For example:
    --
    -- @"BOOL": true@
    bool :: Core.Maybe Core.Bool,
    -- | An attribute of type Binary Set. For example:
    --
    -- @"BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]@
    bs :: Core.Maybe [Core.Base64],
    -- | An attribute of type List. For example:
    --
    -- @"L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]@
    l :: Core.Maybe [AttributeValue],
    -- | An attribute of type Map. For example:
    --
    -- @"M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}@
    m :: Core.Maybe (Core.HashMap Types.AttributeName AttributeValue),
    -- | An attribute of type Number. For example:
    --
    -- @"N": "123.45"@
    -- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
    n :: Core.Maybe Types.N,
    -- | An attribute of type Number Set. For example:
    --
    -- @"NS": ["42.2", "-19", "7.5", "3.14"]@
    -- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
    ns :: Core.Maybe [Types.NumberAttributeValue],
    -- | An attribute of type Null. For example:
    --
    -- @"NULL": true@
    null :: Core.Maybe Core.Bool,
    -- | An attribute of type String. For example:
    --
    -- @"S": "Hello"@
    s :: Core.Maybe Types.S,
    -- | An attribute of type String Set. For example:
    --
    -- @"SS": ["Giraffe", "Hippo" ,"Zebra"]@
    ss :: Core.Maybe [Types.StringAttributeValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeValue' value with any optional fields omitted.
mkAttributeValue ::
  AttributeValue
mkAttributeValue =
  AttributeValue'
    { b = Core.Nothing,
      bool = Core.Nothing,
      bs = Core.Nothing,
      l = Core.Nothing,
      m = Core.Nothing,
      n = Core.Nothing,
      ns = Core.Nothing,
      null = Core.Nothing,
      s = Core.Nothing,
      ss = Core.Nothing
    }

-- | An attribute of type Binary. For example:
--
-- @"B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"@ --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'b' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avB :: Lens.Lens' AttributeValue (Core.Maybe Core.Base64)
avB = Lens.field @"b"
{-# DEPRECATED avB "Use generic-lens or generic-optics with 'b' instead." #-}

-- | An attribute of type Boolean. For example:
--
-- @"BOOL": true@
--
-- /Note:/ Consider using 'bool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBOOL :: Lens.Lens' AttributeValue (Core.Maybe Core.Bool)
avBOOL = Lens.field @"bool"
{-# DEPRECATED avBOOL "Use generic-lens or generic-optics with 'bool' instead." #-}

-- | An attribute of type Binary Set. For example:
--
-- @"BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]@
--
-- /Note:/ Consider using 'bs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBS :: Lens.Lens' AttributeValue (Core.Maybe [Core.Base64])
avBS = Lens.field @"bs"
{-# DEPRECATED avBS "Use generic-lens or generic-optics with 'bs' instead." #-}

-- | An attribute of type List. For example:
--
-- @"L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]@
--
-- /Note:/ Consider using 'l' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avL :: Lens.Lens' AttributeValue (Core.Maybe [AttributeValue])
avL = Lens.field @"l"
{-# DEPRECATED avL "Use generic-lens or generic-optics with 'l' instead." #-}

-- | An attribute of type Map. For example:
--
-- @"M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}@
--
-- /Note:/ Consider using 'm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avM :: Lens.Lens' AttributeValue (Core.Maybe (Core.HashMap Types.AttributeName AttributeValue))
avM = Lens.field @"m"
{-# DEPRECATED avM "Use generic-lens or generic-optics with 'm' instead." #-}

-- | An attribute of type Number. For example:
--
-- @"N": "123.45"@
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
--
-- /Note:/ Consider using 'n' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avN :: Lens.Lens' AttributeValue (Core.Maybe Types.N)
avN = Lens.field @"n"
{-# DEPRECATED avN "Use generic-lens or generic-optics with 'n' instead." #-}

-- | An attribute of type Number Set. For example:
--
-- @"NS": ["42.2", "-19", "7.5", "3.14"]@
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
--
-- /Note:/ Consider using 'ns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNS :: Lens.Lens' AttributeValue (Core.Maybe [Types.NumberAttributeValue])
avNS = Lens.field @"ns"
{-# DEPRECATED avNS "Use generic-lens or generic-optics with 'ns' instead." #-}

-- | An attribute of type Null. For example:
--
-- @"NULL": true@
--
-- /Note:/ Consider using 'null' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNULL :: Lens.Lens' AttributeValue (Core.Maybe Core.Bool)
avNULL = Lens.field @"null"
{-# DEPRECATED avNULL "Use generic-lens or generic-optics with 'null' instead." #-}

-- | An attribute of type String. For example:
--
-- @"S": "Hello"@
--
-- /Note:/ Consider using 's' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avS :: Lens.Lens' AttributeValue (Core.Maybe Types.S)
avS = Lens.field @"s"
{-# DEPRECATED avS "Use generic-lens or generic-optics with 's' instead." #-}

-- | An attribute of type String Set. For example:
--
-- @"SS": ["Giraffe", "Hippo" ,"Zebra"]@
--
-- /Note:/ Consider using 'ss' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSS :: Lens.Lens' AttributeValue (Core.Maybe [Types.StringAttributeValue])
avSS = Lens.field @"ss"
{-# DEPRECATED avSS "Use generic-lens or generic-optics with 'ss' instead." #-}

instance Core.FromJSON AttributeValue where
  parseJSON =
    Core.withObject "AttributeValue" Core.$
      \x ->
        AttributeValue'
          Core.<$> (x Core..:? "B")
          Core.<*> (x Core..:? "BOOL")
          Core.<*> (x Core..:? "BS")
          Core.<*> (x Core..:? "L")
          Core.<*> (x Core..:? "M")
          Core.<*> (x Core..:? "N")
          Core.<*> (x Core..:? "NS")
          Core.<*> (x Core..:? "NULL")
          Core.<*> (x Core..:? "S")
          Core.<*> (x Core..:? "SS")
