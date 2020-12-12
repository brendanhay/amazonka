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
    avL,
    avNS,
    avM,
    avNULL,
    avN,
    avBS,
    avB,
    avSS,
    avS,
    avBOOL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the data for an attribute.
--
-- Each attribute value is described as a name-value pair. The name is the data type, and the value is the data itself.
-- For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types> in the /Amazon DynamoDB Developer Guide/ .
--
-- /See:/ 'mkAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { l ::
      Lude.Maybe [AttributeValue],
    nS :: Lude.Maybe [Lude.Text],
    m :: Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)),
    nULL :: Lude.Maybe Lude.Bool,
    n :: Lude.Maybe Lude.Text,
    bS :: Lude.Maybe [Lude.Base64],
    b :: Lude.Maybe Lude.Base64,
    sS :: Lude.Maybe [Lude.Text],
    s :: Lude.Maybe Lude.Text,
    bOOL :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- * 'b' - An attribute of type Binary. For example:
--
-- @"B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"@ --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'bOOL' - An attribute of type Boolean. For example:
--
-- @"BOOL": true@
-- * 'bS' - An attribute of type Binary Set. For example:
--
-- @"BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]@
-- * 'l' - An attribute of type List. For example:
--
-- @"L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]@
-- * 'm' - An attribute of type Map. For example:
--
-- @"M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}@
-- * 'n' - An attribute of type Number. For example:
--
-- @"N": "123.45"@
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
-- * 'nS' - An attribute of type Number Set. For example:
--
-- @"NS": ["42.2", "-19", "7.5", "3.14"]@
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
-- * 'nULL' - An attribute of type Null. For example:
--
-- @"NULL": true@
-- * 's' - An attribute of type String. For example:
--
-- @"S": "Hello"@
-- * 'sS' - An attribute of type String Set. For example:
--
-- @"SS": ["Giraffe", "Hippo" ,"Zebra"]@
mkAttributeValue ::
  AttributeValue
mkAttributeValue =
  AttributeValue'
    { l = Lude.Nothing,
      nS = Lude.Nothing,
      m = Lude.Nothing,
      nULL = Lude.Nothing,
      n = Lude.Nothing,
      bS = Lude.Nothing,
      b = Lude.Nothing,
      sS = Lude.Nothing,
      s = Lude.Nothing,
      bOOL = Lude.Nothing
    }

-- | An attribute of type List. For example:
--
-- @"L": [ {"S": "Cookies"} , {"S": "Coffee"}, {"N", "3.14159"}]@
--
-- /Note:/ Consider using 'l' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avL :: Lens.Lens' AttributeValue (Lude.Maybe [AttributeValue])
avL = Lens.lens (l :: AttributeValue -> Lude.Maybe [AttributeValue]) (\s a -> s {l = a} :: AttributeValue)
{-# DEPRECATED avL "Use generic-lens or generic-optics with 'l' instead." #-}

-- | An attribute of type Number Set. For example:
--
-- @"NS": ["42.2", "-19", "7.5", "3.14"]@
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
--
-- /Note:/ Consider using 'nS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNS :: Lens.Lens' AttributeValue (Lude.Maybe [Lude.Text])
avNS = Lens.lens (nS :: AttributeValue -> Lude.Maybe [Lude.Text]) (\s a -> s {nS = a} :: AttributeValue)
{-# DEPRECATED avNS "Use generic-lens or generic-optics with 'nS' instead." #-}

-- | An attribute of type Map. For example:
--
-- @"M": {"Name": {"S": "Joe"}, "Age": {"N": "35"}}@
--
-- /Note:/ Consider using 'm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avM :: Lens.Lens' AttributeValue (Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue)))
avM = Lens.lens (m :: AttributeValue -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeValue))) (\s a -> s {m = a} :: AttributeValue)
{-# DEPRECATED avM "Use generic-lens or generic-optics with 'm' instead." #-}

-- | An attribute of type Null. For example:
--
-- @"NULL": true@
--
-- /Note:/ Consider using 'nULL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avNULL :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Bool)
avNULL = Lens.lens (nULL :: AttributeValue -> Lude.Maybe Lude.Bool) (\s a -> s {nULL = a} :: AttributeValue)
{-# DEPRECATED avNULL "Use generic-lens or generic-optics with 'nULL' instead." #-}

-- | An attribute of type Number. For example:
--
-- @"N": "123.45"@
-- Numbers are sent across the network to DynamoDB as strings, to maximize compatibility across languages and libraries. However, DynamoDB treats them as number type attributes for mathematical operations.
--
-- /Note:/ Consider using 'n' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avN :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Text)
avN = Lens.lens (n :: AttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {n = a} :: AttributeValue)
{-# DEPRECATED avN "Use generic-lens or generic-optics with 'n' instead." #-}

-- | An attribute of type Binary Set. For example:
--
-- @"BS": ["U3Vubnk=", "UmFpbnk=", "U25vd3k="]@
--
-- /Note:/ Consider using 'bS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBS :: Lens.Lens' AttributeValue (Lude.Maybe [Lude.Base64])
avBS = Lens.lens (bS :: AttributeValue -> Lude.Maybe [Lude.Base64]) (\s a -> s {bS = a} :: AttributeValue)
{-# DEPRECATED avBS "Use generic-lens or generic-optics with 'bS' instead." #-}

-- | An attribute of type Binary. For example:
--
-- @"B": "dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk"@ --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'b' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avB :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Base64)
avB = Lens.lens (b :: AttributeValue -> Lude.Maybe Lude.Base64) (\s a -> s {b = a} :: AttributeValue)
{-# DEPRECATED avB "Use generic-lens or generic-optics with 'b' instead." #-}

-- | An attribute of type String Set. For example:
--
-- @"SS": ["Giraffe", "Hippo" ,"Zebra"]@
--
-- /Note:/ Consider using 'sS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avSS :: Lens.Lens' AttributeValue (Lude.Maybe [Lude.Text])
avSS = Lens.lens (sS :: AttributeValue -> Lude.Maybe [Lude.Text]) (\s a -> s {sS = a} :: AttributeValue)
{-# DEPRECATED avSS "Use generic-lens or generic-optics with 'sS' instead." #-}

-- | An attribute of type String. For example:
--
-- @"S": "Hello"@
--
-- /Note:/ Consider using 's' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avS :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Text)
avS = Lens.lens (s :: AttributeValue -> Lude.Maybe Lude.Text) (\s a -> s {s = a} :: AttributeValue)
{-# DEPRECATED avS "Use generic-lens or generic-optics with 's' instead." #-}

-- | An attribute of type Boolean. For example:
--
-- @"BOOL": true@
--
-- /Note:/ Consider using 'bOOL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avBOOL :: Lens.Lens' AttributeValue (Lude.Maybe Lude.Bool)
avBOOL = Lens.lens (bOOL :: AttributeValue -> Lude.Maybe Lude.Bool) (\s a -> s {bOOL = a} :: AttributeValue)
{-# DEPRECATED avBOOL "Use generic-lens or generic-optics with 'bOOL' instead." #-}

instance Lude.FromJSON AttributeValue where
  parseJSON =
    Lude.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Lude.<$> (x Lude..:? "L" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NS" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "M" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "NULL")
            Lude.<*> (x Lude..:? "N")
            Lude.<*> (x Lude..:? "BS" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "B")
            Lude.<*> (x Lude..:? "SS" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "S")
            Lude.<*> (x Lude..:? "BOOL")
      )
