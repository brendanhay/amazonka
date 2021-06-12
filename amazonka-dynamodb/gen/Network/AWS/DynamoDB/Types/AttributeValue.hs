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
-- Module      : Network.AWS.DynamoDB.Types.AttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AttributeValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the data for an attribute.
--
-- Each attribute value is described as a name-value pair. The name is the
-- data type, and the value is the data itself.
--
-- For more information, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.NamingRulesDataTypes.html#HowItWorks.DataTypes Data Types>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | An attribute of type Binary Set. For example:
    --
    -- @\"BS\": [\"U3Vubnk=\", \"UmFpbnk=\", \"U25vd3k=\"]@
    bs :: Core.Maybe [Core.Base64],
    -- | An attribute of type Boolean. For example:
    --
    -- @\"BOOL\": true@
    bool :: Core.Maybe Core.Bool,
    -- | An attribute of type Number. For example:
    --
    -- @\"N\": \"123.45\"@
    --
    -- Numbers are sent across the network to DynamoDB as strings, to maximize
    -- compatibility across languages and libraries. However, DynamoDB treats
    -- them as number type attributes for mathematical operations.
    n :: Core.Maybe Core.Text,
    -- | An attribute of type String. For example:
    --
    -- @\"S\": \"Hello\"@
    s :: Core.Maybe Core.Text,
    -- | An attribute of type Null. For example:
    --
    -- @\"NULL\": true@
    null :: Core.Maybe Core.Bool,
    -- | An attribute of type Map. For example:
    --
    -- @\"M\": {\"Name\": {\"S\": \"Joe\"}, \"Age\": {\"N\": \"35\"}}@
    m :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | An attribute of type Binary. For example:
    --
    -- @\"B\": \"dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk\"@
    b :: Core.Maybe Core.Base64,
    -- | An attribute of type List. For example:
    --
    -- @\"L\": [ {\"S\": \"Cookies\"} , {\"S\": \"Coffee\"}, {\"N\", \"3.14159\"}]@
    l :: Core.Maybe [AttributeValue],
    -- | An attribute of type String Set. For example:
    --
    -- @\"SS\": [\"Giraffe\", \"Hippo\" ,\"Zebra\"]@
    ss :: Core.Maybe [Core.Text],
    -- | An attribute of type Number Set. For example:
    --
    -- @\"NS\": [\"42.2\", \"-19\", \"7.5\", \"3.14\"]@
    --
    -- Numbers are sent across the network to DynamoDB as strings, to maximize
    -- compatibility across languages and libraries. However, DynamoDB treats
    -- them as number type attributes for mathematical operations.
    ns :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bs', 'attributeValue_bs' - An attribute of type Binary Set. For example:
--
-- @\"BS\": [\"U3Vubnk=\", \"UmFpbnk=\", \"U25vd3k=\"]@
--
-- 'bool', 'attributeValue_bool' - An attribute of type Boolean. For example:
--
-- @\"BOOL\": true@
--
-- 'n', 'attributeValue_n' - An attribute of type Number. For example:
--
-- @\"N\": \"123.45\"@
--
-- Numbers are sent across the network to DynamoDB as strings, to maximize
-- compatibility across languages and libraries. However, DynamoDB treats
-- them as number type attributes for mathematical operations.
--
-- 's', 'attributeValue_s' - An attribute of type String. For example:
--
-- @\"S\": \"Hello\"@
--
-- 'null', 'attributeValue_null' - An attribute of type Null. For example:
--
-- @\"NULL\": true@
--
-- 'm', 'attributeValue_m' - An attribute of type Map. For example:
--
-- @\"M\": {\"Name\": {\"S\": \"Joe\"}, \"Age\": {\"N\": \"35\"}}@
--
-- 'b', 'attributeValue_b' - An attribute of type Binary. For example:
--
-- @\"B\": \"dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk\"@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'l', 'attributeValue_l' - An attribute of type List. For example:
--
-- @\"L\": [ {\"S\": \"Cookies\"} , {\"S\": \"Coffee\"}, {\"N\", \"3.14159\"}]@
--
-- 'ss', 'attributeValue_ss' - An attribute of type String Set. For example:
--
-- @\"SS\": [\"Giraffe\", \"Hippo\" ,\"Zebra\"]@
--
-- 'ns', 'attributeValue_ns' - An attribute of type Number Set. For example:
--
-- @\"NS\": [\"42.2\", \"-19\", \"7.5\", \"3.14\"]@
--
-- Numbers are sent across the network to DynamoDB as strings, to maximize
-- compatibility across languages and libraries. However, DynamoDB treats
-- them as number type attributes for mathematical operations.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue'
    { bs = Core.Nothing,
      bool = Core.Nothing,
      n = Core.Nothing,
      s = Core.Nothing,
      null = Core.Nothing,
      m = Core.Nothing,
      b = Core.Nothing,
      l = Core.Nothing,
      ss = Core.Nothing,
      ns = Core.Nothing
    }

-- | An attribute of type Binary Set. For example:
--
-- @\"BS\": [\"U3Vubnk=\", \"UmFpbnk=\", \"U25vd3k=\"]@
attributeValue_bs :: Lens.Lens' AttributeValue (Core.Maybe [Core.ByteString])
attributeValue_bs = Lens.lens (\AttributeValue' {bs} -> bs) (\s@AttributeValue' {} a -> s {bs = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

-- | An attribute of type Boolean. For example:
--
-- @\"BOOL\": true@
attributeValue_bool :: Lens.Lens' AttributeValue (Core.Maybe Core.Bool)
attributeValue_bool = Lens.lens (\AttributeValue' {bool} -> bool) (\s@AttributeValue' {} a -> s {bool = a} :: AttributeValue)

-- | An attribute of type Number. For example:
--
-- @\"N\": \"123.45\"@
--
-- Numbers are sent across the network to DynamoDB as strings, to maximize
-- compatibility across languages and libraries. However, DynamoDB treats
-- them as number type attributes for mathematical operations.
attributeValue_n :: Lens.Lens' AttributeValue (Core.Maybe Core.Text)
attributeValue_n = Lens.lens (\AttributeValue' {n} -> n) (\s@AttributeValue' {} a -> s {n = a} :: AttributeValue)

-- | An attribute of type String. For example:
--
-- @\"S\": \"Hello\"@
attributeValue_s :: Lens.Lens' AttributeValue (Core.Maybe Core.Text)
attributeValue_s = Lens.lens (\AttributeValue' {s} -> s) (\s@AttributeValue' {} a -> s {s = a} :: AttributeValue)

-- | An attribute of type Null. For example:
--
-- @\"NULL\": true@
attributeValue_null :: Lens.Lens' AttributeValue (Core.Maybe Core.Bool)
attributeValue_null = Lens.lens (\AttributeValue' {null} -> null) (\s@AttributeValue' {} a -> s {null = a} :: AttributeValue)

-- | An attribute of type Map. For example:
--
-- @\"M\": {\"Name\": {\"S\": \"Joe\"}, \"Age\": {\"N\": \"35\"}}@
attributeValue_m :: Lens.Lens' AttributeValue (Core.Maybe (Core.HashMap Core.Text AttributeValue))
attributeValue_m = Lens.lens (\AttributeValue' {m} -> m) (\s@AttributeValue' {} a -> s {m = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

-- | An attribute of type Binary. For example:
--
-- @\"B\": \"dGhpcyB0ZXh0IGlzIGJhc2U2NC1lbmNvZGVk\"@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
attributeValue_b :: Lens.Lens' AttributeValue (Core.Maybe Core.ByteString)
attributeValue_b = Lens.lens (\AttributeValue' {b} -> b) (\s@AttributeValue' {} a -> s {b = a} :: AttributeValue) Core.. Lens.mapping Core._Base64

-- | An attribute of type List. For example:
--
-- @\"L\": [ {\"S\": \"Cookies\"} , {\"S\": \"Coffee\"}, {\"N\", \"3.14159\"}]@
attributeValue_l :: Lens.Lens' AttributeValue (Core.Maybe [AttributeValue])
attributeValue_l = Lens.lens (\AttributeValue' {l} -> l) (\s@AttributeValue' {} a -> s {l = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

-- | An attribute of type String Set. For example:
--
-- @\"SS\": [\"Giraffe\", \"Hippo\" ,\"Zebra\"]@
attributeValue_ss :: Lens.Lens' AttributeValue (Core.Maybe [Core.Text])
attributeValue_ss = Lens.lens (\AttributeValue' {ss} -> ss) (\s@AttributeValue' {} a -> s {ss = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

-- | An attribute of type Number Set. For example:
--
-- @\"NS\": [\"42.2\", \"-19\", \"7.5\", \"3.14\"]@
--
-- Numbers are sent across the network to DynamoDB as strings, to maximize
-- compatibility across languages and libraries. However, DynamoDB treats
-- them as number type attributes for mathematical operations.
attributeValue_ns :: Lens.Lens' AttributeValue (Core.Maybe [Core.Text])
attributeValue_ns = Lens.lens (\AttributeValue' {ns} -> ns) (\s@AttributeValue' {} a -> s {ns = a} :: AttributeValue) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AttributeValue where
  parseJSON =
    Core.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Core.<$> (x Core..:? "BS" Core..!= Core.mempty)
            Core.<*> (x Core..:? "BOOL")
            Core.<*> (x Core..:? "N")
            Core.<*> (x Core..:? "S")
            Core.<*> (x Core..:? "NULL")
            Core.<*> (x Core..:? "M" Core..!= Core.mempty)
            Core.<*> (x Core..:? "B")
            Core.<*> (x Core..:? "L" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SS" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NS" Core..!= Core.mempty)
      )

instance Core.Hashable AttributeValue

instance Core.NFData AttributeValue

instance Core.ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BS" Core..=) Core.<$> bs,
            ("BOOL" Core..=) Core.<$> bool,
            ("N" Core..=) Core.<$> n,
            ("S" Core..=) Core.<$> s,
            ("NULL" Core..=) Core.<$> null,
            ("M" Core..=) Core.<$> m,
            ("B" Core..=) Core.<$> b,
            ("L" Core..=) Core.<$> l,
            ("SS" Core..=) Core.<$> ss,
            ("NS" Core..=) Core.<$> ns
          ]
      )
