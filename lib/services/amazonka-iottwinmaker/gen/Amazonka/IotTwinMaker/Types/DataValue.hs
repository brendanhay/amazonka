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
-- Module      : Amazonka.IotTwinMaker.Types.DataValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.DataValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.RelationshipValue
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies a value for a property.
--
-- /See:/ 'newDataValue' smart constructor.
data DataValue = DataValue'
  { -- | A Boolean value.
    booleanValue :: Prelude.Maybe Prelude.Bool,
    -- | A double value.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | An expression that produces the value.
    expression :: Prelude.Maybe Prelude.Text,
    -- | An integer value.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | A list of multiple values.
    listValue :: Prelude.Maybe [DataValue],
    -- | A long value.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | An object that maps strings to multiple @DataValue@ objects.
    mapValue :: Prelude.Maybe (Prelude.HashMap Prelude.Text DataValue),
    -- | A value that relates a component to another component.
    relationshipValue :: Prelude.Maybe RelationshipValue,
    -- | A string value.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'booleanValue', 'dataValue_booleanValue' - A Boolean value.
--
-- 'doubleValue', 'dataValue_doubleValue' - A double value.
--
-- 'expression', 'dataValue_expression' - An expression that produces the value.
--
-- 'integerValue', 'dataValue_integerValue' - An integer value.
--
-- 'listValue', 'dataValue_listValue' - A list of multiple values.
--
-- 'longValue', 'dataValue_longValue' - A long value.
--
-- 'mapValue', 'dataValue_mapValue' - An object that maps strings to multiple @DataValue@ objects.
--
-- 'relationshipValue', 'dataValue_relationshipValue' - A value that relates a component to another component.
--
-- 'stringValue', 'dataValue_stringValue' - A string value.
newDataValue ::
  DataValue
newDataValue =
  DataValue'
    { booleanValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      expression = Prelude.Nothing,
      integerValue = Prelude.Nothing,
      listValue = Prelude.Nothing,
      longValue = Prelude.Nothing,
      mapValue = Prelude.Nothing,
      relationshipValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | A Boolean value.
dataValue_booleanValue :: Lens.Lens' DataValue (Prelude.Maybe Prelude.Bool)
dataValue_booleanValue = Lens.lens (\DataValue' {booleanValue} -> booleanValue) (\s@DataValue' {} a -> s {booleanValue = a} :: DataValue)

-- | A double value.
dataValue_doubleValue :: Lens.Lens' DataValue (Prelude.Maybe Prelude.Double)
dataValue_doubleValue = Lens.lens (\DataValue' {doubleValue} -> doubleValue) (\s@DataValue' {} a -> s {doubleValue = a} :: DataValue)

-- | An expression that produces the value.
dataValue_expression :: Lens.Lens' DataValue (Prelude.Maybe Prelude.Text)
dataValue_expression = Lens.lens (\DataValue' {expression} -> expression) (\s@DataValue' {} a -> s {expression = a} :: DataValue)

-- | An integer value.
dataValue_integerValue :: Lens.Lens' DataValue (Prelude.Maybe Prelude.Int)
dataValue_integerValue = Lens.lens (\DataValue' {integerValue} -> integerValue) (\s@DataValue' {} a -> s {integerValue = a} :: DataValue)

-- | A list of multiple values.
dataValue_listValue :: Lens.Lens' DataValue (Prelude.Maybe [DataValue])
dataValue_listValue = Lens.lens (\DataValue' {listValue} -> listValue) (\s@DataValue' {} a -> s {listValue = a} :: DataValue) Prelude.. Lens.mapping Lens.coerced

-- | A long value.
dataValue_longValue :: Lens.Lens' DataValue (Prelude.Maybe Prelude.Integer)
dataValue_longValue = Lens.lens (\DataValue' {longValue} -> longValue) (\s@DataValue' {} a -> s {longValue = a} :: DataValue)

-- | An object that maps strings to multiple @DataValue@ objects.
dataValue_mapValue :: Lens.Lens' DataValue (Prelude.Maybe (Prelude.HashMap Prelude.Text DataValue))
dataValue_mapValue = Lens.lens (\DataValue' {mapValue} -> mapValue) (\s@DataValue' {} a -> s {mapValue = a} :: DataValue) Prelude.. Lens.mapping Lens.coerced

-- | A value that relates a component to another component.
dataValue_relationshipValue :: Lens.Lens' DataValue (Prelude.Maybe RelationshipValue)
dataValue_relationshipValue = Lens.lens (\DataValue' {relationshipValue} -> relationshipValue) (\s@DataValue' {} a -> s {relationshipValue = a} :: DataValue)

-- | A string value.
dataValue_stringValue :: Lens.Lens' DataValue (Prelude.Maybe Prelude.Text)
dataValue_stringValue = Lens.lens (\DataValue' {stringValue} -> stringValue) (\s@DataValue' {} a -> s {stringValue = a} :: DataValue)

instance Data.FromJSON DataValue where
  parseJSON =
    Data.withObject
      "DataValue"
      ( \x ->
          DataValue'
            Prelude.<$> (x Data..:? "booleanValue")
            Prelude.<*> (x Data..:? "doubleValue")
            Prelude.<*> (x Data..:? "expression")
            Prelude.<*> (x Data..:? "integerValue")
            Prelude.<*> (x Data..:? "listValue" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "longValue")
            Prelude.<*> (x Data..:? "mapValue" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "relationshipValue")
            Prelude.<*> (x Data..:? "stringValue")
      )

instance Prelude.Hashable DataValue where
  hashWithSalt _salt DataValue' {..} =
    _salt
      `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` listValue
      `Prelude.hashWithSalt` longValue
      `Prelude.hashWithSalt` mapValue
      `Prelude.hashWithSalt` relationshipValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData DataValue where
  rnf DataValue' {..} =
    Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf listValue
      `Prelude.seq` Prelude.rnf longValue
      `Prelude.seq` Prelude.rnf mapValue
      `Prelude.seq` Prelude.rnf relationshipValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON DataValue where
  toJSON DataValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("booleanValue" Data..=) Prelude.<$> booleanValue,
            ("doubleValue" Data..=) Prelude.<$> doubleValue,
            ("expression" Data..=) Prelude.<$> expression,
            ("integerValue" Data..=) Prelude.<$> integerValue,
            ("listValue" Data..=) Prelude.<$> listValue,
            ("longValue" Data..=) Prelude.<$> longValue,
            ("mapValue" Data..=) Prelude.<$> mapValue,
            ("relationshipValue" Data..=)
              Prelude.<$> relationshipValue,
            ("stringValue" Data..=) Prelude.<$> stringValue
          ]
      )
