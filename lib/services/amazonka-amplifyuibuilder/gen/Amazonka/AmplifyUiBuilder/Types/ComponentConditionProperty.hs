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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentConditionProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentConditionProperty where

import {-# SOURCE #-} Amazonka.AmplifyUiBuilder.Types.ComponentProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a conditional expression to set a component property. Use
-- @ComponentConditionProperty@ to set a property to different values
-- conditionally, based on the value of another property.
--
-- /See:/ 'newComponentConditionProperty' smart constructor.
data ComponentConditionProperty = ComponentConditionProperty'
  { -- | The value of the property to evaluate.
    operand :: Prelude.Maybe Prelude.Text,
    -- | The value to assign to the property if the condition is not met.
    else' :: Prelude.Maybe ComponentProperty,
    -- | The value to assign to the property if the condition is met.
    then' :: Prelude.Maybe ComponentProperty,
    -- | The type of the property to evaluate.
    operandType :: Prelude.Maybe Prelude.Text,
    -- | The name of a field. Specify this when the property is a data model.
    field :: Prelude.Maybe Prelude.Text,
    -- | The name of the conditional property.
    property :: Prelude.Maybe Prelude.Text,
    -- | The operator to use to perform the evaluation, such as @eq@ to represent
    -- equals.
    operator :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentConditionProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operand', 'componentConditionProperty_operand' - The value of the property to evaluate.
--
-- 'else'', 'componentConditionProperty_else' - The value to assign to the property if the condition is not met.
--
-- 'then'', 'componentConditionProperty_then' - The value to assign to the property if the condition is met.
--
-- 'operandType', 'componentConditionProperty_operandType' - The type of the property to evaluate.
--
-- 'field', 'componentConditionProperty_field' - The name of a field. Specify this when the property is a data model.
--
-- 'property', 'componentConditionProperty_property' - The name of the conditional property.
--
-- 'operator', 'componentConditionProperty_operator' - The operator to use to perform the evaluation, such as @eq@ to represent
-- equals.
newComponentConditionProperty ::
  ComponentConditionProperty
newComponentConditionProperty =
  ComponentConditionProperty'
    { operand =
        Prelude.Nothing,
      else' = Prelude.Nothing,
      then' = Prelude.Nothing,
      operandType = Prelude.Nothing,
      field = Prelude.Nothing,
      property = Prelude.Nothing,
      operator = Prelude.Nothing
    }

-- | The value of the property to evaluate.
componentConditionProperty_operand :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_operand = Lens.lens (\ComponentConditionProperty' {operand} -> operand) (\s@ComponentConditionProperty' {} a -> s {operand = a} :: ComponentConditionProperty)

-- | The value to assign to the property if the condition is not met.
componentConditionProperty_else :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe ComponentProperty)
componentConditionProperty_else = Lens.lens (\ComponentConditionProperty' {else'} -> else') (\s@ComponentConditionProperty' {} a -> s {else' = a} :: ComponentConditionProperty)

-- | The value to assign to the property if the condition is met.
componentConditionProperty_then :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe ComponentProperty)
componentConditionProperty_then = Lens.lens (\ComponentConditionProperty' {then'} -> then') (\s@ComponentConditionProperty' {} a -> s {then' = a} :: ComponentConditionProperty)

-- | The type of the property to evaluate.
componentConditionProperty_operandType :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_operandType = Lens.lens (\ComponentConditionProperty' {operandType} -> operandType) (\s@ComponentConditionProperty' {} a -> s {operandType = a} :: ComponentConditionProperty)

-- | The name of a field. Specify this when the property is a data model.
componentConditionProperty_field :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_field = Lens.lens (\ComponentConditionProperty' {field} -> field) (\s@ComponentConditionProperty' {} a -> s {field = a} :: ComponentConditionProperty)

-- | The name of the conditional property.
componentConditionProperty_property :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_property = Lens.lens (\ComponentConditionProperty' {property} -> property) (\s@ComponentConditionProperty' {} a -> s {property = a} :: ComponentConditionProperty)

-- | The operator to use to perform the evaluation, such as @eq@ to represent
-- equals.
componentConditionProperty_operator :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_operator = Lens.lens (\ComponentConditionProperty' {operator} -> operator) (\s@ComponentConditionProperty' {} a -> s {operator = a} :: ComponentConditionProperty)

instance Data.FromJSON ComponentConditionProperty where
  parseJSON =
    Data.withObject
      "ComponentConditionProperty"
      ( \x ->
          ComponentConditionProperty'
            Prelude.<$> (x Data..:? "operand")
            Prelude.<*> (x Data..:? "else")
            Prelude.<*> (x Data..:? "then")
            Prelude.<*> (x Data..:? "operandType")
            Prelude.<*> (x Data..:? "field")
            Prelude.<*> (x Data..:? "property")
            Prelude.<*> (x Data..:? "operator")
      )

instance Prelude.Hashable ComponentConditionProperty where
  hashWithSalt _salt ComponentConditionProperty' {..} =
    _salt `Prelude.hashWithSalt` operand
      `Prelude.hashWithSalt` else'
      `Prelude.hashWithSalt` then'
      `Prelude.hashWithSalt` operandType
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` property
      `Prelude.hashWithSalt` operator

instance Prelude.NFData ComponentConditionProperty where
  rnf ComponentConditionProperty' {..} =
    Prelude.rnf operand
      `Prelude.seq` Prelude.rnf else'
      `Prelude.seq` Prelude.rnf then'
      `Prelude.seq` Prelude.rnf operandType
      `Prelude.seq` Prelude.rnf field
      `Prelude.seq` Prelude.rnf property
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON ComponentConditionProperty where
  toJSON ComponentConditionProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("operand" Data..=) Prelude.<$> operand,
            ("else" Data..=) Prelude.<$> else',
            ("then" Data..=) Prelude.<$> then',
            ("operandType" Data..=) Prelude.<$> operandType,
            ("field" Data..=) Prelude.<$> field,
            ("property" Data..=) Prelude.<$> property,
            ("operator" Data..=) Prelude.<$> operator
          ]
      )
