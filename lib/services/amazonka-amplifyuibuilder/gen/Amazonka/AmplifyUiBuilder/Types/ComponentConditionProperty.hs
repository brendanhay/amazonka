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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The value to assign to the property if the condition is not met.
    else' :: Prelude.Maybe ComponentProperty,
    -- | The name of a field. Specify this when the property is a data model.
    field :: Prelude.Maybe Prelude.Text,
    -- | The value of the property to evaluate.
    operand :: Prelude.Maybe Prelude.Text,
    -- | The type of the property to evaluate.
    operandType :: Prelude.Maybe Prelude.Text,
    -- | The operator to use to perform the evaluation, such as @eq@ to represent
    -- equals.
    operator :: Prelude.Maybe Prelude.Text,
    -- | The name of the conditional property.
    property :: Prelude.Maybe Prelude.Text,
    -- | The value to assign to the property if the condition is met.
    then' :: Prelude.Maybe ComponentProperty
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
-- 'else'', 'componentConditionProperty_else' - The value to assign to the property if the condition is not met.
--
-- 'field', 'componentConditionProperty_field' - The name of a field. Specify this when the property is a data model.
--
-- 'operand', 'componentConditionProperty_operand' - The value of the property to evaluate.
--
-- 'operandType', 'componentConditionProperty_operandType' - The type of the property to evaluate.
--
-- 'operator', 'componentConditionProperty_operator' - The operator to use to perform the evaluation, such as @eq@ to represent
-- equals.
--
-- 'property', 'componentConditionProperty_property' - The name of the conditional property.
--
-- 'then'', 'componentConditionProperty_then' - The value to assign to the property if the condition is met.
newComponentConditionProperty ::
  ComponentConditionProperty
newComponentConditionProperty =
  ComponentConditionProperty'
    { else' =
        Prelude.Nothing,
      field = Prelude.Nothing,
      operand = Prelude.Nothing,
      operandType = Prelude.Nothing,
      operator = Prelude.Nothing,
      property = Prelude.Nothing,
      then' = Prelude.Nothing
    }

-- | The value to assign to the property if the condition is not met.
componentConditionProperty_else :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe ComponentProperty)
componentConditionProperty_else = Lens.lens (\ComponentConditionProperty' {else'} -> else') (\s@ComponentConditionProperty' {} a -> s {else' = a} :: ComponentConditionProperty)

-- | The name of a field. Specify this when the property is a data model.
componentConditionProperty_field :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_field = Lens.lens (\ComponentConditionProperty' {field} -> field) (\s@ComponentConditionProperty' {} a -> s {field = a} :: ComponentConditionProperty)

-- | The value of the property to evaluate.
componentConditionProperty_operand :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_operand = Lens.lens (\ComponentConditionProperty' {operand} -> operand) (\s@ComponentConditionProperty' {} a -> s {operand = a} :: ComponentConditionProperty)

-- | The type of the property to evaluate.
componentConditionProperty_operandType :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_operandType = Lens.lens (\ComponentConditionProperty' {operandType} -> operandType) (\s@ComponentConditionProperty' {} a -> s {operandType = a} :: ComponentConditionProperty)

-- | The operator to use to perform the evaluation, such as @eq@ to represent
-- equals.
componentConditionProperty_operator :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_operator = Lens.lens (\ComponentConditionProperty' {operator} -> operator) (\s@ComponentConditionProperty' {} a -> s {operator = a} :: ComponentConditionProperty)

-- | The name of the conditional property.
componentConditionProperty_property :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe Prelude.Text)
componentConditionProperty_property = Lens.lens (\ComponentConditionProperty' {property} -> property) (\s@ComponentConditionProperty' {} a -> s {property = a} :: ComponentConditionProperty)

-- | The value to assign to the property if the condition is met.
componentConditionProperty_then :: Lens.Lens' ComponentConditionProperty (Prelude.Maybe ComponentProperty)
componentConditionProperty_then = Lens.lens (\ComponentConditionProperty' {then'} -> then') (\s@ComponentConditionProperty' {} a -> s {then' = a} :: ComponentConditionProperty)

instance Data.FromJSON ComponentConditionProperty where
  parseJSON =
    Data.withObject
      "ComponentConditionProperty"
      ( \x ->
          ComponentConditionProperty'
            Prelude.<$> (x Data..:? "else")
            Prelude.<*> (x Data..:? "field")
            Prelude.<*> (x Data..:? "operand")
            Prelude.<*> (x Data..:? "operandType")
            Prelude.<*> (x Data..:? "operator")
            Prelude.<*> (x Data..:? "property")
            Prelude.<*> (x Data..:? "then")
      )

instance Prelude.Hashable ComponentConditionProperty where
  hashWithSalt _salt ComponentConditionProperty' {..} =
    _salt
      `Prelude.hashWithSalt` else'
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` operand
      `Prelude.hashWithSalt` operandType
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` property
      `Prelude.hashWithSalt` then'

instance Prelude.NFData ComponentConditionProperty where
  rnf ComponentConditionProperty' {..} =
    Prelude.rnf else' `Prelude.seq`
      Prelude.rnf field `Prelude.seq`
        Prelude.rnf operand `Prelude.seq`
          Prelude.rnf operandType `Prelude.seq`
            Prelude.rnf operator `Prelude.seq`
              Prelude.rnf property `Prelude.seq`
                Prelude.rnf then'

instance Data.ToJSON ComponentConditionProperty where
  toJSON ComponentConditionProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("else" Data..=) Prelude.<$> else',
            ("field" Data..=) Prelude.<$> field,
            ("operand" Data..=) Prelude.<$> operand,
            ("operandType" Data..=) Prelude.<$> operandType,
            ("operator" Data..=) Prelude.<$> operator,
            ("property" Data..=) Prelude.<$> property,
            ("then" Data..=) Prelude.<$> then'
          ]
      )
