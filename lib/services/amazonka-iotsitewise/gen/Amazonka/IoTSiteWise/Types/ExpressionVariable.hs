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
-- Module      : Amazonka.IoTSiteWise.Types.ExpressionVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ExpressionVariable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.VariableValue
import qualified Amazonka.Prelude as Prelude

-- | Contains expression variable information.
--
-- /See:/ 'newExpressionVariable' smart constructor.
data ExpressionVariable = ExpressionVariable'
  { -- | The friendly name of the variable to be used in the expression.
    name :: Prelude.Text,
    -- | The variable that identifies an asset property from which to use values.
    value :: VariableValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpressionVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'expressionVariable_name' - The friendly name of the variable to be used in the expression.
--
-- 'value', 'expressionVariable_value' - The variable that identifies an asset property from which to use values.
newExpressionVariable ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  VariableValue ->
  ExpressionVariable
newExpressionVariable pName_ pValue_ =
  ExpressionVariable' {name = pName_, value = pValue_}

-- | The friendly name of the variable to be used in the expression.
expressionVariable_name :: Lens.Lens' ExpressionVariable Prelude.Text
expressionVariable_name = Lens.lens (\ExpressionVariable' {name} -> name) (\s@ExpressionVariable' {} a -> s {name = a} :: ExpressionVariable)

-- | The variable that identifies an asset property from which to use values.
expressionVariable_value :: Lens.Lens' ExpressionVariable VariableValue
expressionVariable_value = Lens.lens (\ExpressionVariable' {value} -> value) (\s@ExpressionVariable' {} a -> s {value = a} :: ExpressionVariable)

instance Data.FromJSON ExpressionVariable where
  parseJSON =
    Data.withObject
      "ExpressionVariable"
      ( \x ->
          ExpressionVariable'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable ExpressionVariable where
  hashWithSalt _salt ExpressionVariable' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ExpressionVariable where
  rnf ExpressionVariable' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ExpressionVariable where
  toJSON ExpressionVariable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )
