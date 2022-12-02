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
-- Module      : Amazonka.IoTEventsData.Types.VariableDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.VariableDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The new value of the variable.
--
-- /See:/ 'newVariableDefinition' smart constructor.
data VariableDefinition = VariableDefinition'
  { -- | The name of the variable.
    name :: Prelude.Text,
    -- | The new value of the variable.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'variableDefinition_name' - The name of the variable.
--
-- 'value', 'variableDefinition_value' - The new value of the variable.
newVariableDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  VariableDefinition
newVariableDefinition pName_ pValue_ =
  VariableDefinition' {name = pName_, value = pValue_}

-- | The name of the variable.
variableDefinition_name :: Lens.Lens' VariableDefinition Prelude.Text
variableDefinition_name = Lens.lens (\VariableDefinition' {name} -> name) (\s@VariableDefinition' {} a -> s {name = a} :: VariableDefinition)

-- | The new value of the variable.
variableDefinition_value :: Lens.Lens' VariableDefinition Prelude.Text
variableDefinition_value = Lens.lens (\VariableDefinition' {value} -> value) (\s@VariableDefinition' {} a -> s {value = a} :: VariableDefinition)

instance Prelude.Hashable VariableDefinition where
  hashWithSalt _salt VariableDefinition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData VariableDefinition where
  rnf VariableDefinition' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON VariableDefinition where
  toJSON VariableDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("value" Data..= value)
          ]
      )
