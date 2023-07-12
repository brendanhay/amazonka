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
-- Module      : Amazonka.IoTEventsData.Types.Variable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.Variable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current state of the variable.
--
-- /See:/ 'newVariable' smart constructor.
data Variable = Variable'
  { -- | The name of the variable.
    name :: Prelude.Text,
    -- | The current value of the variable.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Variable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'variable_name' - The name of the variable.
--
-- 'value', 'variable_value' - The current value of the variable.
newVariable ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Variable
newVariable pName_ pValue_ =
  Variable' {name = pName_, value = pValue_}

-- | The name of the variable.
variable_name :: Lens.Lens' Variable Prelude.Text
variable_name = Lens.lens (\Variable' {name} -> name) (\s@Variable' {} a -> s {name = a} :: Variable)

-- | The current value of the variable.
variable_value :: Lens.Lens' Variable Prelude.Text
variable_value = Lens.lens (\Variable' {value} -> value) (\s@Variable' {} a -> s {value = a} :: Variable)

instance Data.FromJSON Variable where
  parseJSON =
    Data.withObject
      "Variable"
      ( \x ->
          Variable'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable Variable where
  hashWithSalt _salt Variable' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Variable where
  rnf Variable' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
