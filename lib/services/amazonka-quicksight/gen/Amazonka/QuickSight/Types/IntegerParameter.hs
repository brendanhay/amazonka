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
-- Module      : Amazonka.QuickSight.Types.IntegerParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IntegerParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An integer parameter.
--
-- /See:/ 'newIntegerParameter' smart constructor.
data IntegerParameter = IntegerParameter'
  { -- | The name of the integer parameter.
    name :: Prelude.Text,
    -- | The values for the integer parameter.
    values :: [Data.Sensitive Prelude.Integer]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'integerParameter_name' - The name of the integer parameter.
--
-- 'values', 'integerParameter_values' - The values for the integer parameter.
newIntegerParameter ::
  -- | 'name'
  Prelude.Text ->
  IntegerParameter
newIntegerParameter pName_ =
  IntegerParameter'
    { name = pName_,
      values = Prelude.mempty
    }

-- | The name of the integer parameter.
integerParameter_name :: Lens.Lens' IntegerParameter Prelude.Text
integerParameter_name = Lens.lens (\IntegerParameter' {name} -> name) (\s@IntegerParameter' {} a -> s {name = a} :: IntegerParameter)

-- | The values for the integer parameter.
integerParameter_values :: Lens.Lens' IntegerParameter [Prelude.Integer]
integerParameter_values = Lens.lens (\IntegerParameter' {values} -> values) (\s@IntegerParameter' {} a -> s {values = a} :: IntegerParameter) Prelude.. Lens.coerced

instance Prelude.Hashable IntegerParameter where
  hashWithSalt _salt IntegerParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData IntegerParameter where
  rnf IntegerParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON IntegerParameter where
  toJSON IntegerParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
