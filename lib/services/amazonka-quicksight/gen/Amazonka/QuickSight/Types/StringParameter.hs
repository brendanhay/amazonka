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
-- Module      : Amazonka.QuickSight.Types.StringParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.StringParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A string parameter.
--
-- /See:/ 'newStringParameter' smart constructor.
data StringParameter = StringParameter'
  { -- | A display name for a string parameter.
    name :: Prelude.Text,
    -- | The values of a string parameter.
    values :: [Data.Sensitive Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stringParameter_name' - A display name for a string parameter.
--
-- 'values', 'stringParameter_values' - The values of a string parameter.
newStringParameter ::
  -- | 'name'
  Prelude.Text ->
  StringParameter
newStringParameter pName_ =
  StringParameter'
    { name = pName_,
      values = Prelude.mempty
    }

-- | A display name for a string parameter.
stringParameter_name :: Lens.Lens' StringParameter Prelude.Text
stringParameter_name = Lens.lens (\StringParameter' {name} -> name) (\s@StringParameter' {} a -> s {name = a} :: StringParameter)

-- | The values of a string parameter.
stringParameter_values :: Lens.Lens' StringParameter [Prelude.Text]
stringParameter_values = Lens.lens (\StringParameter' {values} -> values) (\s@StringParameter' {} a -> s {values = a} :: StringParameter) Prelude.. Lens.coerced

instance Prelude.Hashable StringParameter where
  hashWithSalt _salt StringParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData StringParameter where
  rnf StringParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON StringParameter where
  toJSON StringParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
