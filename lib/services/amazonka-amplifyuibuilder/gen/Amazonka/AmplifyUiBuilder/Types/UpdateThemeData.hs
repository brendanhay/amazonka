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
-- Module      : Amazonka.AmplifyUiBuilder.Types.UpdateThemeData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.UpdateThemeData where

import Amazonka.AmplifyUiBuilder.Types.ThemeValues
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Saves the data binding information for a theme.
--
-- /See:/ 'newUpdateThemeData' smart constructor.
data UpdateThemeData = UpdateThemeData'
  { -- | The unique ID of the theme to update.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the theme to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | Describes the properties that can be overriden to customize the theme.
    overrides :: Prelude.Maybe [ThemeValues],
    -- | A list of key-value pairs that define the theme\'s properties.
    values :: [ThemeValues]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateThemeData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateThemeData_id' - The unique ID of the theme to update.
--
-- 'name', 'updateThemeData_name' - The name of the theme to update.
--
-- 'overrides', 'updateThemeData_overrides' - Describes the properties that can be overriden to customize the theme.
--
-- 'values', 'updateThemeData_values' - A list of key-value pairs that define the theme\'s properties.
newUpdateThemeData ::
  UpdateThemeData
newUpdateThemeData =
  UpdateThemeData'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      overrides = Prelude.Nothing,
      values = Prelude.mempty
    }

-- | The unique ID of the theme to update.
updateThemeData_id :: Lens.Lens' UpdateThemeData (Prelude.Maybe Prelude.Text)
updateThemeData_id = Lens.lens (\UpdateThemeData' {id} -> id) (\s@UpdateThemeData' {} a -> s {id = a} :: UpdateThemeData)

-- | The name of the theme to update.
updateThemeData_name :: Lens.Lens' UpdateThemeData (Prelude.Maybe Prelude.Text)
updateThemeData_name = Lens.lens (\UpdateThemeData' {name} -> name) (\s@UpdateThemeData' {} a -> s {name = a} :: UpdateThemeData)

-- | Describes the properties that can be overriden to customize the theme.
updateThemeData_overrides :: Lens.Lens' UpdateThemeData (Prelude.Maybe [ThemeValues])
updateThemeData_overrides = Lens.lens (\UpdateThemeData' {overrides} -> overrides) (\s@UpdateThemeData' {} a -> s {overrides = a} :: UpdateThemeData) Prelude.. Lens.mapping Lens.coerced

-- | A list of key-value pairs that define the theme\'s properties.
updateThemeData_values :: Lens.Lens' UpdateThemeData [ThemeValues]
updateThemeData_values = Lens.lens (\UpdateThemeData' {values} -> values) (\s@UpdateThemeData' {} a -> s {values = a} :: UpdateThemeData) Prelude.. Lens.coerced

instance Prelude.Hashable UpdateThemeData where
  hashWithSalt _salt UpdateThemeData' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` values

instance Prelude.NFData UpdateThemeData where
  rnf UpdateThemeData' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON UpdateThemeData where
  toJSON UpdateThemeData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("id" Data..=) Prelude.<$> id,
            ("name" Data..=) Prelude.<$> name,
            ("overrides" Data..=) Prelude.<$> overrides,
            Prelude.Just ("values" Data..= values)
          ]
      )
