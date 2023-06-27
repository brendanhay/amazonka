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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ThemeValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ThemeValues where

import {-# SOURCE #-} Amazonka.AmplifyUiBuilder.Types.ThemeValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair that defines a property of a theme.
--
-- /See:/ 'newThemeValues' smart constructor.
data ThemeValues = ThemeValues'
  { -- | The name of the property.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the property.
    value :: Prelude.Maybe ThemeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'themeValues_key' - The name of the property.
--
-- 'value', 'themeValues_value' - The value of the property.
newThemeValues ::
  ThemeValues
newThemeValues =
  ThemeValues'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the property.
themeValues_key :: Lens.Lens' ThemeValues (Prelude.Maybe Prelude.Text)
themeValues_key = Lens.lens (\ThemeValues' {key} -> key) (\s@ThemeValues' {} a -> s {key = a} :: ThemeValues)

-- | The value of the property.
themeValues_value :: Lens.Lens' ThemeValues (Prelude.Maybe ThemeValue)
themeValues_value = Lens.lens (\ThemeValues' {value} -> value) (\s@ThemeValues' {} a -> s {value = a} :: ThemeValues)

instance Data.FromJSON ThemeValues where
  parseJSON =
    Data.withObject
      "ThemeValues"
      ( \x ->
          ThemeValues'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ThemeValues where
  hashWithSalt _salt ThemeValues' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ThemeValues where
  rnf ThemeValues' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ThemeValues where
  toJSON ThemeValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )
