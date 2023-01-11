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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ThemeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ThemeValue where

import {-# SOURCE #-} Amazonka.AmplifyUiBuilder.Types.ThemeValues
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of a theme\'s properties.
--
-- /See:/ 'newThemeValue' smart constructor.
data ThemeValue = ThemeValue'
  { -- | A list of key-value pairs that define the theme\'s properties.
    children :: Prelude.Maybe [ThemeValues],
    -- | The value of a theme property.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'children', 'themeValue_children' - A list of key-value pairs that define the theme\'s properties.
--
-- 'value', 'themeValue_value' - The value of a theme property.
newThemeValue ::
  ThemeValue
newThemeValue =
  ThemeValue'
    { children = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A list of key-value pairs that define the theme\'s properties.
themeValue_children :: Lens.Lens' ThemeValue (Prelude.Maybe [ThemeValues])
themeValue_children = Lens.lens (\ThemeValue' {children} -> children) (\s@ThemeValue' {} a -> s {children = a} :: ThemeValue) Prelude.. Lens.mapping Lens.coerced

-- | The value of a theme property.
themeValue_value :: Lens.Lens' ThemeValue (Prelude.Maybe Prelude.Text)
themeValue_value = Lens.lens (\ThemeValue' {value} -> value) (\s@ThemeValue' {} a -> s {value = a} :: ThemeValue)

instance Data.FromJSON ThemeValue where
  parseJSON =
    Data.withObject
      "ThemeValue"
      ( \x ->
          ThemeValue'
            Prelude.<$> (x Data..:? "children" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ThemeValue where
  hashWithSalt _salt ThemeValue' {..} =
    _salt `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` value

instance Prelude.NFData ThemeValue where
  rnf ThemeValue' {..} =
    Prelude.rnf children
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ThemeValue where
  toJSON ThemeValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("children" Data..=) Prelude.<$> children,
            ("value" Data..=) Prelude.<$> value
          ]
      )
