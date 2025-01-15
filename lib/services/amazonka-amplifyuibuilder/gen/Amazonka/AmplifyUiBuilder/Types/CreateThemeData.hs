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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CreateThemeData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CreateThemeData where

import Amazonka.AmplifyUiBuilder.Types.ThemeValues
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents all of the information that is required to create a theme.
--
-- /See:/ 'newCreateThemeData' smart constructor.
data CreateThemeData = CreateThemeData'
  { -- | Describes the properties that can be overriden to customize an instance
    -- of the theme.
    overrides :: Prelude.Maybe [ThemeValues],
    -- | One or more key-value pairs to use when tagging the theme data.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the theme.
    name :: Prelude.Text,
    -- | A list of key-value pairs that deﬁnes the properties of the theme.
    values :: [ThemeValues]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThemeData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'createThemeData_overrides' - Describes the properties that can be overriden to customize an instance
-- of the theme.
--
-- 'tags', 'createThemeData_tags' - One or more key-value pairs to use when tagging the theme data.
--
-- 'name', 'createThemeData_name' - The name of the theme.
--
-- 'values', 'createThemeData_values' - A list of key-value pairs that deﬁnes the properties of the theme.
newCreateThemeData ::
  -- | 'name'
  Prelude.Text ->
  CreateThemeData
newCreateThemeData pName_ =
  CreateThemeData'
    { overrides = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      values = Prelude.mempty
    }

-- | Describes the properties that can be overriden to customize an instance
-- of the theme.
createThemeData_overrides :: Lens.Lens' CreateThemeData (Prelude.Maybe [ThemeValues])
createThemeData_overrides = Lens.lens (\CreateThemeData' {overrides} -> overrides) (\s@CreateThemeData' {} a -> s {overrides = a} :: CreateThemeData) Prelude.. Lens.mapping Lens.coerced

-- | One or more key-value pairs to use when tagging the theme data.
createThemeData_tags :: Lens.Lens' CreateThemeData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createThemeData_tags = Lens.lens (\CreateThemeData' {tags} -> tags) (\s@CreateThemeData' {} a -> s {tags = a} :: CreateThemeData) Prelude.. Lens.mapping Lens.coerced

-- | The name of the theme.
createThemeData_name :: Lens.Lens' CreateThemeData Prelude.Text
createThemeData_name = Lens.lens (\CreateThemeData' {name} -> name) (\s@CreateThemeData' {} a -> s {name = a} :: CreateThemeData)

-- | A list of key-value pairs that deﬁnes the properties of the theme.
createThemeData_values :: Lens.Lens' CreateThemeData [ThemeValues]
createThemeData_values = Lens.lens (\CreateThemeData' {values} -> values) (\s@CreateThemeData' {} a -> s {values = a} :: CreateThemeData) Prelude.. Lens.coerced

instance Prelude.Hashable CreateThemeData where
  hashWithSalt _salt CreateThemeData' {..} =
    _salt
      `Prelude.hashWithSalt` overrides
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData CreateThemeData where
  rnf CreateThemeData' {..} =
    Prelude.rnf overrides `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf values

instance Data.ToJSON CreateThemeData where
  toJSON CreateThemeData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("overrides" Data..=) Prelude.<$> overrides,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values)
          ]
      )
