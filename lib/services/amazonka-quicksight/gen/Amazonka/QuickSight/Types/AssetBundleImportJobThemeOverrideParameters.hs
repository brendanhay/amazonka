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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportJobThemeOverrideParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportJobThemeOverrideParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The override parameters for a single theme that is imported.
--
-- /See:/ 'newAssetBundleImportJobThemeOverrideParameters' smart constructor.
data AssetBundleImportJobThemeOverrideParameters = AssetBundleImportJobThemeOverrideParameters'
  { -- | A new name for the theme.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the theme to apply overrides to.
    themeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportJobThemeOverrideParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assetBundleImportJobThemeOverrideParameters_name' - A new name for the theme.
--
-- 'themeId', 'assetBundleImportJobThemeOverrideParameters_themeId' - The ID of the theme to apply overrides to.
newAssetBundleImportJobThemeOverrideParameters ::
  -- | 'themeId'
  Prelude.Text ->
  AssetBundleImportJobThemeOverrideParameters
newAssetBundleImportJobThemeOverrideParameters
  pThemeId_ =
    AssetBundleImportJobThemeOverrideParameters'
      { name =
          Prelude.Nothing,
        themeId = pThemeId_
      }

-- | A new name for the theme.
assetBundleImportJobThemeOverrideParameters_name :: Lens.Lens' AssetBundleImportJobThemeOverrideParameters (Prelude.Maybe Prelude.Text)
assetBundleImportJobThemeOverrideParameters_name = Lens.lens (\AssetBundleImportJobThemeOverrideParameters' {name} -> name) (\s@AssetBundleImportJobThemeOverrideParameters' {} a -> s {name = a} :: AssetBundleImportJobThemeOverrideParameters)

-- | The ID of the theme to apply overrides to.
assetBundleImportJobThemeOverrideParameters_themeId :: Lens.Lens' AssetBundleImportJobThemeOverrideParameters Prelude.Text
assetBundleImportJobThemeOverrideParameters_themeId = Lens.lens (\AssetBundleImportJobThemeOverrideParameters' {themeId} -> themeId) (\s@AssetBundleImportJobThemeOverrideParameters' {} a -> s {themeId = a} :: AssetBundleImportJobThemeOverrideParameters)

instance
  Data.FromJSON
    AssetBundleImportJobThemeOverrideParameters
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportJobThemeOverrideParameters"
      ( \x ->
          AssetBundleImportJobThemeOverrideParameters'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "ThemeId")
      )

instance
  Prelude.Hashable
    AssetBundleImportJobThemeOverrideParameters
  where
  hashWithSalt
    _salt
    AssetBundleImportJobThemeOverrideParameters' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` themeId

instance
  Prelude.NFData
    AssetBundleImportJobThemeOverrideParameters
  where
  rnf AssetBundleImportJobThemeOverrideParameters' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf themeId

instance
  Data.ToJSON
    AssetBundleImportJobThemeOverrideParameters
  where
  toJSON
    AssetBundleImportJobThemeOverrideParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Name" Data..=) Prelude.<$> name,
              Prelude.Just ("ThemeId" Data..= themeId)
            ]
        )
