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
-- Module      : Amazonka.AppIntegrationS.Types.FileConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppIntegrationS.Types.FileConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for what files should be pulled from the source.
--
-- /See:/ 'newFileConfiguration' smart constructor.
data FileConfiguration = FileConfiguration'
  { -- | Restrictions for what files should be pulled from the source.
    filters :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | Identifiers for the source folders to pull all files from recursively.
    folders :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'fileConfiguration_filters' - Restrictions for what files should be pulled from the source.
--
-- 'folders', 'fileConfiguration_folders' - Identifiers for the source folders to pull all files from recursively.
newFileConfiguration ::
  -- | 'folders'
  Prelude.NonEmpty Prelude.Text ->
  FileConfiguration
newFileConfiguration pFolders_ =
  FileConfiguration'
    { filters = Prelude.Nothing,
      folders = Lens.coerced Lens.# pFolders_
    }

-- | Restrictions for what files should be pulled from the source.
fileConfiguration_filters :: Lens.Lens' FileConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
fileConfiguration_filters = Lens.lens (\FileConfiguration' {filters} -> filters) (\s@FileConfiguration' {} a -> s {filters = a} :: FileConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Identifiers for the source folders to pull all files from recursively.
fileConfiguration_folders :: Lens.Lens' FileConfiguration (Prelude.NonEmpty Prelude.Text)
fileConfiguration_folders = Lens.lens (\FileConfiguration' {folders} -> folders) (\s@FileConfiguration' {} a -> s {folders = a} :: FileConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON FileConfiguration where
  parseJSON =
    Data.withObject
      "FileConfiguration"
      ( \x ->
          FileConfiguration'
            Prelude.<$> (x Data..:? "Filters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Folders")
      )

instance Prelude.Hashable FileConfiguration where
  hashWithSalt _salt FileConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` folders

instance Prelude.NFData FileConfiguration where
  rnf FileConfiguration' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf folders

instance Data.ToJSON FileConfiguration where
  toJSON FileConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            Prelude.Just ("Folders" Data..= folders)
          ]
      )
