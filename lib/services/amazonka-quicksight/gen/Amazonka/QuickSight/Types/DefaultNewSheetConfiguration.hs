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
-- Module      : Amazonka.QuickSight.Types.DefaultNewSheetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultNewSheetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DefaultInteractiveLayoutConfiguration
import Amazonka.QuickSight.Types.DefaultPaginatedLayoutConfiguration
import Amazonka.QuickSight.Types.SheetContentType

-- | The configuration for default new sheet settings.
--
-- /See:/ 'newDefaultNewSheetConfiguration' smart constructor.
data DefaultNewSheetConfiguration = DefaultNewSheetConfiguration'
  { -- | The options that determine the default settings for interactive layout
    -- configuration.
    interactiveLayoutConfiguration :: Prelude.Maybe DefaultInteractiveLayoutConfiguration,
    -- | The options that determine the default settings for a paginated layout
    -- configuration.
    paginatedLayoutConfiguration :: Prelude.Maybe DefaultPaginatedLayoutConfiguration,
    -- | The option that determines the sheet content type.
    sheetContentType :: Prelude.Maybe SheetContentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultNewSheetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interactiveLayoutConfiguration', 'defaultNewSheetConfiguration_interactiveLayoutConfiguration' - The options that determine the default settings for interactive layout
-- configuration.
--
-- 'paginatedLayoutConfiguration', 'defaultNewSheetConfiguration_paginatedLayoutConfiguration' - The options that determine the default settings for a paginated layout
-- configuration.
--
-- 'sheetContentType', 'defaultNewSheetConfiguration_sheetContentType' - The option that determines the sheet content type.
newDefaultNewSheetConfiguration ::
  DefaultNewSheetConfiguration
newDefaultNewSheetConfiguration =
  DefaultNewSheetConfiguration'
    { interactiveLayoutConfiguration =
        Prelude.Nothing,
      paginatedLayoutConfiguration =
        Prelude.Nothing,
      sheetContentType = Prelude.Nothing
    }

-- | The options that determine the default settings for interactive layout
-- configuration.
defaultNewSheetConfiguration_interactiveLayoutConfiguration :: Lens.Lens' DefaultNewSheetConfiguration (Prelude.Maybe DefaultInteractiveLayoutConfiguration)
defaultNewSheetConfiguration_interactiveLayoutConfiguration = Lens.lens (\DefaultNewSheetConfiguration' {interactiveLayoutConfiguration} -> interactiveLayoutConfiguration) (\s@DefaultNewSheetConfiguration' {} a -> s {interactiveLayoutConfiguration = a} :: DefaultNewSheetConfiguration)

-- | The options that determine the default settings for a paginated layout
-- configuration.
defaultNewSheetConfiguration_paginatedLayoutConfiguration :: Lens.Lens' DefaultNewSheetConfiguration (Prelude.Maybe DefaultPaginatedLayoutConfiguration)
defaultNewSheetConfiguration_paginatedLayoutConfiguration = Lens.lens (\DefaultNewSheetConfiguration' {paginatedLayoutConfiguration} -> paginatedLayoutConfiguration) (\s@DefaultNewSheetConfiguration' {} a -> s {paginatedLayoutConfiguration = a} :: DefaultNewSheetConfiguration)

-- | The option that determines the sheet content type.
defaultNewSheetConfiguration_sheetContentType :: Lens.Lens' DefaultNewSheetConfiguration (Prelude.Maybe SheetContentType)
defaultNewSheetConfiguration_sheetContentType = Lens.lens (\DefaultNewSheetConfiguration' {sheetContentType} -> sheetContentType) (\s@DefaultNewSheetConfiguration' {} a -> s {sheetContentType = a} :: DefaultNewSheetConfiguration)

instance Data.FromJSON DefaultNewSheetConfiguration where
  parseJSON =
    Data.withObject
      "DefaultNewSheetConfiguration"
      ( \x ->
          DefaultNewSheetConfiguration'
            Prelude.<$> (x Data..:? "InteractiveLayoutConfiguration")
            Prelude.<*> (x Data..:? "PaginatedLayoutConfiguration")
            Prelude.<*> (x Data..:? "SheetContentType")
      )

instance
  Prelude.Hashable
    DefaultNewSheetConfiguration
  where
  hashWithSalt _salt DefaultNewSheetConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` interactiveLayoutConfiguration
      `Prelude.hashWithSalt` paginatedLayoutConfiguration
      `Prelude.hashWithSalt` sheetContentType

instance Prelude.NFData DefaultNewSheetConfiguration where
  rnf DefaultNewSheetConfiguration' {..} =
    Prelude.rnf interactiveLayoutConfiguration `Prelude.seq`
      Prelude.rnf paginatedLayoutConfiguration `Prelude.seq`
        Prelude.rnf sheetContentType

instance Data.ToJSON DefaultNewSheetConfiguration where
  toJSON DefaultNewSheetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InteractiveLayoutConfiguration" Data..=)
              Prelude.<$> interactiveLayoutConfiguration,
            ("PaginatedLayoutConfiguration" Data..=)
              Prelude.<$> paginatedLayoutConfiguration,
            ("SheetContentType" Data..=)
              Prelude.<$> sheetContentType
          ]
      )
