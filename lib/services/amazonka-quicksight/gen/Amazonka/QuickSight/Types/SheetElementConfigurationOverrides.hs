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
-- Module      : Amazonka.QuickSight.Types.SheetElementConfigurationOverrides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetElementConfigurationOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The override configuration of the rendering rules of a sheet.
--
-- /See:/ 'newSheetElementConfigurationOverrides' smart constructor.
data SheetElementConfigurationOverrides = SheetElementConfigurationOverrides'
  { -- | Determines whether or not the overrides are visible. Choose one of the
    -- following options:
    --
    -- -   @VISIBLE@
    --
    -- -   @HIDDEN@
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetElementConfigurationOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'sheetElementConfigurationOverrides_visibility' - Determines whether or not the overrides are visible. Choose one of the
-- following options:
--
-- -   @VISIBLE@
--
-- -   @HIDDEN@
newSheetElementConfigurationOverrides ::
  SheetElementConfigurationOverrides
newSheetElementConfigurationOverrides =
  SheetElementConfigurationOverrides'
    { visibility =
        Prelude.Nothing
    }

-- | Determines whether or not the overrides are visible. Choose one of the
-- following options:
--
-- -   @VISIBLE@
--
-- -   @HIDDEN@
sheetElementConfigurationOverrides_visibility :: Lens.Lens' SheetElementConfigurationOverrides (Prelude.Maybe Visibility)
sheetElementConfigurationOverrides_visibility = Lens.lens (\SheetElementConfigurationOverrides' {visibility} -> visibility) (\s@SheetElementConfigurationOverrides' {} a -> s {visibility = a} :: SheetElementConfigurationOverrides)

instance
  Data.FromJSON
    SheetElementConfigurationOverrides
  where
  parseJSON =
    Data.withObject
      "SheetElementConfigurationOverrides"
      ( \x ->
          SheetElementConfigurationOverrides'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance
  Prelude.Hashable
    SheetElementConfigurationOverrides
  where
  hashWithSalt
    _salt
    SheetElementConfigurationOverrides' {..} =
      _salt `Prelude.hashWithSalt` visibility

instance
  Prelude.NFData
    SheetElementConfigurationOverrides
  where
  rnf SheetElementConfigurationOverrides' {..} =
    Prelude.rnf visibility

instance
  Data.ToJSON
    SheetElementConfigurationOverrides
  where
  toJSON SheetElementConfigurationOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
