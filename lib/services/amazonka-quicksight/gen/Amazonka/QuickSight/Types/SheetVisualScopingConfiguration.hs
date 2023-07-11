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
-- Module      : Amazonka.QuickSight.Types.SheetVisualScopingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetVisualScopingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterVisualScope

-- | The filter that is applied to the options.
--
-- /See:/ 'newSheetVisualScopingConfiguration' smart constructor.
data SheetVisualScopingConfiguration = SheetVisualScopingConfiguration'
  { -- | The selected visuals that the filter is applied to.
    visualIds :: Prelude.Maybe [Prelude.Text],
    -- | The selected sheet that the filter is applied to.
    sheetId :: Prelude.Text,
    -- | The scope of the applied entities. Choose one of the following options:
    --
    -- -   @ALL_VISUALS@
    --
    -- -   @SELECTED_VISUALS@
    scope :: FilterVisualScope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetVisualScopingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visualIds', 'sheetVisualScopingConfiguration_visualIds' - The selected visuals that the filter is applied to.
--
-- 'sheetId', 'sheetVisualScopingConfiguration_sheetId' - The selected sheet that the filter is applied to.
--
-- 'scope', 'sheetVisualScopingConfiguration_scope' - The scope of the applied entities. Choose one of the following options:
--
-- -   @ALL_VISUALS@
--
-- -   @SELECTED_VISUALS@
newSheetVisualScopingConfiguration ::
  -- | 'sheetId'
  Prelude.Text ->
  -- | 'scope'
  FilterVisualScope ->
  SheetVisualScopingConfiguration
newSheetVisualScopingConfiguration pSheetId_ pScope_ =
  SheetVisualScopingConfiguration'
    { visualIds =
        Prelude.Nothing,
      sheetId = pSheetId_,
      scope = pScope_
    }

-- | The selected visuals that the filter is applied to.
sheetVisualScopingConfiguration_visualIds :: Lens.Lens' SheetVisualScopingConfiguration (Prelude.Maybe [Prelude.Text])
sheetVisualScopingConfiguration_visualIds = Lens.lens (\SheetVisualScopingConfiguration' {visualIds} -> visualIds) (\s@SheetVisualScopingConfiguration' {} a -> s {visualIds = a} :: SheetVisualScopingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The selected sheet that the filter is applied to.
sheetVisualScopingConfiguration_sheetId :: Lens.Lens' SheetVisualScopingConfiguration Prelude.Text
sheetVisualScopingConfiguration_sheetId = Lens.lens (\SheetVisualScopingConfiguration' {sheetId} -> sheetId) (\s@SheetVisualScopingConfiguration' {} a -> s {sheetId = a} :: SheetVisualScopingConfiguration)

-- | The scope of the applied entities. Choose one of the following options:
--
-- -   @ALL_VISUALS@
--
-- -   @SELECTED_VISUALS@
sheetVisualScopingConfiguration_scope :: Lens.Lens' SheetVisualScopingConfiguration FilterVisualScope
sheetVisualScopingConfiguration_scope = Lens.lens (\SheetVisualScopingConfiguration' {scope} -> scope) (\s@SheetVisualScopingConfiguration' {} a -> s {scope = a} :: SheetVisualScopingConfiguration)

instance
  Data.FromJSON
    SheetVisualScopingConfiguration
  where
  parseJSON =
    Data.withObject
      "SheetVisualScopingConfiguration"
      ( \x ->
          SheetVisualScopingConfiguration'
            Prelude.<$> (x Data..:? "VisualIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "SheetId")
            Prelude.<*> (x Data..: "Scope")
      )

instance
  Prelude.Hashable
    SheetVisualScopingConfiguration
  where
  hashWithSalt
    _salt
    SheetVisualScopingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` visualIds
        `Prelude.hashWithSalt` sheetId
        `Prelude.hashWithSalt` scope

instance
  Prelude.NFData
    SheetVisualScopingConfiguration
  where
  rnf SheetVisualScopingConfiguration' {..} =
    Prelude.rnf visualIds
      `Prelude.seq` Prelude.rnf sheetId
      `Prelude.seq` Prelude.rnf scope

instance Data.ToJSON SheetVisualScopingConfiguration where
  toJSON SheetVisualScopingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VisualIds" Data..=) Prelude.<$> visualIds,
            Prelude.Just ("SheetId" Data..= sheetId),
            Prelude.Just ("Scope" Data..= scope)
          ]
      )
