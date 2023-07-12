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
-- Module      : Amazonka.QuickSight.Types.LayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FreeFormLayoutConfiguration
import Amazonka.QuickSight.Types.GridLayoutConfiguration
import Amazonka.QuickSight.Types.SectionBasedLayoutConfiguration

-- | The configuration that determines what the type of layout will be used
-- on a sheet.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newLayoutConfiguration' smart constructor.
data LayoutConfiguration = LayoutConfiguration'
  { -- | A free-form is optimized for a fixed width and has more control over the
    -- exact placement of layout elements.
    freeFormLayout :: Prelude.Maybe FreeFormLayoutConfiguration,
    -- | A type of layout that can be used on a sheet. In a grid layout, visuals
    -- snap to a grid with standard spacing and alignment. Dashboards are
    -- displayed as designed, with options to fit to screen or view at actual
    -- size. A grid layout can be configured to behave in one of two ways when
    -- the viewport is resized: @FIXED@ or @RESPONSIVE@.
    gridLayout :: Prelude.Maybe GridLayoutConfiguration,
    -- | A section based layout organizes visuals into multiple sections and has
    -- customized header, footer and page break.
    sectionBasedLayout :: Prelude.Maybe SectionBasedLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeFormLayout', 'layoutConfiguration_freeFormLayout' - A free-form is optimized for a fixed width and has more control over the
-- exact placement of layout elements.
--
-- 'gridLayout', 'layoutConfiguration_gridLayout' - A type of layout that can be used on a sheet. In a grid layout, visuals
-- snap to a grid with standard spacing and alignment. Dashboards are
-- displayed as designed, with options to fit to screen or view at actual
-- size. A grid layout can be configured to behave in one of two ways when
-- the viewport is resized: @FIXED@ or @RESPONSIVE@.
--
-- 'sectionBasedLayout', 'layoutConfiguration_sectionBasedLayout' - A section based layout organizes visuals into multiple sections and has
-- customized header, footer and page break.
newLayoutConfiguration ::
  LayoutConfiguration
newLayoutConfiguration =
  LayoutConfiguration'
    { freeFormLayout =
        Prelude.Nothing,
      gridLayout = Prelude.Nothing,
      sectionBasedLayout = Prelude.Nothing
    }

-- | A free-form is optimized for a fixed width and has more control over the
-- exact placement of layout elements.
layoutConfiguration_freeFormLayout :: Lens.Lens' LayoutConfiguration (Prelude.Maybe FreeFormLayoutConfiguration)
layoutConfiguration_freeFormLayout = Lens.lens (\LayoutConfiguration' {freeFormLayout} -> freeFormLayout) (\s@LayoutConfiguration' {} a -> s {freeFormLayout = a} :: LayoutConfiguration)

-- | A type of layout that can be used on a sheet. In a grid layout, visuals
-- snap to a grid with standard spacing and alignment. Dashboards are
-- displayed as designed, with options to fit to screen or view at actual
-- size. A grid layout can be configured to behave in one of two ways when
-- the viewport is resized: @FIXED@ or @RESPONSIVE@.
layoutConfiguration_gridLayout :: Lens.Lens' LayoutConfiguration (Prelude.Maybe GridLayoutConfiguration)
layoutConfiguration_gridLayout = Lens.lens (\LayoutConfiguration' {gridLayout} -> gridLayout) (\s@LayoutConfiguration' {} a -> s {gridLayout = a} :: LayoutConfiguration)

-- | A section based layout organizes visuals into multiple sections and has
-- customized header, footer and page break.
layoutConfiguration_sectionBasedLayout :: Lens.Lens' LayoutConfiguration (Prelude.Maybe SectionBasedLayoutConfiguration)
layoutConfiguration_sectionBasedLayout = Lens.lens (\LayoutConfiguration' {sectionBasedLayout} -> sectionBasedLayout) (\s@LayoutConfiguration' {} a -> s {sectionBasedLayout = a} :: LayoutConfiguration)

instance Data.FromJSON LayoutConfiguration where
  parseJSON =
    Data.withObject
      "LayoutConfiguration"
      ( \x ->
          LayoutConfiguration'
            Prelude.<$> (x Data..:? "FreeFormLayout")
            Prelude.<*> (x Data..:? "GridLayout")
            Prelude.<*> (x Data..:? "SectionBasedLayout")
      )

instance Prelude.Hashable LayoutConfiguration where
  hashWithSalt _salt LayoutConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` freeFormLayout
      `Prelude.hashWithSalt` gridLayout
      `Prelude.hashWithSalt` sectionBasedLayout

instance Prelude.NFData LayoutConfiguration where
  rnf LayoutConfiguration' {..} =
    Prelude.rnf freeFormLayout
      `Prelude.seq` Prelude.rnf gridLayout
      `Prelude.seq` Prelude.rnf sectionBasedLayout

instance Data.ToJSON LayoutConfiguration where
  toJSON LayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FreeFormLayout" Data..=)
              Prelude.<$> freeFormLayout,
            ("GridLayout" Data..=) Prelude.<$> gridLayout,
            ("SectionBasedLayout" Data..=)
              Prelude.<$> sectionBasedLayout
          ]
      )
