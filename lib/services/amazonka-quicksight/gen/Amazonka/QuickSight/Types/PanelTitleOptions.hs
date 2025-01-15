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
-- Module      : Amazonka.QuickSight.Types.PanelTitleOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PanelTitleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.HorizontalTextAlignment
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine the title styles for each small multiples
-- panel.
--
-- /See:/ 'newPanelTitleOptions' smart constructor.
data PanelTitleOptions = PanelTitleOptions'
  { fontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | Sets the horizontal text alignment of the title within each panel.
    horizontalTextAlignment :: Prelude.Maybe HorizontalTextAlignment,
    -- | Determines whether or not panel titles are displayed.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PanelTitleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fontConfiguration', 'panelTitleOptions_fontConfiguration' - Undocumented member.
--
-- 'horizontalTextAlignment', 'panelTitleOptions_horizontalTextAlignment' - Sets the horizontal text alignment of the title within each panel.
--
-- 'visibility', 'panelTitleOptions_visibility' - Determines whether or not panel titles are displayed.
newPanelTitleOptions ::
  PanelTitleOptions
newPanelTitleOptions =
  PanelTitleOptions'
    { fontConfiguration =
        Prelude.Nothing,
      horizontalTextAlignment = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | Undocumented member.
panelTitleOptions_fontConfiguration :: Lens.Lens' PanelTitleOptions (Prelude.Maybe FontConfiguration)
panelTitleOptions_fontConfiguration = Lens.lens (\PanelTitleOptions' {fontConfiguration} -> fontConfiguration) (\s@PanelTitleOptions' {} a -> s {fontConfiguration = a} :: PanelTitleOptions)

-- | Sets the horizontal text alignment of the title within each panel.
panelTitleOptions_horizontalTextAlignment :: Lens.Lens' PanelTitleOptions (Prelude.Maybe HorizontalTextAlignment)
panelTitleOptions_horizontalTextAlignment = Lens.lens (\PanelTitleOptions' {horizontalTextAlignment} -> horizontalTextAlignment) (\s@PanelTitleOptions' {} a -> s {horizontalTextAlignment = a} :: PanelTitleOptions)

-- | Determines whether or not panel titles are displayed.
panelTitleOptions_visibility :: Lens.Lens' PanelTitleOptions (Prelude.Maybe Visibility)
panelTitleOptions_visibility = Lens.lens (\PanelTitleOptions' {visibility} -> visibility) (\s@PanelTitleOptions' {} a -> s {visibility = a} :: PanelTitleOptions)

instance Data.FromJSON PanelTitleOptions where
  parseJSON =
    Data.withObject
      "PanelTitleOptions"
      ( \x ->
          PanelTitleOptions'
            Prelude.<$> (x Data..:? "FontConfiguration")
            Prelude.<*> (x Data..:? "HorizontalTextAlignment")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable PanelTitleOptions where
  hashWithSalt _salt PanelTitleOptions' {..} =
    _salt
      `Prelude.hashWithSalt` fontConfiguration
      `Prelude.hashWithSalt` horizontalTextAlignment
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData PanelTitleOptions where
  rnf PanelTitleOptions' {..} =
    Prelude.rnf fontConfiguration `Prelude.seq`
      Prelude.rnf horizontalTextAlignment `Prelude.seq`
        Prelude.rnf visibility

instance Data.ToJSON PanelTitleOptions where
  toJSON PanelTitleOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FontConfiguration" Data..=)
              Prelude.<$> fontConfiguration,
            ("HorizontalTextAlignment" Data..=)
              Prelude.<$> horizontalTextAlignment,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
