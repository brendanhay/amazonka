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
-- Module      : Amazonka.QuickSight.Types.LabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.Visibility

-- | The share label options for the labels.
--
-- /See:/ 'newLabelOptions' smart constructor.
data LabelOptions = LabelOptions'
  { -- | The text for the label.
    customLabel :: Prelude.Maybe Prelude.Text,
    -- | The font configuration of the label.
    fontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | Determines whether or not the label is visible.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customLabel', 'labelOptions_customLabel' - The text for the label.
--
-- 'fontConfiguration', 'labelOptions_fontConfiguration' - The font configuration of the label.
--
-- 'visibility', 'labelOptions_visibility' - Determines whether or not the label is visible.
newLabelOptions ::
  LabelOptions
newLabelOptions =
  LabelOptions'
    { customLabel = Prelude.Nothing,
      fontConfiguration = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The text for the label.
labelOptions_customLabel :: Lens.Lens' LabelOptions (Prelude.Maybe Prelude.Text)
labelOptions_customLabel = Lens.lens (\LabelOptions' {customLabel} -> customLabel) (\s@LabelOptions' {} a -> s {customLabel = a} :: LabelOptions)

-- | The font configuration of the label.
labelOptions_fontConfiguration :: Lens.Lens' LabelOptions (Prelude.Maybe FontConfiguration)
labelOptions_fontConfiguration = Lens.lens (\LabelOptions' {fontConfiguration} -> fontConfiguration) (\s@LabelOptions' {} a -> s {fontConfiguration = a} :: LabelOptions)

-- | Determines whether or not the label is visible.
labelOptions_visibility :: Lens.Lens' LabelOptions (Prelude.Maybe Visibility)
labelOptions_visibility = Lens.lens (\LabelOptions' {visibility} -> visibility) (\s@LabelOptions' {} a -> s {visibility = a} :: LabelOptions)

instance Data.FromJSON LabelOptions where
  parseJSON =
    Data.withObject
      "LabelOptions"
      ( \x ->
          LabelOptions'
            Prelude.<$> (x Data..:? "CustomLabel")
            Prelude.<*> (x Data..:? "FontConfiguration")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable LabelOptions where
  hashWithSalt _salt LabelOptions' {..} =
    _salt `Prelude.hashWithSalt` customLabel
      `Prelude.hashWithSalt` fontConfiguration
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData LabelOptions where
  rnf LabelOptions' {..} =
    Prelude.rnf customLabel
      `Prelude.seq` Prelude.rnf fontConfiguration
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON LabelOptions where
  toJSON LabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomLabel" Data..=) Prelude.<$> customLabel,
            ("FontConfiguration" Data..=)
              Prelude.<$> fontConfiguration,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
