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
-- Module      : Amazonka.QuickSight.Types.VisualTitleLabelOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualTitleLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ShortFormatText
import Amazonka.QuickSight.Types.Visibility

-- | The title label options for a visual.
--
-- /See:/ 'newVisualTitleLabelOptions' smart constructor.
data VisualTitleLabelOptions = VisualTitleLabelOptions'
  { -- | The short text format of the title label, such as plain text or rich
    -- text.
    formatText :: Prelude.Maybe ShortFormatText,
    -- | The visibility of the title label.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualTitleLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatText', 'visualTitleLabelOptions_formatText' - The short text format of the title label, such as plain text or rich
-- text.
--
-- 'visibility', 'visualTitleLabelOptions_visibility' - The visibility of the title label.
newVisualTitleLabelOptions ::
  VisualTitleLabelOptions
newVisualTitleLabelOptions =
  VisualTitleLabelOptions'
    { formatText =
        Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The short text format of the title label, such as plain text or rich
-- text.
visualTitleLabelOptions_formatText :: Lens.Lens' VisualTitleLabelOptions (Prelude.Maybe ShortFormatText)
visualTitleLabelOptions_formatText = Lens.lens (\VisualTitleLabelOptions' {formatText} -> formatText) (\s@VisualTitleLabelOptions' {} a -> s {formatText = a} :: VisualTitleLabelOptions)

-- | The visibility of the title label.
visualTitleLabelOptions_visibility :: Lens.Lens' VisualTitleLabelOptions (Prelude.Maybe Visibility)
visualTitleLabelOptions_visibility = Lens.lens (\VisualTitleLabelOptions' {visibility} -> visibility) (\s@VisualTitleLabelOptions' {} a -> s {visibility = a} :: VisualTitleLabelOptions)

instance Data.FromJSON VisualTitleLabelOptions where
  parseJSON =
    Data.withObject
      "VisualTitleLabelOptions"
      ( \x ->
          VisualTitleLabelOptions'
            Prelude.<$> (x Data..:? "FormatText")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable VisualTitleLabelOptions where
  hashWithSalt _salt VisualTitleLabelOptions' {..} =
    _salt `Prelude.hashWithSalt` formatText
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData VisualTitleLabelOptions where
  rnf VisualTitleLabelOptions' {..} =
    Prelude.rnf formatText
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON VisualTitleLabelOptions where
  toJSON VisualTitleLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatText" Data..=) Prelude.<$> formatText,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
