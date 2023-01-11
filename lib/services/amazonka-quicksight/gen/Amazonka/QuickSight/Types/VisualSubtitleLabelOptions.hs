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
-- Module      : Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualSubtitleLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LongFormatText
import Amazonka.QuickSight.Types.Visibility

-- | The subtitle label options for a visual.
--
-- /See:/ 'newVisualSubtitleLabelOptions' smart constructor.
data VisualSubtitleLabelOptions = VisualSubtitleLabelOptions'
  { -- | The long text format of the subtitle label, such as plain text or rich
    -- text.
    formatText :: Prelude.Maybe LongFormatText,
    -- | The visibility of the subtitle label.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualSubtitleLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatText', 'visualSubtitleLabelOptions_formatText' - The long text format of the subtitle label, such as plain text or rich
-- text.
--
-- 'visibility', 'visualSubtitleLabelOptions_visibility' - The visibility of the subtitle label.
newVisualSubtitleLabelOptions ::
  VisualSubtitleLabelOptions
newVisualSubtitleLabelOptions =
  VisualSubtitleLabelOptions'
    { formatText =
        Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The long text format of the subtitle label, such as plain text or rich
-- text.
visualSubtitleLabelOptions_formatText :: Lens.Lens' VisualSubtitleLabelOptions (Prelude.Maybe LongFormatText)
visualSubtitleLabelOptions_formatText = Lens.lens (\VisualSubtitleLabelOptions' {formatText} -> formatText) (\s@VisualSubtitleLabelOptions' {} a -> s {formatText = a} :: VisualSubtitleLabelOptions)

-- | The visibility of the subtitle label.
visualSubtitleLabelOptions_visibility :: Lens.Lens' VisualSubtitleLabelOptions (Prelude.Maybe Visibility)
visualSubtitleLabelOptions_visibility = Lens.lens (\VisualSubtitleLabelOptions' {visibility} -> visibility) (\s@VisualSubtitleLabelOptions' {} a -> s {visibility = a} :: VisualSubtitleLabelOptions)

instance Data.FromJSON VisualSubtitleLabelOptions where
  parseJSON =
    Data.withObject
      "VisualSubtitleLabelOptions"
      ( \x ->
          VisualSubtitleLabelOptions'
            Prelude.<$> (x Data..:? "FormatText")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable VisualSubtitleLabelOptions where
  hashWithSalt _salt VisualSubtitleLabelOptions' {..} =
    _salt `Prelude.hashWithSalt` formatText
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData VisualSubtitleLabelOptions where
  rnf VisualSubtitleLabelOptions' {..} =
    Prelude.rnf formatText
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON VisualSubtitleLabelOptions where
  toJSON VisualSubtitleLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatText" Data..=) Prelude.<$> formatText,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
