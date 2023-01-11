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
-- Module      : Amazonka.QuickSight.Types.SheetTextBox
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetTextBox where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A text box.
--
-- /See:/ 'newSheetTextBox' smart constructor.
data SheetTextBox = SheetTextBox'
  { -- | The content that is displayed in the text box.
    content :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a text box. This identifier must be unique
    -- within the context of a dashboard, template, or analysis. Two
    -- dashboards, analyses, or templates can have text boxes that share
    -- identifiers.
    sheetTextBoxId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetTextBox' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'sheetTextBox_content' - The content that is displayed in the text box.
--
-- 'sheetTextBoxId', 'sheetTextBox_sheetTextBoxId' - The unique identifier for a text box. This identifier must be unique
-- within the context of a dashboard, template, or analysis. Two
-- dashboards, analyses, or templates can have text boxes that share
-- identifiers.
newSheetTextBox ::
  -- | 'sheetTextBoxId'
  Prelude.Text ->
  SheetTextBox
newSheetTextBox pSheetTextBoxId_ =
  SheetTextBox'
    { content = Prelude.Nothing,
      sheetTextBoxId = pSheetTextBoxId_
    }

-- | The content that is displayed in the text box.
sheetTextBox_content :: Lens.Lens' SheetTextBox (Prelude.Maybe Prelude.Text)
sheetTextBox_content = Lens.lens (\SheetTextBox' {content} -> content) (\s@SheetTextBox' {} a -> s {content = a} :: SheetTextBox)

-- | The unique identifier for a text box. This identifier must be unique
-- within the context of a dashboard, template, or analysis. Two
-- dashboards, analyses, or templates can have text boxes that share
-- identifiers.
sheetTextBox_sheetTextBoxId :: Lens.Lens' SheetTextBox Prelude.Text
sheetTextBox_sheetTextBoxId = Lens.lens (\SheetTextBox' {sheetTextBoxId} -> sheetTextBoxId) (\s@SheetTextBox' {} a -> s {sheetTextBoxId = a} :: SheetTextBox)

instance Data.FromJSON SheetTextBox where
  parseJSON =
    Data.withObject
      "SheetTextBox"
      ( \x ->
          SheetTextBox'
            Prelude.<$> (x Data..:? "Content")
            Prelude.<*> (x Data..: "SheetTextBoxId")
      )

instance Prelude.Hashable SheetTextBox where
  hashWithSalt _salt SheetTextBox' {..} =
    _salt `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` sheetTextBoxId

instance Prelude.NFData SheetTextBox where
  rnf SheetTextBox' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf sheetTextBoxId

instance Data.ToJSON SheetTextBox where
  toJSON SheetTextBox' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Content" Data..=) Prelude.<$> content,
            Prelude.Just
              ("SheetTextBoxId" Data..= sheetTextBoxId)
          ]
      )
