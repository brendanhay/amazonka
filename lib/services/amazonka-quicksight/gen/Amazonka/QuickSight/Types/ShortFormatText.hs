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
-- Module      : Amazonka.QuickSight.Types.ShortFormatText
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ShortFormatText where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The text format for the title.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newShortFormatText' smart constructor.
data ShortFormatText = ShortFormatText'
  { -- | Plain text format.
    plainText :: Prelude.Maybe Prelude.Text,
    -- | Rich text. Examples of rich text include bold, underline, and italics.
    richText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShortFormatText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'plainText', 'shortFormatText_plainText' - Plain text format.
--
-- 'richText', 'shortFormatText_richText' - Rich text. Examples of rich text include bold, underline, and italics.
newShortFormatText ::
  ShortFormatText
newShortFormatText =
  ShortFormatText'
    { plainText = Prelude.Nothing,
      richText = Prelude.Nothing
    }

-- | Plain text format.
shortFormatText_plainText :: Lens.Lens' ShortFormatText (Prelude.Maybe Prelude.Text)
shortFormatText_plainText = Lens.lens (\ShortFormatText' {plainText} -> plainText) (\s@ShortFormatText' {} a -> s {plainText = a} :: ShortFormatText)

-- | Rich text. Examples of rich text include bold, underline, and italics.
shortFormatText_richText :: Lens.Lens' ShortFormatText (Prelude.Maybe Prelude.Text)
shortFormatText_richText = Lens.lens (\ShortFormatText' {richText} -> richText) (\s@ShortFormatText' {} a -> s {richText = a} :: ShortFormatText)

instance Data.FromJSON ShortFormatText where
  parseJSON =
    Data.withObject
      "ShortFormatText"
      ( \x ->
          ShortFormatText'
            Prelude.<$> (x Data..:? "PlainText")
            Prelude.<*> (x Data..:? "RichText")
      )

instance Prelude.Hashable ShortFormatText where
  hashWithSalt _salt ShortFormatText' {..} =
    _salt
      `Prelude.hashWithSalt` plainText
      `Prelude.hashWithSalt` richText

instance Prelude.NFData ShortFormatText where
  rnf ShortFormatText' {..} =
    Prelude.rnf plainText `Prelude.seq`
      Prelude.rnf richText

instance Data.ToJSON ShortFormatText where
  toJSON ShortFormatText' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlainText" Data..=) Prelude.<$> plainText,
            ("RichText" Data..=) Prelude.<$> richText
          ]
      )
