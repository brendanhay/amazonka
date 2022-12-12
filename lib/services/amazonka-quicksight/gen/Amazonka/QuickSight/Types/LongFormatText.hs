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
-- Module      : Amazonka.QuickSight.Types.LongFormatText
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LongFormatText where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The text format for a subtitle.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newLongFormatText' smart constructor.
data LongFormatText = LongFormatText'
  { -- | Plain text format.
    plainText :: Prelude.Maybe Prelude.Text,
    -- | Rich text. Examples of rich text include bold, underline, and italics.
    richText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LongFormatText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'plainText', 'longFormatText_plainText' - Plain text format.
--
-- 'richText', 'longFormatText_richText' - Rich text. Examples of rich text include bold, underline, and italics.
newLongFormatText ::
  LongFormatText
newLongFormatText =
  LongFormatText'
    { plainText = Prelude.Nothing,
      richText = Prelude.Nothing
    }

-- | Plain text format.
longFormatText_plainText :: Lens.Lens' LongFormatText (Prelude.Maybe Prelude.Text)
longFormatText_plainText = Lens.lens (\LongFormatText' {plainText} -> plainText) (\s@LongFormatText' {} a -> s {plainText = a} :: LongFormatText)

-- | Rich text. Examples of rich text include bold, underline, and italics.
longFormatText_richText :: Lens.Lens' LongFormatText (Prelude.Maybe Prelude.Text)
longFormatText_richText = Lens.lens (\LongFormatText' {richText} -> richText) (\s@LongFormatText' {} a -> s {richText = a} :: LongFormatText)

instance Data.FromJSON LongFormatText where
  parseJSON =
    Data.withObject
      "LongFormatText"
      ( \x ->
          LongFormatText'
            Prelude.<$> (x Data..:? "PlainText")
            Prelude.<*> (x Data..:? "RichText")
      )

instance Prelude.Hashable LongFormatText where
  hashWithSalt _salt LongFormatText' {..} =
    _salt `Prelude.hashWithSalt` plainText
      `Prelude.hashWithSalt` richText

instance Prelude.NFData LongFormatText where
  rnf LongFormatText' {..} =
    Prelude.rnf plainText
      `Prelude.seq` Prelude.rnf richText

instance Data.ToJSON LongFormatText where
  toJSON LongFormatText' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlainText" Data..=) Prelude.<$> plainText,
            ("RichText" Data..=) Prelude.<$> richText
          ]
      )
