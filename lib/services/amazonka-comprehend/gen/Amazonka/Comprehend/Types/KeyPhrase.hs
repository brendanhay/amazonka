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
-- Module      : Amazonka.Comprehend.Types.KeyPhrase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.KeyPhrase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a key noun phrase.
--
-- /See:/ 'newKeyPhrase' smart constructor.
data KeyPhrase = KeyPhrase'
  { -- | A character offset in the input text that shows where the key phrase
    -- begins (the first character is at position 0). The offset returns the
    -- position of each UTF-8 code point in the string. A /code point/ is the
    -- abstract character from a particular graphical representation. For
    -- example, a multi-byte UTF-8 character maps to a single code point.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The text of a key noun phrase.
    text :: Prelude.Maybe Prelude.Text,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | A character offset in the input text where the key phrase ends. The
    -- offset returns the position of each UTF-8 code point in the string. A
    -- @code point@ is the abstract character from a particular graphical
    -- representation. For example, a multi-byte UTF-8 character maps to a
    -- single code point.
    endOffset :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyPhrase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'keyPhrase_beginOffset' - A character offset in the input text that shows where the key phrase
-- begins (the first character is at position 0). The offset returns the
-- position of each UTF-8 code point in the string. A /code point/ is the
-- abstract character from a particular graphical representation. For
-- example, a multi-byte UTF-8 character maps to a single code point.
--
-- 'text', 'keyPhrase_text' - The text of a key noun phrase.
--
-- 'score', 'keyPhrase_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'endOffset', 'keyPhrase_endOffset' - A character offset in the input text where the key phrase ends. The
-- offset returns the position of each UTF-8 code point in the string. A
-- @code point@ is the abstract character from a particular graphical
-- representation. For example, a multi-byte UTF-8 character maps to a
-- single code point.
newKeyPhrase ::
  KeyPhrase
newKeyPhrase =
  KeyPhrase'
    { beginOffset = Prelude.Nothing,
      text = Prelude.Nothing,
      score = Prelude.Nothing,
      endOffset = Prelude.Nothing
    }

-- | A character offset in the input text that shows where the key phrase
-- begins (the first character is at position 0). The offset returns the
-- position of each UTF-8 code point in the string. A /code point/ is the
-- abstract character from a particular graphical representation. For
-- example, a multi-byte UTF-8 character maps to a single code point.
keyPhrase_beginOffset :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Int)
keyPhrase_beginOffset = Lens.lens (\KeyPhrase' {beginOffset} -> beginOffset) (\s@KeyPhrase' {} a -> s {beginOffset = a} :: KeyPhrase)

-- | The text of a key noun phrase.
keyPhrase_text :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Text)
keyPhrase_text = Lens.lens (\KeyPhrase' {text} -> text) (\s@KeyPhrase' {} a -> s {text = a} :: KeyPhrase)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
keyPhrase_score :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Double)
keyPhrase_score = Lens.lens (\KeyPhrase' {score} -> score) (\s@KeyPhrase' {} a -> s {score = a} :: KeyPhrase)

-- | A character offset in the input text where the key phrase ends. The
-- offset returns the position of each UTF-8 code point in the string. A
-- @code point@ is the abstract character from a particular graphical
-- representation. For example, a multi-byte UTF-8 character maps to a
-- single code point.
keyPhrase_endOffset :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Int)
keyPhrase_endOffset = Lens.lens (\KeyPhrase' {endOffset} -> endOffset) (\s@KeyPhrase' {} a -> s {endOffset = a} :: KeyPhrase)

instance Core.FromJSON KeyPhrase where
  parseJSON =
    Core.withObject
      "KeyPhrase"
      ( \x ->
          KeyPhrase'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Score")
            Prelude.<*> (x Core..:? "EndOffset")
      )

instance Prelude.Hashable KeyPhrase where
  hashWithSalt salt' KeyPhrase' {..} =
    salt' `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` beginOffset

instance Prelude.NFData KeyPhrase where
  rnf KeyPhrase' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf text
