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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.KeyPhrase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a key noun phrase.
--
-- /See:/ 'newKeyPhrase' smart constructor.
data KeyPhrase = KeyPhrase'
  { -- | The zero-based offset from the beginning of the source text to the first
    -- character in the key phrase.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The zero-based offset from the beginning of the source text to the last
    -- character in the key phrase.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- the detection.
    score :: Prelude.Maybe Prelude.Double,
    -- | The text of a key noun phrase.
    text :: Prelude.Maybe Prelude.Text
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
-- 'beginOffset', 'keyPhrase_beginOffset' - The zero-based offset from the beginning of the source text to the first
-- character in the key phrase.
--
-- 'endOffset', 'keyPhrase_endOffset' - The zero-based offset from the beginning of the source text to the last
-- character in the key phrase.
--
-- 'score', 'keyPhrase_score' - The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
--
-- 'text', 'keyPhrase_text' - The text of a key noun phrase.
newKeyPhrase ::
  KeyPhrase
newKeyPhrase =
  KeyPhrase'
    { beginOffset = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      score = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The zero-based offset from the beginning of the source text to the first
-- character in the key phrase.
keyPhrase_beginOffset :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Int)
keyPhrase_beginOffset = Lens.lens (\KeyPhrase' {beginOffset} -> beginOffset) (\s@KeyPhrase' {} a -> s {beginOffset = a} :: KeyPhrase)

-- | The zero-based offset from the beginning of the source text to the last
-- character in the key phrase.
keyPhrase_endOffset :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Int)
keyPhrase_endOffset = Lens.lens (\KeyPhrase' {endOffset} -> endOffset) (\s@KeyPhrase' {} a -> s {endOffset = a} :: KeyPhrase)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- the detection.
keyPhrase_score :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Double)
keyPhrase_score = Lens.lens (\KeyPhrase' {score} -> score) (\s@KeyPhrase' {} a -> s {score = a} :: KeyPhrase)

-- | The text of a key noun phrase.
keyPhrase_text :: Lens.Lens' KeyPhrase (Prelude.Maybe Prelude.Text)
keyPhrase_text = Lens.lens (\KeyPhrase' {text} -> text) (\s@KeyPhrase' {} a -> s {text = a} :: KeyPhrase)

instance Data.FromJSON KeyPhrase where
  parseJSON =
    Data.withObject
      "KeyPhrase"
      ( \x ->
          KeyPhrase'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "EndOffset")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable KeyPhrase where
  hashWithSalt _salt KeyPhrase' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` text

instance Prelude.NFData KeyPhrase where
  rnf KeyPhrase' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf text
