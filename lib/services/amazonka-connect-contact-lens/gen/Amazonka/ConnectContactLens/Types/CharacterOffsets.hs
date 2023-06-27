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
-- Module      : Amazonka.ConnectContactLens.Types.CharacterOffsets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectContactLens.Types.CharacterOffsets where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For characters that were detected as issues, where they occur in the
-- transcript.
--
-- /See:/ 'newCharacterOffsets' smart constructor.
data CharacterOffsets = CharacterOffsets'
  { -- | The beginning of the issue.
    beginOffsetChar :: Prelude.Natural,
    -- | The end of the issue.
    endOffsetChar :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CharacterOffsets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffsetChar', 'characterOffsets_beginOffsetChar' - The beginning of the issue.
--
-- 'endOffsetChar', 'characterOffsets_endOffsetChar' - The end of the issue.
newCharacterOffsets ::
  -- | 'beginOffsetChar'
  Prelude.Natural ->
  -- | 'endOffsetChar'
  Prelude.Natural ->
  CharacterOffsets
newCharacterOffsets pBeginOffsetChar_ pEndOffsetChar_ =
  CharacterOffsets'
    { beginOffsetChar =
        pBeginOffsetChar_,
      endOffsetChar = pEndOffsetChar_
    }

-- | The beginning of the issue.
characterOffsets_beginOffsetChar :: Lens.Lens' CharacterOffsets Prelude.Natural
characterOffsets_beginOffsetChar = Lens.lens (\CharacterOffsets' {beginOffsetChar} -> beginOffsetChar) (\s@CharacterOffsets' {} a -> s {beginOffsetChar = a} :: CharacterOffsets)

-- | The end of the issue.
characterOffsets_endOffsetChar :: Lens.Lens' CharacterOffsets Prelude.Natural
characterOffsets_endOffsetChar = Lens.lens (\CharacterOffsets' {endOffsetChar} -> endOffsetChar) (\s@CharacterOffsets' {} a -> s {endOffsetChar = a} :: CharacterOffsets)

instance Data.FromJSON CharacterOffsets where
  parseJSON =
    Data.withObject
      "CharacterOffsets"
      ( \x ->
          CharacterOffsets'
            Prelude.<$> (x Data..: "BeginOffsetChar")
            Prelude.<*> (x Data..: "EndOffsetChar")
      )

instance Prelude.Hashable CharacterOffsets where
  hashWithSalt _salt CharacterOffsets' {..} =
    _salt
      `Prelude.hashWithSalt` beginOffsetChar
      `Prelude.hashWithSalt` endOffsetChar

instance Prelude.NFData CharacterOffsets where
  rnf CharacterOffsets' {..} =
    Prelude.rnf beginOffsetChar
      `Prelude.seq` Prelude.rnf endOffsetChar
