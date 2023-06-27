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
-- Module      : Amazonka.MediaConvert.Types.Id3Insertion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Id3Insertion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3)
-- to specify the base 64 encoded string and use Timecode (TimeCode) to
-- specify the time when the tag should be inserted. To insert multiple ID3
-- tags in your output, create multiple instances of ID3 insertion
-- (Id3Insertion).
--
-- /See:/ 'newId3Insertion' smart constructor.
data Id3Insertion = Id3Insertion'
  { -- | Use ID3 tag (Id3) to provide a fully formed ID3 tag in base64-encode
    -- format.
    id3 :: Prelude.Maybe Prelude.Text,
    -- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
    timecode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Id3Insertion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id3', 'id3Insertion_id3' - Use ID3 tag (Id3) to provide a fully formed ID3 tag in base64-encode
-- format.
--
-- 'timecode', 'id3Insertion_timecode' - Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
newId3Insertion ::
  Id3Insertion
newId3Insertion =
  Id3Insertion'
    { id3 = Prelude.Nothing,
      timecode = Prelude.Nothing
    }

-- | Use ID3 tag (Id3) to provide a fully formed ID3 tag in base64-encode
-- format.
id3Insertion_id3 :: Lens.Lens' Id3Insertion (Prelude.Maybe Prelude.Text)
id3Insertion_id3 = Lens.lens (\Id3Insertion' {id3} -> id3) (\s@Id3Insertion' {} a -> s {id3 = a} :: Id3Insertion)

-- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
id3Insertion_timecode :: Lens.Lens' Id3Insertion (Prelude.Maybe Prelude.Text)
id3Insertion_timecode = Lens.lens (\Id3Insertion' {timecode} -> timecode) (\s@Id3Insertion' {} a -> s {timecode = a} :: Id3Insertion)

instance Data.FromJSON Id3Insertion where
  parseJSON =
    Data.withObject
      "Id3Insertion"
      ( \x ->
          Id3Insertion'
            Prelude.<$> (x Data..:? "id3")
            Prelude.<*> (x Data..:? "timecode")
      )

instance Prelude.Hashable Id3Insertion where
  hashWithSalt _salt Id3Insertion' {..} =
    _salt
      `Prelude.hashWithSalt` id3
      `Prelude.hashWithSalt` timecode

instance Prelude.NFData Id3Insertion where
  rnf Id3Insertion' {..} =
    Prelude.rnf id3 `Prelude.seq` Prelude.rnf timecode

instance Data.ToJSON Id3Insertion where
  toJSON Id3Insertion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("id3" Data..=) Prelude.<$> id3,
            ("timecode" Data..=) Prelude.<$> timecode
          ]
      )
