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
-- Module      : Network.AWS.MediaConvert.Types.Id3Insertion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Id3Insertion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3)
-- to specify the base 64 encoded string and use Timecode (TimeCode) to
-- specify the time when the tag should be inserted. To insert multiple ID3
-- tags in your output, create multiple instances of ID3 insertion
-- (Id3Insertion).
--
-- /See:/ 'newId3Insertion' smart constructor.
data Id3Insertion = Id3Insertion'
  { -- | Use ID3 tag (Id3) to provide a tag value in base64-encode format.
    id3 :: Core.Maybe Core.Text,
    -- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
    timecode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Id3Insertion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id3', 'id3Insertion_id3' - Use ID3 tag (Id3) to provide a tag value in base64-encode format.
--
-- 'timecode', 'id3Insertion_timecode' - Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
newId3Insertion ::
  Id3Insertion
newId3Insertion =
  Id3Insertion'
    { id3 = Core.Nothing,
      timecode = Core.Nothing
    }

-- | Use ID3 tag (Id3) to provide a tag value in base64-encode format.
id3Insertion_id3 :: Lens.Lens' Id3Insertion (Core.Maybe Core.Text)
id3Insertion_id3 = Lens.lens (\Id3Insertion' {id3} -> id3) (\s@Id3Insertion' {} a -> s {id3 = a} :: Id3Insertion)

-- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
id3Insertion_timecode :: Lens.Lens' Id3Insertion (Core.Maybe Core.Text)
id3Insertion_timecode = Lens.lens (\Id3Insertion' {timecode} -> timecode) (\s@Id3Insertion' {} a -> s {timecode = a} :: Id3Insertion)

instance Core.FromJSON Id3Insertion where
  parseJSON =
    Core.withObject
      "Id3Insertion"
      ( \x ->
          Id3Insertion'
            Core.<$> (x Core..:? "id3") Core.<*> (x Core..:? "timecode")
      )

instance Core.Hashable Id3Insertion

instance Core.NFData Id3Insertion

instance Core.ToJSON Id3Insertion where
  toJSON Id3Insertion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("id3" Core..=) Core.<$> id3,
            ("timecode" Core..=) Core.<$> timecode
          ]
      )
