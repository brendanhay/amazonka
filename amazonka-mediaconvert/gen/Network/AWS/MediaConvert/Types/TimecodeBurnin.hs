{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurnin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimecodeBurnin where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TimecodeBurninPosition
import qualified Network.AWS.Prelude as Prelude

-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and
-- specified prefix into the output.
--
-- /See:/ 'newTimecodeBurnin' smart constructor.
data TimecodeBurnin = TimecodeBurnin'
  { -- | Use Prefix (Prefix) to place ASCII characters before any burned-in
    -- timecode. For example, a prefix of \"EZ-\" will result in the timecode
    -- \"EZ-00:00:00:00\". Provide either the characters themselves or the
    -- ASCII code equivalents. The supported range of characters is 0x20
    -- through 0x7e. This includes letters, numbers, and all special characters
    -- represented on a standard English keyboard.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
    -- specify the location the burned-in timecode on output video.
    position :: Prelude.Maybe TimecodeBurninPosition,
    -- | Use Font Size (FontSize) to set the font size of any burned-in timecode.
    -- Valid values are 10, 16, 32, 48.
    fontSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimecodeBurnin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'timecodeBurnin_prefix' - Use Prefix (Prefix) to place ASCII characters before any burned-in
-- timecode. For example, a prefix of \"EZ-\" will result in the timecode
-- \"EZ-00:00:00:00\". Provide either the characters themselves or the
-- ASCII code equivalents. The supported range of characters is 0x20
-- through 0x7e. This includes letters, numbers, and all special characters
-- represented on a standard English keyboard.
--
-- 'position', 'timecodeBurnin_position' - Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
-- specify the location the burned-in timecode on output video.
--
-- 'fontSize', 'timecodeBurnin_fontSize' - Use Font Size (FontSize) to set the font size of any burned-in timecode.
-- Valid values are 10, 16, 32, 48.
newTimecodeBurnin ::
  TimecodeBurnin
newTimecodeBurnin =
  TimecodeBurnin'
    { prefix = Prelude.Nothing,
      position = Prelude.Nothing,
      fontSize = Prelude.Nothing
    }

-- | Use Prefix (Prefix) to place ASCII characters before any burned-in
-- timecode. For example, a prefix of \"EZ-\" will result in the timecode
-- \"EZ-00:00:00:00\". Provide either the characters themselves or the
-- ASCII code equivalents. The supported range of characters is 0x20
-- through 0x7e. This includes letters, numbers, and all special characters
-- represented on a standard English keyboard.
timecodeBurnin_prefix :: Lens.Lens' TimecodeBurnin (Prelude.Maybe Prelude.Text)
timecodeBurnin_prefix = Lens.lens (\TimecodeBurnin' {prefix} -> prefix) (\s@TimecodeBurnin' {} a -> s {prefix = a} :: TimecodeBurnin)

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
-- specify the location the burned-in timecode on output video.
timecodeBurnin_position :: Lens.Lens' TimecodeBurnin (Prelude.Maybe TimecodeBurninPosition)
timecodeBurnin_position = Lens.lens (\TimecodeBurnin' {position} -> position) (\s@TimecodeBurnin' {} a -> s {position = a} :: TimecodeBurnin)

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode.
-- Valid values are 10, 16, 32, 48.
timecodeBurnin_fontSize :: Lens.Lens' TimecodeBurnin (Prelude.Maybe Prelude.Natural)
timecodeBurnin_fontSize = Lens.lens (\TimecodeBurnin' {fontSize} -> fontSize) (\s@TimecodeBurnin' {} a -> s {fontSize = a} :: TimecodeBurnin)

instance Prelude.FromJSON TimecodeBurnin where
  parseJSON =
    Prelude.withObject
      "TimecodeBurnin"
      ( \x ->
          TimecodeBurnin'
            Prelude.<$> (x Prelude..:? "prefix")
            Prelude.<*> (x Prelude..:? "position")
            Prelude.<*> (x Prelude..:? "fontSize")
      )

instance Prelude.Hashable TimecodeBurnin

instance Prelude.NFData TimecodeBurnin

instance Prelude.ToJSON TimecodeBurnin where
  toJSON TimecodeBurnin' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("prefix" Prelude..=) Prelude.<$> prefix,
            ("position" Prelude..=) Prelude.<$> position,
            ("fontSize" Prelude..=) Prelude.<$> fontSize
          ]
      )
