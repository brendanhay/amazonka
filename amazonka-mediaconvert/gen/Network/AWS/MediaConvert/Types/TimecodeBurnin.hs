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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.TimecodeBurninPosition

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
    prefix :: Core.Maybe Core.Text,
    -- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
    -- specify the location the burned-in timecode on output video.
    position :: Core.Maybe TimecodeBurninPosition,
    -- | Use Font Size (FontSize) to set the font size of any burned-in timecode.
    -- Valid values are 10, 16, 32, 48.
    fontSize :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { prefix = Core.Nothing,
      position = Core.Nothing,
      fontSize = Core.Nothing
    }

-- | Use Prefix (Prefix) to place ASCII characters before any burned-in
-- timecode. For example, a prefix of \"EZ-\" will result in the timecode
-- \"EZ-00:00:00:00\". Provide either the characters themselves or the
-- ASCII code equivalents. The supported range of characters is 0x20
-- through 0x7e. This includes letters, numbers, and all special characters
-- represented on a standard English keyboard.
timecodeBurnin_prefix :: Lens.Lens' TimecodeBurnin (Core.Maybe Core.Text)
timecodeBurnin_prefix = Lens.lens (\TimecodeBurnin' {prefix} -> prefix) (\s@TimecodeBurnin' {} a -> s {prefix = a} :: TimecodeBurnin)

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
-- specify the location the burned-in timecode on output video.
timecodeBurnin_position :: Lens.Lens' TimecodeBurnin (Core.Maybe TimecodeBurninPosition)
timecodeBurnin_position = Lens.lens (\TimecodeBurnin' {position} -> position) (\s@TimecodeBurnin' {} a -> s {position = a} :: TimecodeBurnin)

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode.
-- Valid values are 10, 16, 32, 48.
timecodeBurnin_fontSize :: Lens.Lens' TimecodeBurnin (Core.Maybe Core.Natural)
timecodeBurnin_fontSize = Lens.lens (\TimecodeBurnin' {fontSize} -> fontSize) (\s@TimecodeBurnin' {} a -> s {fontSize = a} :: TimecodeBurnin)

instance Core.FromJSON TimecodeBurnin where
  parseJSON =
    Core.withObject
      "TimecodeBurnin"
      ( \x ->
          TimecodeBurnin'
            Core.<$> (x Core..:? "prefix")
            Core.<*> (x Core..:? "position")
            Core.<*> (x Core..:? "fontSize")
      )

instance Core.Hashable TimecodeBurnin

instance Core.NFData TimecodeBurnin

instance Core.ToJSON TimecodeBurnin where
  toJSON TimecodeBurnin' {..} =
    Core.object
      ( Core.catMaybes
          [ ("prefix" Core..=) Core.<$> prefix,
            ("position" Core..=) Core.<$> position,
            ("fontSize" Core..=) Core.<$> fontSize
          ]
      )
