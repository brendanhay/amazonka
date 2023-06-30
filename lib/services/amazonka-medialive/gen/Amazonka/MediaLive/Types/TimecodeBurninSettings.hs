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
-- Module      : Amazonka.MediaLive.Types.TimecodeBurninSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TimecodeBurninSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.TimecodeBurninFontSize
import Amazonka.MediaLive.Types.TimecodeBurninPosition
import qualified Amazonka.Prelude as Prelude

-- | Timecode Burnin Settings
--
-- /See:/ 'newTimecodeBurninSettings' smart constructor.
data TimecodeBurninSettings = TimecodeBurninSettings'
  { -- | Create a timecode burn-in prefix (optional)
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Choose a timecode burn-in output position
    position :: TimecodeBurninPosition,
    -- | Choose a timecode burn-in font size
    fontSize :: TimecodeBurninFontSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimecodeBurninSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'timecodeBurninSettings_prefix' - Create a timecode burn-in prefix (optional)
--
-- 'position', 'timecodeBurninSettings_position' - Choose a timecode burn-in output position
--
-- 'fontSize', 'timecodeBurninSettings_fontSize' - Choose a timecode burn-in font size
newTimecodeBurninSettings ::
  -- | 'position'
  TimecodeBurninPosition ->
  -- | 'fontSize'
  TimecodeBurninFontSize ->
  TimecodeBurninSettings
newTimecodeBurninSettings pPosition_ pFontSize_ =
  TimecodeBurninSettings'
    { prefix = Prelude.Nothing,
      position = pPosition_,
      fontSize = pFontSize_
    }

-- | Create a timecode burn-in prefix (optional)
timecodeBurninSettings_prefix :: Lens.Lens' TimecodeBurninSettings (Prelude.Maybe Prelude.Text)
timecodeBurninSettings_prefix = Lens.lens (\TimecodeBurninSettings' {prefix} -> prefix) (\s@TimecodeBurninSettings' {} a -> s {prefix = a} :: TimecodeBurninSettings)

-- | Choose a timecode burn-in output position
timecodeBurninSettings_position :: Lens.Lens' TimecodeBurninSettings TimecodeBurninPosition
timecodeBurninSettings_position = Lens.lens (\TimecodeBurninSettings' {position} -> position) (\s@TimecodeBurninSettings' {} a -> s {position = a} :: TimecodeBurninSettings)

-- | Choose a timecode burn-in font size
timecodeBurninSettings_fontSize :: Lens.Lens' TimecodeBurninSettings TimecodeBurninFontSize
timecodeBurninSettings_fontSize = Lens.lens (\TimecodeBurninSettings' {fontSize} -> fontSize) (\s@TimecodeBurninSettings' {} a -> s {fontSize = a} :: TimecodeBurninSettings)

instance Data.FromJSON TimecodeBurninSettings where
  parseJSON =
    Data.withObject
      "TimecodeBurninSettings"
      ( \x ->
          TimecodeBurninSettings'
            Prelude.<$> (x Data..:? "prefix")
            Prelude.<*> (x Data..: "position")
            Prelude.<*> (x Data..: "fontSize")
      )

instance Prelude.Hashable TimecodeBurninSettings where
  hashWithSalt _salt TimecodeBurninSettings' {..} =
    _salt
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` fontSize

instance Prelude.NFData TimecodeBurninSettings where
  rnf TimecodeBurninSettings' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf fontSize

instance Data.ToJSON TimecodeBurninSettings where
  toJSON TimecodeBurninSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("position" Data..= position),
            Prelude.Just ("fontSize" Data..= fontSize)
          ]
      )
