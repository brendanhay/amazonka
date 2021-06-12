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
-- Module      : Network.AWS.MediaLive.Types.AudioSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioSelector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioSelectorSettings

-- | Audio Selector
--
-- /See:/ 'newAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | The audio selector settings.
    selectorSettings :: Core.Maybe AudioSelectorSettings,
    -- | The name of this AudioSelector. AudioDescriptions will use this name to
    -- uniquely identify this Selector. Selector names should be unique per
    -- input.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AudioSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectorSettings', 'audioSelector_selectorSettings' - The audio selector settings.
--
-- 'name', 'audioSelector_name' - The name of this AudioSelector. AudioDescriptions will use this name to
-- uniquely identify this Selector. Selector names should be unique per
-- input.
newAudioSelector ::
  -- | 'name'
  Core.Text ->
  AudioSelector
newAudioSelector pName_ =
  AudioSelector'
    { selectorSettings = Core.Nothing,
      name = pName_
    }

-- | The audio selector settings.
audioSelector_selectorSettings :: Lens.Lens' AudioSelector (Core.Maybe AudioSelectorSettings)
audioSelector_selectorSettings = Lens.lens (\AudioSelector' {selectorSettings} -> selectorSettings) (\s@AudioSelector' {} a -> s {selectorSettings = a} :: AudioSelector)

-- | The name of this AudioSelector. AudioDescriptions will use this name to
-- uniquely identify this Selector. Selector names should be unique per
-- input.
audioSelector_name :: Lens.Lens' AudioSelector Core.Text
audioSelector_name = Lens.lens (\AudioSelector' {name} -> name) (\s@AudioSelector' {} a -> s {name = a} :: AudioSelector)

instance Core.FromJSON AudioSelector where
  parseJSON =
    Core.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Core.<$> (x Core..:? "selectorSettings")
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable AudioSelector

instance Core.NFData AudioSelector

instance Core.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("selectorSettings" Core..=)
              Core.<$> selectorSettings,
            Core.Just ("name" Core..= name)
          ]
      )
