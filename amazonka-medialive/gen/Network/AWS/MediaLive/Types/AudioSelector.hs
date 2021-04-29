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
-- Module      : Network.AWS.MediaLive.Types.AudioSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioSelector where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioSelectorSettings
import qualified Network.AWS.Prelude as Prelude

-- | Audio Selector
--
-- /See:/ 'newAudioSelector' smart constructor.
data AudioSelector = AudioSelector'
  { -- | The audio selector settings.
    selectorSettings :: Prelude.Maybe AudioSelectorSettings,
    -- | The name of this AudioSelector. AudioDescriptions will use this name to
    -- uniquely identify this Selector. Selector names should be unique per
    -- input.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AudioSelector
newAudioSelector pName_ =
  AudioSelector'
    { selectorSettings = Prelude.Nothing,
      name = pName_
    }

-- | The audio selector settings.
audioSelector_selectorSettings :: Lens.Lens' AudioSelector (Prelude.Maybe AudioSelectorSettings)
audioSelector_selectorSettings = Lens.lens (\AudioSelector' {selectorSettings} -> selectorSettings) (\s@AudioSelector' {} a -> s {selectorSettings = a} :: AudioSelector)

-- | The name of this AudioSelector. AudioDescriptions will use this name to
-- uniquely identify this Selector. Selector names should be unique per
-- input.
audioSelector_name :: Lens.Lens' AudioSelector Prelude.Text
audioSelector_name = Lens.lens (\AudioSelector' {name} -> name) (\s@AudioSelector' {} a -> s {name = a} :: AudioSelector)

instance Prelude.FromJSON AudioSelector where
  parseJSON =
    Prelude.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Prelude.<$> (x Prelude..:? "selectorSettings")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable AudioSelector

instance Prelude.NFData AudioSelector

instance Prelude.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("selectorSettings" Prelude..=)
              Prelude.<$> selectorSettings,
            Prelude.Just ("name" Prelude..= name)
          ]
      )
