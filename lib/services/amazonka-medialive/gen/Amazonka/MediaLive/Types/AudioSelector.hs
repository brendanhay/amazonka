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
-- Module      : Amazonka.MediaLive.Types.AudioSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioSelectorSettings
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON AudioSelector where
  parseJSON =
    Data.withObject
      "AudioSelector"
      ( \x ->
          AudioSelector'
            Prelude.<$> (x Data..:? "selectorSettings")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable AudioSelector where
  hashWithSalt _salt AudioSelector' {..} =
    _salt
      `Prelude.hashWithSalt` selectorSettings
      `Prelude.hashWithSalt` name

instance Prelude.NFData AudioSelector where
  rnf AudioSelector' {..} =
    Prelude.rnf selectorSettings `Prelude.seq`
      Prelude.rnf name

instance Data.ToJSON AudioSelector where
  toJSON AudioSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("selectorSettings" Data..=)
              Prelude.<$> selectorSettings,
            Prelude.Just ("name" Data..= name)
          ]
      )
