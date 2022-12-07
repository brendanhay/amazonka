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
-- Module      : Amazonka.MediaLive.Types.Fmp4HlsSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Fmp4HlsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Fmp4NielsenId3Behavior
import Amazonka.MediaLive.Types.Fmp4TimedMetadataBehavior
import qualified Amazonka.Prelude as Prelude

-- | Fmp4 Hls Settings
--
-- /See:/ 'newFmp4HlsSettings' smart constructor.
data Fmp4HlsSettings = Fmp4HlsSettings'
  { -- | List all the audio groups that are used with the video output stream.
    -- Input all the audio GROUP-IDs that are associated to the video, separate
    -- by \',\'.
    audioRenditionSets :: Prelude.Maybe Prelude.Text,
    -- | If set to passthrough, Nielsen inaudible tones for media tracking will
    -- be detected in the input audio and an equivalent ID3 tag will be
    -- inserted in the output.
    nielsenId3Behavior :: Prelude.Maybe Fmp4NielsenId3Behavior,
    -- | When set to passthrough, timed metadata is passed through from input to
    -- output.
    timedMetadataBehavior :: Prelude.Maybe Fmp4TimedMetadataBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Fmp4HlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioRenditionSets', 'fmp4HlsSettings_audioRenditionSets' - List all the audio groups that are used with the video output stream.
-- Input all the audio GROUP-IDs that are associated to the video, separate
-- by \',\'.
--
-- 'nielsenId3Behavior', 'fmp4HlsSettings_nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
--
-- 'timedMetadataBehavior', 'fmp4HlsSettings_timedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to
-- output.
newFmp4HlsSettings ::
  Fmp4HlsSettings
newFmp4HlsSettings =
  Fmp4HlsSettings'
    { audioRenditionSets =
        Prelude.Nothing,
      nielsenId3Behavior = Prelude.Nothing,
      timedMetadataBehavior = Prelude.Nothing
    }

-- | List all the audio groups that are used with the video output stream.
-- Input all the audio GROUP-IDs that are associated to the video, separate
-- by \',\'.
fmp4HlsSettings_audioRenditionSets :: Lens.Lens' Fmp4HlsSettings (Prelude.Maybe Prelude.Text)
fmp4HlsSettings_audioRenditionSets = Lens.lens (\Fmp4HlsSettings' {audioRenditionSets} -> audioRenditionSets) (\s@Fmp4HlsSettings' {} a -> s {audioRenditionSets = a} :: Fmp4HlsSettings)

-- | If set to passthrough, Nielsen inaudible tones for media tracking will
-- be detected in the input audio and an equivalent ID3 tag will be
-- inserted in the output.
fmp4HlsSettings_nielsenId3Behavior :: Lens.Lens' Fmp4HlsSettings (Prelude.Maybe Fmp4NielsenId3Behavior)
fmp4HlsSettings_nielsenId3Behavior = Lens.lens (\Fmp4HlsSettings' {nielsenId3Behavior} -> nielsenId3Behavior) (\s@Fmp4HlsSettings' {} a -> s {nielsenId3Behavior = a} :: Fmp4HlsSettings)

-- | When set to passthrough, timed metadata is passed through from input to
-- output.
fmp4HlsSettings_timedMetadataBehavior :: Lens.Lens' Fmp4HlsSettings (Prelude.Maybe Fmp4TimedMetadataBehavior)
fmp4HlsSettings_timedMetadataBehavior = Lens.lens (\Fmp4HlsSettings' {timedMetadataBehavior} -> timedMetadataBehavior) (\s@Fmp4HlsSettings' {} a -> s {timedMetadataBehavior = a} :: Fmp4HlsSettings)

instance Data.FromJSON Fmp4HlsSettings where
  parseJSON =
    Data.withObject
      "Fmp4HlsSettings"
      ( \x ->
          Fmp4HlsSettings'
            Prelude.<$> (x Data..:? "audioRenditionSets")
            Prelude.<*> (x Data..:? "nielsenId3Behavior")
            Prelude.<*> (x Data..:? "timedMetadataBehavior")
      )

instance Prelude.Hashable Fmp4HlsSettings where
  hashWithSalt _salt Fmp4HlsSettings' {..} =
    _salt `Prelude.hashWithSalt` audioRenditionSets
      `Prelude.hashWithSalt` nielsenId3Behavior
      `Prelude.hashWithSalt` timedMetadataBehavior

instance Prelude.NFData Fmp4HlsSettings where
  rnf Fmp4HlsSettings' {..} =
    Prelude.rnf audioRenditionSets
      `Prelude.seq` Prelude.rnf nielsenId3Behavior
      `Prelude.seq` Prelude.rnf timedMetadataBehavior

instance Data.ToJSON Fmp4HlsSettings where
  toJSON Fmp4HlsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioRenditionSets" Data..=)
              Prelude.<$> audioRenditionSets,
            ("nielsenId3Behavior" Data..=)
              Prelude.<$> nielsenId3Behavior,
            ("timedMetadataBehavior" Data..=)
              Prelude.<$> timedMetadataBehavior
          ]
      )
