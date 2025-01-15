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
-- Module      : Amazonka.MediaLive.Types.AudioTrackSelection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioTrackSelection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioDolbyEDecode
import Amazonka.MediaLive.Types.AudioTrack
import qualified Amazonka.Prelude as Prelude

-- | Audio Track Selection
--
-- /See:/ 'newAudioTrackSelection' smart constructor.
data AudioTrackSelection = AudioTrackSelection'
  { -- | Configure decoding options for Dolby E streams - these should be Dolby E
    -- frames carried in PCM streams tagged with SMPTE-337
    dolbyEDecode :: Prelude.Maybe AudioDolbyEDecode,
    -- | Selects one or more unique audio tracks from within a source.
    tracks :: [AudioTrack]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioTrackSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dolbyEDecode', 'audioTrackSelection_dolbyEDecode' - Configure decoding options for Dolby E streams - these should be Dolby E
-- frames carried in PCM streams tagged with SMPTE-337
--
-- 'tracks', 'audioTrackSelection_tracks' - Selects one or more unique audio tracks from within a source.
newAudioTrackSelection ::
  AudioTrackSelection
newAudioTrackSelection =
  AudioTrackSelection'
    { dolbyEDecode =
        Prelude.Nothing,
      tracks = Prelude.mempty
    }

-- | Configure decoding options for Dolby E streams - these should be Dolby E
-- frames carried in PCM streams tagged with SMPTE-337
audioTrackSelection_dolbyEDecode :: Lens.Lens' AudioTrackSelection (Prelude.Maybe AudioDolbyEDecode)
audioTrackSelection_dolbyEDecode = Lens.lens (\AudioTrackSelection' {dolbyEDecode} -> dolbyEDecode) (\s@AudioTrackSelection' {} a -> s {dolbyEDecode = a} :: AudioTrackSelection)

-- | Selects one or more unique audio tracks from within a source.
audioTrackSelection_tracks :: Lens.Lens' AudioTrackSelection [AudioTrack]
audioTrackSelection_tracks = Lens.lens (\AudioTrackSelection' {tracks} -> tracks) (\s@AudioTrackSelection' {} a -> s {tracks = a} :: AudioTrackSelection) Prelude.. Lens.coerced

instance Data.FromJSON AudioTrackSelection where
  parseJSON =
    Data.withObject
      "AudioTrackSelection"
      ( \x ->
          AudioTrackSelection'
            Prelude.<$> (x Data..:? "dolbyEDecode")
            Prelude.<*> (x Data..:? "tracks" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AudioTrackSelection where
  hashWithSalt _salt AudioTrackSelection' {..} =
    _salt
      `Prelude.hashWithSalt` dolbyEDecode
      `Prelude.hashWithSalt` tracks

instance Prelude.NFData AudioTrackSelection where
  rnf AudioTrackSelection' {..} =
    Prelude.rnf dolbyEDecode `Prelude.seq`
      Prelude.rnf tracks

instance Data.ToJSON AudioTrackSelection where
  toJSON AudioTrackSelection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dolbyEDecode" Data..=) Prelude.<$> dolbyEDecode,
            Prelude.Just ("tracks" Data..= tracks)
          ]
      )
