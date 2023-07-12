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
-- Module      : Amazonka.LexV2Models.Types.AudioAndDTMFInputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AudioAndDTMFInputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AudioSpecification
import Amazonka.LexV2Models.Types.DTMFSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies the audio and DTMF input specification.
--
-- /See:/ 'newAudioAndDTMFInputSpecification' smart constructor.
data AudioAndDTMFInputSpecification = AudioAndDTMFInputSpecification'
  { -- | Specifies the settings on audio input.
    audioSpecification :: Prelude.Maybe AudioSpecification,
    -- | Specifies the settings on DTMF input.
    dtmfSpecification :: Prelude.Maybe DTMFSpecification,
    -- | Time for which a bot waits before assuming that the customer isn\'t
    -- going to speak or press a key. This timeout is shared between Audio and
    -- DTMF inputs.
    startTimeoutMs :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioAndDTMFInputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioSpecification', 'audioAndDTMFInputSpecification_audioSpecification' - Specifies the settings on audio input.
--
-- 'dtmfSpecification', 'audioAndDTMFInputSpecification_dtmfSpecification' - Specifies the settings on DTMF input.
--
-- 'startTimeoutMs', 'audioAndDTMFInputSpecification_startTimeoutMs' - Time for which a bot waits before assuming that the customer isn\'t
-- going to speak or press a key. This timeout is shared between Audio and
-- DTMF inputs.
newAudioAndDTMFInputSpecification ::
  -- | 'startTimeoutMs'
  Prelude.Natural ->
  AudioAndDTMFInputSpecification
newAudioAndDTMFInputSpecification pStartTimeoutMs_ =
  AudioAndDTMFInputSpecification'
    { audioSpecification =
        Prelude.Nothing,
      dtmfSpecification = Prelude.Nothing,
      startTimeoutMs = pStartTimeoutMs_
    }

-- | Specifies the settings on audio input.
audioAndDTMFInputSpecification_audioSpecification :: Lens.Lens' AudioAndDTMFInputSpecification (Prelude.Maybe AudioSpecification)
audioAndDTMFInputSpecification_audioSpecification = Lens.lens (\AudioAndDTMFInputSpecification' {audioSpecification} -> audioSpecification) (\s@AudioAndDTMFInputSpecification' {} a -> s {audioSpecification = a} :: AudioAndDTMFInputSpecification)

-- | Specifies the settings on DTMF input.
audioAndDTMFInputSpecification_dtmfSpecification :: Lens.Lens' AudioAndDTMFInputSpecification (Prelude.Maybe DTMFSpecification)
audioAndDTMFInputSpecification_dtmfSpecification = Lens.lens (\AudioAndDTMFInputSpecification' {dtmfSpecification} -> dtmfSpecification) (\s@AudioAndDTMFInputSpecification' {} a -> s {dtmfSpecification = a} :: AudioAndDTMFInputSpecification)

-- | Time for which a bot waits before assuming that the customer isn\'t
-- going to speak or press a key. This timeout is shared between Audio and
-- DTMF inputs.
audioAndDTMFInputSpecification_startTimeoutMs :: Lens.Lens' AudioAndDTMFInputSpecification Prelude.Natural
audioAndDTMFInputSpecification_startTimeoutMs = Lens.lens (\AudioAndDTMFInputSpecification' {startTimeoutMs} -> startTimeoutMs) (\s@AudioAndDTMFInputSpecification' {} a -> s {startTimeoutMs = a} :: AudioAndDTMFInputSpecification)

instance Data.FromJSON AudioAndDTMFInputSpecification where
  parseJSON =
    Data.withObject
      "AudioAndDTMFInputSpecification"
      ( \x ->
          AudioAndDTMFInputSpecification'
            Prelude.<$> (x Data..:? "audioSpecification")
            Prelude.<*> (x Data..:? "dtmfSpecification")
            Prelude.<*> (x Data..: "startTimeoutMs")
      )

instance
  Prelude.Hashable
    AudioAndDTMFInputSpecification
  where
  hashWithSalt
    _salt
    AudioAndDTMFInputSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` audioSpecification
        `Prelude.hashWithSalt` dtmfSpecification
        `Prelude.hashWithSalt` startTimeoutMs

instance
  Prelude.NFData
    AudioAndDTMFInputSpecification
  where
  rnf AudioAndDTMFInputSpecification' {..} =
    Prelude.rnf audioSpecification
      `Prelude.seq` Prelude.rnf dtmfSpecification
      `Prelude.seq` Prelude.rnf startTimeoutMs

instance Data.ToJSON AudioAndDTMFInputSpecification where
  toJSON AudioAndDTMFInputSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioSpecification" Data..=)
              Prelude.<$> audioSpecification,
            ("dtmfSpecification" Data..=)
              Prelude.<$> dtmfSpecification,
            Prelude.Just
              ("startTimeoutMs" Data..= startTimeoutMs)
          ]
      )
