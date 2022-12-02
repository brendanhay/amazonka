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
-- Module      : Amazonka.LexV2Models.Types.AudioSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AudioSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the audio input specifications.
--
-- /See:/ 'newAudioSpecification' smart constructor.
data AudioSpecification = AudioSpecification'
  { -- | Time for how long Amazon Lex waits before speech input is truncated and
    -- the speech is returned to application.
    maxLengthMs :: Prelude.Natural,
    -- | Time for which a bot waits after the customer stops speaking to assume
    -- the utterance is finished.
    endTimeoutMs :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLengthMs', 'audioSpecification_maxLengthMs' - Time for how long Amazon Lex waits before speech input is truncated and
-- the speech is returned to application.
--
-- 'endTimeoutMs', 'audioSpecification_endTimeoutMs' - Time for which a bot waits after the customer stops speaking to assume
-- the utterance is finished.
newAudioSpecification ::
  -- | 'maxLengthMs'
  Prelude.Natural ->
  -- | 'endTimeoutMs'
  Prelude.Natural ->
  AudioSpecification
newAudioSpecification pMaxLengthMs_ pEndTimeoutMs_ =
  AudioSpecification'
    { maxLengthMs = pMaxLengthMs_,
      endTimeoutMs = pEndTimeoutMs_
    }

-- | Time for how long Amazon Lex waits before speech input is truncated and
-- the speech is returned to application.
audioSpecification_maxLengthMs :: Lens.Lens' AudioSpecification Prelude.Natural
audioSpecification_maxLengthMs = Lens.lens (\AudioSpecification' {maxLengthMs} -> maxLengthMs) (\s@AudioSpecification' {} a -> s {maxLengthMs = a} :: AudioSpecification)

-- | Time for which a bot waits after the customer stops speaking to assume
-- the utterance is finished.
audioSpecification_endTimeoutMs :: Lens.Lens' AudioSpecification Prelude.Natural
audioSpecification_endTimeoutMs = Lens.lens (\AudioSpecification' {endTimeoutMs} -> endTimeoutMs) (\s@AudioSpecification' {} a -> s {endTimeoutMs = a} :: AudioSpecification)

instance Data.FromJSON AudioSpecification where
  parseJSON =
    Data.withObject
      "AudioSpecification"
      ( \x ->
          AudioSpecification'
            Prelude.<$> (x Data..: "maxLengthMs")
            Prelude.<*> (x Data..: "endTimeoutMs")
      )

instance Prelude.Hashable AudioSpecification where
  hashWithSalt _salt AudioSpecification' {..} =
    _salt `Prelude.hashWithSalt` maxLengthMs
      `Prelude.hashWithSalt` endTimeoutMs

instance Prelude.NFData AudioSpecification where
  rnf AudioSpecification' {..} =
    Prelude.rnf maxLengthMs
      `Prelude.seq` Prelude.rnf endTimeoutMs

instance Data.ToJSON AudioSpecification where
  toJSON AudioSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("maxLengthMs" Data..= maxLengthMs),
            Prelude.Just ("endTimeoutMs" Data..= endTimeoutMs)
          ]
      )
