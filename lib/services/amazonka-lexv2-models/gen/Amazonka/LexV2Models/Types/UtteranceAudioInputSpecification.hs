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
-- Module      : Amazonka.LexV2Models.Types.UtteranceAudioInputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UtteranceAudioInputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the audio for an utterance.
--
-- /See:/ 'newUtteranceAudioInputSpecification' smart constructor.
data UtteranceAudioInputSpecification = UtteranceAudioInputSpecification'
  { -- | Amazon S3 file pointing to the audio.
    audioFileS3Location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceAudioInputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioFileS3Location', 'utteranceAudioInputSpecification_audioFileS3Location' - Amazon S3 file pointing to the audio.
newUtteranceAudioInputSpecification ::
  -- | 'audioFileS3Location'
  Prelude.Text ->
  UtteranceAudioInputSpecification
newUtteranceAudioInputSpecification
  pAudioFileS3Location_ =
    UtteranceAudioInputSpecification'
      { audioFileS3Location =
          pAudioFileS3Location_
      }

-- | Amazon S3 file pointing to the audio.
utteranceAudioInputSpecification_audioFileS3Location :: Lens.Lens' UtteranceAudioInputSpecification Prelude.Text
utteranceAudioInputSpecification_audioFileS3Location = Lens.lens (\UtteranceAudioInputSpecification' {audioFileS3Location} -> audioFileS3Location) (\s@UtteranceAudioInputSpecification' {} a -> s {audioFileS3Location = a} :: UtteranceAudioInputSpecification)

instance
  Data.FromJSON
    UtteranceAudioInputSpecification
  where
  parseJSON =
    Data.withObject
      "UtteranceAudioInputSpecification"
      ( \x ->
          UtteranceAudioInputSpecification'
            Prelude.<$> (x Data..: "audioFileS3Location")
      )

instance
  Prelude.Hashable
    UtteranceAudioInputSpecification
  where
  hashWithSalt
    _salt
    UtteranceAudioInputSpecification' {..} =
      _salt `Prelude.hashWithSalt` audioFileS3Location

instance
  Prelude.NFData
    UtteranceAudioInputSpecification
  where
  rnf UtteranceAudioInputSpecification' {..} =
    Prelude.rnf audioFileS3Location
