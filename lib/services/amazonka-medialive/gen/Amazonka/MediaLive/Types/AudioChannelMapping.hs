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
-- Module      : Amazonka.MediaLive.Types.AudioChannelMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioChannelMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputChannelLevel
import qualified Amazonka.Prelude as Prelude

-- | Audio Channel Mapping
--
-- /See:/ 'newAudioChannelMapping' smart constructor.
data AudioChannelMapping = AudioChannelMapping'
  { -- | The index of the output channel being produced.
    outputChannel :: Prelude.Natural,
    -- | Indices and gain values for each input channel that should be remixed
    -- into this output channel.
    inputChannelLevels :: [InputChannelLevel]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioChannelMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputChannel', 'audioChannelMapping_outputChannel' - The index of the output channel being produced.
--
-- 'inputChannelLevels', 'audioChannelMapping_inputChannelLevels' - Indices and gain values for each input channel that should be remixed
-- into this output channel.
newAudioChannelMapping ::
  -- | 'outputChannel'
  Prelude.Natural ->
  AudioChannelMapping
newAudioChannelMapping pOutputChannel_ =
  AudioChannelMapping'
    { outputChannel =
        pOutputChannel_,
      inputChannelLevels = Prelude.mempty
    }

-- | The index of the output channel being produced.
audioChannelMapping_outputChannel :: Lens.Lens' AudioChannelMapping Prelude.Natural
audioChannelMapping_outputChannel = Lens.lens (\AudioChannelMapping' {outputChannel} -> outputChannel) (\s@AudioChannelMapping' {} a -> s {outputChannel = a} :: AudioChannelMapping)

-- | Indices and gain values for each input channel that should be remixed
-- into this output channel.
audioChannelMapping_inputChannelLevels :: Lens.Lens' AudioChannelMapping [InputChannelLevel]
audioChannelMapping_inputChannelLevels = Lens.lens (\AudioChannelMapping' {inputChannelLevels} -> inputChannelLevels) (\s@AudioChannelMapping' {} a -> s {inputChannelLevels = a} :: AudioChannelMapping) Prelude.. Lens.coerced

instance Data.FromJSON AudioChannelMapping where
  parseJSON =
    Data.withObject
      "AudioChannelMapping"
      ( \x ->
          AudioChannelMapping'
            Prelude.<$> (x Data..: "outputChannel")
            Prelude.<*> ( x
                            Data..:? "inputChannelLevels"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AudioChannelMapping where
  hashWithSalt _salt AudioChannelMapping' {..} =
    _salt
      `Prelude.hashWithSalt` outputChannel
      `Prelude.hashWithSalt` inputChannelLevels

instance Prelude.NFData AudioChannelMapping where
  rnf AudioChannelMapping' {..} =
    Prelude.rnf outputChannel
      `Prelude.seq` Prelude.rnf inputChannelLevels

instance Data.ToJSON AudioChannelMapping where
  toJSON AudioChannelMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("outputChannel" Data..= outputChannel),
            Prelude.Just
              ("inputChannelLevels" Data..= inputChannelLevels)
          ]
      )
