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
-- Module      : Network.AWS.MediaLive.Types.AudioChannelMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioChannelMapping where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputChannelLevel
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
audioChannelMapping_inputChannelLevels = Lens.lens (\AudioChannelMapping' {inputChannelLevels} -> inputChannelLevels) (\s@AudioChannelMapping' {} a -> s {inputChannelLevels = a} :: AudioChannelMapping) Prelude.. Prelude._Coerce

instance Prelude.FromJSON AudioChannelMapping where
  parseJSON =
    Prelude.withObject
      "AudioChannelMapping"
      ( \x ->
          AudioChannelMapping'
            Prelude.<$> (x Prelude..: "outputChannel")
            Prelude.<*> ( x Prelude..:? "inputChannelLevels"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AudioChannelMapping

instance Prelude.NFData AudioChannelMapping

instance Prelude.ToJSON AudioChannelMapping where
  toJSON AudioChannelMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("outputChannel" Prelude..= outputChannel),
            Prelude.Just
              ( "inputChannelLevels"
                  Prelude..= inputChannelLevels
              )
          ]
      )
