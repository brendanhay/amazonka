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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.AudioArtifactsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.AudioArtifactsConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.AudioMuxType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The audio artifact configuration object.
--
-- /See:/ 'newAudioArtifactsConfiguration' smart constructor.
data AudioArtifactsConfiguration = AudioArtifactsConfiguration'
  { -- | The MUX type of the audio artifact configuration object.
    muxType :: AudioMuxType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioArtifactsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'muxType', 'audioArtifactsConfiguration_muxType' - The MUX type of the audio artifact configuration object.
newAudioArtifactsConfiguration ::
  -- | 'muxType'
  AudioMuxType ->
  AudioArtifactsConfiguration
newAudioArtifactsConfiguration pMuxType_ =
  AudioArtifactsConfiguration' {muxType = pMuxType_}

-- | The MUX type of the audio artifact configuration object.
audioArtifactsConfiguration_muxType :: Lens.Lens' AudioArtifactsConfiguration AudioMuxType
audioArtifactsConfiguration_muxType = Lens.lens (\AudioArtifactsConfiguration' {muxType} -> muxType) (\s@AudioArtifactsConfiguration' {} a -> s {muxType = a} :: AudioArtifactsConfiguration)

instance Core.FromJSON AudioArtifactsConfiguration where
  parseJSON =
    Core.withObject
      "AudioArtifactsConfiguration"
      ( \x ->
          AudioArtifactsConfiguration'
            Prelude.<$> (x Core..: "MuxType")
      )

instance Prelude.Hashable AudioArtifactsConfiguration where
  hashWithSalt _salt AudioArtifactsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` muxType

instance Prelude.NFData AudioArtifactsConfiguration where
  rnf AudioArtifactsConfiguration' {..} =
    Prelude.rnf muxType

instance Core.ToJSON AudioArtifactsConfiguration where
  toJSON AudioArtifactsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("MuxType" Core..= muxType)]
      )
