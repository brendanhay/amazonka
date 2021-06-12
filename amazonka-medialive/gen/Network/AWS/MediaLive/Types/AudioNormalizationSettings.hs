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
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl

-- | Audio Normalization Settings
--
-- /See:/ 'newAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { -- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act
    -- specification, itu17702 conforms to the EBU R-128 specification.
    algorithm :: Core.Maybe AudioNormalizationAlgorithm,
    -- | Target LKFS(loudness) to adjust volume to. If no value is entered, a
    -- default value will be used according to the chosen algorithm. The CALM
    -- Act (1770-1) recommends a target of -24 LKFS. The EBU R-128
    -- specification (1770-2) recommends a target of -23 LKFS.
    targetLkfs :: Core.Maybe Core.Double,
    -- | When set to correctAudio the output audio is corrected using the chosen
    -- algorithm. If set to measureOnly, the audio will be measured but not
    -- adjusted.
    algorithmControl :: Core.Maybe AudioNormalizationAlgorithmControl
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AudioNormalizationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithm', 'audioNormalizationSettings_algorithm' - Audio normalization algorithm to use. itu17701 conforms to the CALM Act
-- specification, itu17702 conforms to the EBU R-128 specification.
--
-- 'targetLkfs', 'audioNormalizationSettings_targetLkfs' - Target LKFS(loudness) to adjust volume to. If no value is entered, a
-- default value will be used according to the chosen algorithm. The CALM
-- Act (1770-1) recommends a target of -24 LKFS. The EBU R-128
-- specification (1770-2) recommends a target of -23 LKFS.
--
-- 'algorithmControl', 'audioNormalizationSettings_algorithmControl' - When set to correctAudio the output audio is corrected using the chosen
-- algorithm. If set to measureOnly, the audio will be measured but not
-- adjusted.
newAudioNormalizationSettings ::
  AudioNormalizationSettings
newAudioNormalizationSettings =
  AudioNormalizationSettings'
    { algorithm =
        Core.Nothing,
      targetLkfs = Core.Nothing,
      algorithmControl = Core.Nothing
    }

-- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act
-- specification, itu17702 conforms to the EBU R-128 specification.
audioNormalizationSettings_algorithm :: Lens.Lens' AudioNormalizationSettings (Core.Maybe AudioNormalizationAlgorithm)
audioNormalizationSettings_algorithm = Lens.lens (\AudioNormalizationSettings' {algorithm} -> algorithm) (\s@AudioNormalizationSettings' {} a -> s {algorithm = a} :: AudioNormalizationSettings)

-- | Target LKFS(loudness) to adjust volume to. If no value is entered, a
-- default value will be used according to the chosen algorithm. The CALM
-- Act (1770-1) recommends a target of -24 LKFS. The EBU R-128
-- specification (1770-2) recommends a target of -23 LKFS.
audioNormalizationSettings_targetLkfs :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Core.Double)
audioNormalizationSettings_targetLkfs = Lens.lens (\AudioNormalizationSettings' {targetLkfs} -> targetLkfs) (\s@AudioNormalizationSettings' {} a -> s {targetLkfs = a} :: AudioNormalizationSettings)

-- | When set to correctAudio the output audio is corrected using the chosen
-- algorithm. If set to measureOnly, the audio will be measured but not
-- adjusted.
audioNormalizationSettings_algorithmControl :: Lens.Lens' AudioNormalizationSettings (Core.Maybe AudioNormalizationAlgorithmControl)
audioNormalizationSettings_algorithmControl = Lens.lens (\AudioNormalizationSettings' {algorithmControl} -> algorithmControl) (\s@AudioNormalizationSettings' {} a -> s {algorithmControl = a} :: AudioNormalizationSettings)

instance Core.FromJSON AudioNormalizationSettings where
  parseJSON =
    Core.withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            Core.<$> (x Core..:? "algorithm")
            Core.<*> (x Core..:? "targetLkfs")
            Core.<*> (x Core..:? "algorithmControl")
      )

instance Core.Hashable AudioNormalizationSettings

instance Core.NFData AudioNormalizationSettings

instance Core.ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("algorithm" Core..=) Core.<$> algorithm,
            ("targetLkfs" Core..=) Core.<$> targetLkfs,
            ("algorithmControl" Core..=)
              Core.<$> algorithmControl
          ]
      )
