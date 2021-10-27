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
-- Module      : Network.AWS.MediaPackageVOD.Types.HlsPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackageVOD.Types.HlsPackage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackageVOD.Types.HlsEncryption
import Network.AWS.MediaPackageVOD.Types.HlsManifest
import qualified Network.AWS.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) packaging configuration.
--
-- /See:/ 'newHlsPackage' smart constructor.
data HlsPackage = HlsPackage'
  { -- | When enabled, audio streams will be placed in rendition groups in the
    -- output.
    useAudioRenditionGroup :: Prelude.Maybe Prelude.Bool,
    -- | When enabled, MediaPackage passes through digital video broadcasting
    -- (DVB) subtitles into the output.
    includeDvbSubtitles :: Prelude.Maybe Prelude.Bool,
    -- | Duration (in seconds) of each fragment. Actual fragments will be rounded
    -- to the nearest multiple of the source fragment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    encryption :: Prelude.Maybe HlsEncryption,
    -- | A list of HLS manifest configurations.
    hlsManifests :: [HlsManifest]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useAudioRenditionGroup', 'hlsPackage_useAudioRenditionGroup' - When enabled, audio streams will be placed in rendition groups in the
-- output.
--
-- 'includeDvbSubtitles', 'hlsPackage_includeDvbSubtitles' - When enabled, MediaPackage passes through digital video broadcasting
-- (DVB) subtitles into the output.
--
-- 'segmentDurationSeconds', 'hlsPackage_segmentDurationSeconds' - Duration (in seconds) of each fragment. Actual fragments will be rounded
-- to the nearest multiple of the source fragment duration.
--
-- 'encryption', 'hlsPackage_encryption' - Undocumented member.
--
-- 'hlsManifests', 'hlsPackage_hlsManifests' - A list of HLS manifest configurations.
newHlsPackage ::
  HlsPackage
newHlsPackage =
  HlsPackage'
    { useAudioRenditionGroup =
        Prelude.Nothing,
      includeDvbSubtitles = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      encryption = Prelude.Nothing,
      hlsManifests = Prelude.mempty
    }

-- | When enabled, audio streams will be placed in rendition groups in the
-- output.
hlsPackage_useAudioRenditionGroup :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Bool)
hlsPackage_useAudioRenditionGroup = Lens.lens (\HlsPackage' {useAudioRenditionGroup} -> useAudioRenditionGroup) (\s@HlsPackage' {} a -> s {useAudioRenditionGroup = a} :: HlsPackage)

-- | When enabled, MediaPackage passes through digital video broadcasting
-- (DVB) subtitles into the output.
hlsPackage_includeDvbSubtitles :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Bool)
hlsPackage_includeDvbSubtitles = Lens.lens (\HlsPackage' {includeDvbSubtitles} -> includeDvbSubtitles) (\s@HlsPackage' {} a -> s {includeDvbSubtitles = a} :: HlsPackage)

-- | Duration (in seconds) of each fragment. Actual fragments will be rounded
-- to the nearest multiple of the source fragment duration.
hlsPackage_segmentDurationSeconds :: Lens.Lens' HlsPackage (Prelude.Maybe Prelude.Int)
hlsPackage_segmentDurationSeconds = Lens.lens (\HlsPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@HlsPackage' {} a -> s {segmentDurationSeconds = a} :: HlsPackage)

-- | Undocumented member.
hlsPackage_encryption :: Lens.Lens' HlsPackage (Prelude.Maybe HlsEncryption)
hlsPackage_encryption = Lens.lens (\HlsPackage' {encryption} -> encryption) (\s@HlsPackage' {} a -> s {encryption = a} :: HlsPackage)

-- | A list of HLS manifest configurations.
hlsPackage_hlsManifests :: Lens.Lens' HlsPackage [HlsManifest]
hlsPackage_hlsManifests = Lens.lens (\HlsPackage' {hlsManifests} -> hlsManifests) (\s@HlsPackage' {} a -> s {hlsManifests = a} :: HlsPackage) Prelude.. Lens.coerced

instance Core.FromJSON HlsPackage where
  parseJSON =
    Core.withObject
      "HlsPackage"
      ( \x ->
          HlsPackage'
            Prelude.<$> (x Core..:? "useAudioRenditionGroup")
            Prelude.<*> (x Core..:? "includeDvbSubtitles")
            Prelude.<*> (x Core..:? "segmentDurationSeconds")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "hlsManifests" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable HlsPackage

instance Prelude.NFData HlsPackage

instance Core.ToJSON HlsPackage where
  toJSON HlsPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("useAudioRenditionGroup" Core..=)
              Prelude.<$> useAudioRenditionGroup,
            ("includeDvbSubtitles" Core..=)
              Prelude.<$> includeDvbSubtitles,
            ("segmentDurationSeconds" Core..=)
              Prelude.<$> segmentDurationSeconds,
            ("encryption" Core..=) Prelude.<$> encryption,
            Prelude.Just ("hlsManifests" Core..= hlsManifests)
          ]
      )
