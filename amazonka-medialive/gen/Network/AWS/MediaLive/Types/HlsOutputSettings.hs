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
-- Module      : Network.AWS.MediaLive.Types.HlsOutputSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsOutputSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsH265PackagingType
import Network.AWS.MediaLive.Types.HlsSettings
import qualified Network.AWS.Prelude as Prelude

-- | Hls Output Settings
--
-- /See:/ 'newHlsOutputSettings' smart constructor.
data HlsOutputSettings = HlsOutputSettings'
  { -- | String concatenated to end of segment filenames.
    segmentModifier :: Prelude.Maybe Prelude.Text,
    -- | Only applicable when this output is referencing an H.265 video
    -- description. Specifies whether MP4 segments should be packaged as HEV1
    -- or HVC1.
    h265PackagingType :: Prelude.Maybe HlsH265PackagingType,
    -- | String concatenated to the end of the destination filename. Accepts
    -- \\\"Format Identifiers\\\":#formatIdentifierParameters.
    nameModifier :: Prelude.Maybe Prelude.Text,
    -- | Settings regarding the underlying stream. These settings are different
    -- for audio-only outputs.
    hlsSettings :: HlsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HlsOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentModifier', 'hlsOutputSettings_segmentModifier' - String concatenated to end of segment filenames.
--
-- 'h265PackagingType', 'hlsOutputSettings_h265PackagingType' - Only applicable when this output is referencing an H.265 video
-- description. Specifies whether MP4 segments should be packaged as HEV1
-- or HVC1.
--
-- 'nameModifier', 'hlsOutputSettings_nameModifier' - String concatenated to the end of the destination filename. Accepts
-- \\\"Format Identifiers\\\":#formatIdentifierParameters.
--
-- 'hlsSettings', 'hlsOutputSettings_hlsSettings' - Settings regarding the underlying stream. These settings are different
-- for audio-only outputs.
newHlsOutputSettings ::
  -- | 'hlsSettings'
  HlsSettings ->
  HlsOutputSettings
newHlsOutputSettings pHlsSettings_ =
  HlsOutputSettings'
    { segmentModifier =
        Prelude.Nothing,
      h265PackagingType = Prelude.Nothing,
      nameModifier = Prelude.Nothing,
      hlsSettings = pHlsSettings_
    }

-- | String concatenated to end of segment filenames.
hlsOutputSettings_segmentModifier :: Lens.Lens' HlsOutputSettings (Prelude.Maybe Prelude.Text)
hlsOutputSettings_segmentModifier = Lens.lens (\HlsOutputSettings' {segmentModifier} -> segmentModifier) (\s@HlsOutputSettings' {} a -> s {segmentModifier = a} :: HlsOutputSettings)

-- | Only applicable when this output is referencing an H.265 video
-- description. Specifies whether MP4 segments should be packaged as HEV1
-- or HVC1.
hlsOutputSettings_h265PackagingType :: Lens.Lens' HlsOutputSettings (Prelude.Maybe HlsH265PackagingType)
hlsOutputSettings_h265PackagingType = Lens.lens (\HlsOutputSettings' {h265PackagingType} -> h265PackagingType) (\s@HlsOutputSettings' {} a -> s {h265PackagingType = a} :: HlsOutputSettings)

-- | String concatenated to the end of the destination filename. Accepts
-- \\\"Format Identifiers\\\":#formatIdentifierParameters.
hlsOutputSettings_nameModifier :: Lens.Lens' HlsOutputSettings (Prelude.Maybe Prelude.Text)
hlsOutputSettings_nameModifier = Lens.lens (\HlsOutputSettings' {nameModifier} -> nameModifier) (\s@HlsOutputSettings' {} a -> s {nameModifier = a} :: HlsOutputSettings)

-- | Settings regarding the underlying stream. These settings are different
-- for audio-only outputs.
hlsOutputSettings_hlsSettings :: Lens.Lens' HlsOutputSettings HlsSettings
hlsOutputSettings_hlsSettings = Lens.lens (\HlsOutputSettings' {hlsSettings} -> hlsSettings) (\s@HlsOutputSettings' {} a -> s {hlsSettings = a} :: HlsOutputSettings)

instance Prelude.FromJSON HlsOutputSettings where
  parseJSON =
    Prelude.withObject
      "HlsOutputSettings"
      ( \x ->
          HlsOutputSettings'
            Prelude.<$> (x Prelude..:? "segmentModifier")
            Prelude.<*> (x Prelude..:? "h265PackagingType")
            Prelude.<*> (x Prelude..:? "nameModifier")
            Prelude.<*> (x Prelude..: "hlsSettings")
      )

instance Prelude.Hashable HlsOutputSettings

instance Prelude.NFData HlsOutputSettings

instance Prelude.ToJSON HlsOutputSettings where
  toJSON HlsOutputSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("segmentModifier" Prelude..=)
              Prelude.<$> segmentModifier,
            ("h265PackagingType" Prelude..=)
              Prelude.<$> h265PackagingType,
            ("nameModifier" Prelude..=) Prelude.<$> nameModifier,
            Prelude.Just ("hlsSettings" Prelude..= hlsSettings)
          ]
      )
