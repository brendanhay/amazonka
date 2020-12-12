{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsOutputSettings
  ( HlsOutputSettings (..),

    -- * Smart constructor
    mkHlsOutputSettings,

    -- * Lenses
    hosH265PackagingType,
    hosSegmentModifier,
    hosNameModifier,
    hosHlsSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.HlsH265PackagingType
import Network.AWS.MediaLive.Types.HlsSettings
import qualified Network.AWS.Prelude as Lude

-- | Hls Output Settings
--
-- /See:/ 'mkHlsOutputSettings' smart constructor.
data HlsOutputSettings = HlsOutputSettings'
  { h265PackagingType ::
      Lude.Maybe HlsH265PackagingType,
    segmentModifier :: Lude.Maybe Lude.Text,
    nameModifier :: Lude.Maybe Lude.Text,
    hlsSettings :: HlsSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsOutputSettings' with the minimum fields required to make a request.
--
-- * 'h265PackagingType' - Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
-- * 'hlsSettings' - Settings regarding the underlying stream. These settings are different for audio-only outputs.
-- * 'nameModifier' - String concatenated to the end of the destination filename. Accepts \"Format Identifiers\":#formatIdentifierParameters.
-- * 'segmentModifier' - String concatenated to end of segment filenames.
mkHlsOutputSettings ::
  -- | 'hlsSettings'
  HlsSettings ->
  HlsOutputSettings
mkHlsOutputSettings pHlsSettings_ =
  HlsOutputSettings'
    { h265PackagingType = Lude.Nothing,
      segmentModifier = Lude.Nothing,
      nameModifier = Lude.Nothing,
      hlsSettings = pHlsSettings_
    }

-- | Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- /Note:/ Consider using 'h265PackagingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosH265PackagingType :: Lens.Lens' HlsOutputSettings (Lude.Maybe HlsH265PackagingType)
hosH265PackagingType = Lens.lens (h265PackagingType :: HlsOutputSettings -> Lude.Maybe HlsH265PackagingType) (\s a -> s {h265PackagingType = a} :: HlsOutputSettings)
{-# DEPRECATED hosH265PackagingType "Use generic-lens or generic-optics with 'h265PackagingType' instead." #-}

-- | String concatenated to end of segment filenames.
--
-- /Note:/ Consider using 'segmentModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosSegmentModifier :: Lens.Lens' HlsOutputSettings (Lude.Maybe Lude.Text)
hosSegmentModifier = Lens.lens (segmentModifier :: HlsOutputSettings -> Lude.Maybe Lude.Text) (\s a -> s {segmentModifier = a} :: HlsOutputSettings)
{-# DEPRECATED hosSegmentModifier "Use generic-lens or generic-optics with 'segmentModifier' instead." #-}

-- | String concatenated to the end of the destination filename. Accepts \"Format Identifiers\":#formatIdentifierParameters.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosNameModifier :: Lens.Lens' HlsOutputSettings (Lude.Maybe Lude.Text)
hosNameModifier = Lens.lens (nameModifier :: HlsOutputSettings -> Lude.Maybe Lude.Text) (\s a -> s {nameModifier = a} :: HlsOutputSettings)
{-# DEPRECATED hosNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

-- | Settings regarding the underlying stream. These settings are different for audio-only outputs.
--
-- /Note:/ Consider using 'hlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosHlsSettings :: Lens.Lens' HlsOutputSettings HlsSettings
hosHlsSettings = Lens.lens (hlsSettings :: HlsOutputSettings -> HlsSettings) (\s a -> s {hlsSettings = a} :: HlsOutputSettings)
{-# DEPRECATED hosHlsSettings "Use generic-lens or generic-optics with 'hlsSettings' instead." #-}

instance Lude.FromJSON HlsOutputSettings where
  parseJSON =
    Lude.withObject
      "HlsOutputSettings"
      ( \x ->
          HlsOutputSettings'
            Lude.<$> (x Lude..:? "h265PackagingType")
            Lude.<*> (x Lude..:? "segmentModifier")
            Lude.<*> (x Lude..:? "nameModifier")
            Lude.<*> (x Lude..: "hlsSettings")
      )

instance Lude.ToJSON HlsOutputSettings where
  toJSON HlsOutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("h265PackagingType" Lude..=) Lude.<$> h265PackagingType,
            ("segmentModifier" Lude..=) Lude.<$> segmentModifier,
            ("nameModifier" Lude..=) Lude.<$> nameModifier,
            Lude.Just ("hlsSettings" Lude..= hlsSettings)
          ]
      )
