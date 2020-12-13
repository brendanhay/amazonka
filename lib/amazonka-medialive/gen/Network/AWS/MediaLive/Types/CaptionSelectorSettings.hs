{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelectorSettings
  ( CaptionSelectorSettings (..),

    -- * Smart constructor
    mkCaptionSelectorSettings,

    -- * Lenses
    cssTeletextSourceSettings,
    cssAribSourceSettings,
    cssScte27SourceSettings,
    cssDvbSubSourceSettings,
    cssAncillarySourceSettings,
    cssScte20SourceSettings,
    cssEmbeddedSourceSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AncillarySourceSettings
import Network.AWS.MediaLive.Types.AribSourceSettings
import Network.AWS.MediaLive.Types.DvbSubSourceSettings
import Network.AWS.MediaLive.Types.EmbeddedSourceSettings
import Network.AWS.MediaLive.Types.Scte20SourceSettings
import Network.AWS.MediaLive.Types.Scte27SourceSettings
import Network.AWS.MediaLive.Types.TeletextSourceSettings
import qualified Network.AWS.Prelude as Lude

-- | Caption Selector Settings
--
-- /See:/ 'mkCaptionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { teletextSourceSettings :: Lude.Maybe TeletextSourceSettings,
    aribSourceSettings :: Lude.Maybe AribSourceSettings,
    scte27SourceSettings :: Lude.Maybe Scte27SourceSettings,
    dvbSubSourceSettings :: Lude.Maybe DvbSubSourceSettings,
    ancillarySourceSettings :: Lude.Maybe AncillarySourceSettings,
    scte20SourceSettings :: Lude.Maybe Scte20SourceSettings,
    embeddedSourceSettings :: Lude.Maybe EmbeddedSourceSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionSelectorSettings' with the minimum fields required to make a request.
--
-- * 'teletextSourceSettings' -
-- * 'aribSourceSettings' -
-- * 'scte27SourceSettings' -
-- * 'dvbSubSourceSettings' -
-- * 'ancillarySourceSettings' -
-- * 'scte20SourceSettings' -
-- * 'embeddedSourceSettings' -
mkCaptionSelectorSettings ::
  CaptionSelectorSettings
mkCaptionSelectorSettings =
  CaptionSelectorSettings'
    { teletextSourceSettings = Lude.Nothing,
      aribSourceSettings = Lude.Nothing,
      scte27SourceSettings = Lude.Nothing,
      dvbSubSourceSettings = Lude.Nothing,
      ancillarySourceSettings = Lude.Nothing,
      scte20SourceSettings = Lude.Nothing,
      embeddedSourceSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'teletextSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTeletextSourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe TeletextSourceSettings)
cssTeletextSourceSettings = Lens.lens (teletextSourceSettings :: CaptionSelectorSettings -> Lude.Maybe TeletextSourceSettings) (\s a -> s {teletextSourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssTeletextSourceSettings "Use generic-lens or generic-optics with 'teletextSourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aribSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAribSourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe AribSourceSettings)
cssAribSourceSettings = Lens.lens (aribSourceSettings :: CaptionSelectorSettings -> Lude.Maybe AribSourceSettings) (\s a -> s {aribSourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssAribSourceSettings "Use generic-lens or generic-optics with 'aribSourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte27SourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScte27SourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe Scte27SourceSettings)
cssScte27SourceSettings = Lens.lens (scte27SourceSettings :: CaptionSelectorSettings -> Lude.Maybe Scte27SourceSettings) (\s a -> s {scte27SourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssScte27SourceSettings "Use generic-lens or generic-optics with 'scte27SourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbSubSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDvbSubSourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe DvbSubSourceSettings)
cssDvbSubSourceSettings = Lens.lens (dvbSubSourceSettings :: CaptionSelectorSettings -> Lude.Maybe DvbSubSourceSettings) (\s a -> s {dvbSubSourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssDvbSubSourceSettings "Use generic-lens or generic-optics with 'dvbSubSourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ancillarySourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAncillarySourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe AncillarySourceSettings)
cssAncillarySourceSettings = Lens.lens (ancillarySourceSettings :: CaptionSelectorSettings -> Lude.Maybe AncillarySourceSettings) (\s a -> s {ancillarySourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssAncillarySourceSettings "Use generic-lens or generic-optics with 'ancillarySourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte20SourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScte20SourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe Scte20SourceSettings)
cssScte20SourceSettings = Lens.lens (scte20SourceSettings :: CaptionSelectorSettings -> Lude.Maybe Scte20SourceSettings) (\s a -> s {scte20SourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssScte20SourceSettings "Use generic-lens or generic-optics with 'scte20SourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'embeddedSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssEmbeddedSourceSettings :: Lens.Lens' CaptionSelectorSettings (Lude.Maybe EmbeddedSourceSettings)
cssEmbeddedSourceSettings = Lens.lens (embeddedSourceSettings :: CaptionSelectorSettings -> Lude.Maybe EmbeddedSourceSettings) (\s a -> s {embeddedSourceSettings = a} :: CaptionSelectorSettings)
{-# DEPRECATED cssEmbeddedSourceSettings "Use generic-lens or generic-optics with 'embeddedSourceSettings' instead." #-}

instance Lude.FromJSON CaptionSelectorSettings where
  parseJSON =
    Lude.withObject
      "CaptionSelectorSettings"
      ( \x ->
          CaptionSelectorSettings'
            Lude.<$> (x Lude..:? "teletextSourceSettings")
            Lude.<*> (x Lude..:? "aribSourceSettings")
            Lude.<*> (x Lude..:? "scte27SourceSettings")
            Lude.<*> (x Lude..:? "dvbSubSourceSettings")
            Lude.<*> (x Lude..:? "ancillarySourceSettings")
            Lude.<*> (x Lude..:? "scte20SourceSettings")
            Lude.<*> (x Lude..:? "embeddedSourceSettings")
      )

instance Lude.ToJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("teletextSourceSettings" Lude..=)
              Lude.<$> teletextSourceSettings,
            ("aribSourceSettings" Lude..=) Lude.<$> aribSourceSettings,
            ("scte27SourceSettings" Lude..=) Lude.<$> scte27SourceSettings,
            ("dvbSubSourceSettings" Lude..=) Lude.<$> dvbSubSourceSettings,
            ("ancillarySourceSettings" Lude..=)
              Lude.<$> ancillarySourceSettings,
            ("scte20SourceSettings" Lude..=) Lude.<$> scte20SourceSettings,
            ("embeddedSourceSettings" Lude..=)
              Lude.<$> embeddedSourceSettings
          ]
      )
