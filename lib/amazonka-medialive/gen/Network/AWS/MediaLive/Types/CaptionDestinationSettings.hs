-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDestinationSettings
  ( CaptionDestinationSettings (..),

    -- * Smart constructor
    mkCaptionDestinationSettings,

    -- * Lenses
    cdsTeletextDestinationSettings,
    cdsEbuTtDDestinationSettings,
    cdsRtmpCaptionInfoDestinationSettings,
    cdsDvbSubDestinationSettings,
    cdsScte27DestinationSettings,
    cdsTtmlDestinationSettings,
    cdsScte20PlusEmbeddedDestinationSettings,
    cdsEmbeddedPlusScte20DestinationSettings,
    cdsSmpteTtDestinationSettings,
    cdsWebvttDestinationSettings,
    cdsEmbeddedDestinationSettings,
    cdsBurnInDestinationSettings,
    cdsAribDestinationSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AribDestinationSettings
import Network.AWS.MediaLive.Types.BurnInDestinationSettings
import Network.AWS.MediaLive.Types.DvbSubDestinationSettings
import Network.AWS.MediaLive.Types.EbuTtDDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.EmbeddedPlusScte20DestinationSettings
import Network.AWS.MediaLive.Types.RtmpCaptionInfoDestinationSettings
import Network.AWS.MediaLive.Types.Scte20PlusEmbeddedDestinationSettings
import Network.AWS.MediaLive.Types.Scte27DestinationSettings
import Network.AWS.MediaLive.Types.SmpteTtDestinationSettings
import Network.AWS.MediaLive.Types.TeletextDestinationSettings
import Network.AWS.MediaLive.Types.TtmlDestinationSettings
import Network.AWS.MediaLive.Types.WebvttDestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Caption Destination Settings
--
-- /See:/ 'mkCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { teletextDestinationSettings ::
      Lude.Maybe
        TeletextDestinationSettings,
    ebuTtDDestinationSettings ::
      Lude.Maybe EbuTtDDestinationSettings,
    rtmpCaptionInfoDestinationSettings ::
      Lude.Maybe
        RtmpCaptionInfoDestinationSettings,
    dvbSubDestinationSettings ::
      Lude.Maybe DvbSubDestinationSettings,
    scte27DestinationSettings ::
      Lude.Maybe Scte27DestinationSettings,
    ttmlDestinationSettings ::
      Lude.Maybe TtmlDestinationSettings,
    scte20PlusEmbeddedDestinationSettings ::
      Lude.Maybe
        Scte20PlusEmbeddedDestinationSettings,
    embeddedPlusScte20DestinationSettings ::
      Lude.Maybe
        EmbeddedPlusScte20DestinationSettings,
    smpteTtDestinationSettings ::
      Lude.Maybe SmpteTtDestinationSettings,
    webvttDestinationSettings ::
      Lude.Maybe WebvttDestinationSettings,
    embeddedDestinationSettings ::
      Lude.Maybe
        EmbeddedDestinationSettings,
    burnInDestinationSettings ::
      Lude.Maybe BurnInDestinationSettings,
    aribDestinationSettings ::
      Lude.Maybe AribDestinationSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionDestinationSettings' with the minimum fields required to make a request.
--
-- * 'aribDestinationSettings' - Undocumented field.
-- * 'burnInDestinationSettings' - Undocumented field.
-- * 'dvbSubDestinationSettings' - Undocumented field.
-- * 'ebuTtDDestinationSettings' - Undocumented field.
-- * 'embeddedDestinationSettings' - Undocumented field.
-- * 'embeddedPlusScte20DestinationSettings' - Undocumented field.
-- * 'rtmpCaptionInfoDestinationSettings' - Undocumented field.
-- * 'scte20PlusEmbeddedDestinationSettings' - Undocumented field.
-- * 'scte27DestinationSettings' - Undocumented field.
-- * 'smpteTtDestinationSettings' - Undocumented field.
-- * 'teletextDestinationSettings' - Undocumented field.
-- * 'ttmlDestinationSettings' - Undocumented field.
-- * 'webvttDestinationSettings' - Undocumented field.
mkCaptionDestinationSettings ::
  CaptionDestinationSettings
mkCaptionDestinationSettings =
  CaptionDestinationSettings'
    { teletextDestinationSettings =
        Lude.Nothing,
      ebuTtDDestinationSettings = Lude.Nothing,
      rtmpCaptionInfoDestinationSettings = Lude.Nothing,
      dvbSubDestinationSettings = Lude.Nothing,
      scte27DestinationSettings = Lude.Nothing,
      ttmlDestinationSettings = Lude.Nothing,
      scte20PlusEmbeddedDestinationSettings = Lude.Nothing,
      embeddedPlusScte20DestinationSettings = Lude.Nothing,
      smpteTtDestinationSettings = Lude.Nothing,
      webvttDestinationSettings = Lude.Nothing,
      embeddedDestinationSettings = Lude.Nothing,
      burnInDestinationSettings = Lude.Nothing,
      aribDestinationSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'teletextDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTeletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe TeletextDestinationSettings)
cdsTeletextDestinationSettings = Lens.lens (teletextDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe TeletextDestinationSettings) (\s a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsTeletextDestinationSettings "Use generic-lens or generic-optics with 'teletextDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ebuTtDDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEbuTtDDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe EbuTtDDestinationSettings)
cdsEbuTtDDestinationSettings = Lens.lens (ebuTtDDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe EbuTtDDestinationSettings) (\s a -> s {ebuTtDDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsEbuTtDDestinationSettings "Use generic-lens or generic-optics with 'ebuTtDDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rtmpCaptionInfoDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsRtmpCaptionInfoDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe RtmpCaptionInfoDestinationSettings)
cdsRtmpCaptionInfoDestinationSettings = Lens.lens (rtmpCaptionInfoDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe RtmpCaptionInfoDestinationSettings) (\s a -> s {rtmpCaptionInfoDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsRtmpCaptionInfoDestinationSettings "Use generic-lens or generic-optics with 'rtmpCaptionInfoDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbSubDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = Lens.lens (dvbSubDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe DvbSubDestinationSettings) (\s a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsDvbSubDestinationSettings "Use generic-lens or generic-optics with 'dvbSubDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte27DestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsScte27DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe Scte27DestinationSettings)
cdsScte27DestinationSettings = Lens.lens (scte27DestinationSettings :: CaptionDestinationSettings -> Lude.Maybe Scte27DestinationSettings) (\s a -> s {scte27DestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsScte27DestinationSettings "Use generic-lens or generic-optics with 'scte27DestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ttmlDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTtmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe TtmlDestinationSettings)
cdsTtmlDestinationSettings = Lens.lens (ttmlDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe TtmlDestinationSettings) (\s a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsTtmlDestinationSettings "Use generic-lens or generic-optics with 'ttmlDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte20PlusEmbeddedDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsScte20PlusEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe Scte20PlusEmbeddedDestinationSettings)
cdsScte20PlusEmbeddedDestinationSettings = Lens.lens (scte20PlusEmbeddedDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe Scte20PlusEmbeddedDestinationSettings) (\s a -> s {scte20PlusEmbeddedDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsScte20PlusEmbeddedDestinationSettings "Use generic-lens or generic-optics with 'scte20PlusEmbeddedDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'embeddedPlusScte20DestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEmbeddedPlusScte20DestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe EmbeddedPlusScte20DestinationSettings)
cdsEmbeddedPlusScte20DestinationSettings = Lens.lens (embeddedPlusScte20DestinationSettings :: CaptionDestinationSettings -> Lude.Maybe EmbeddedPlusScte20DestinationSettings) (\s a -> s {embeddedPlusScte20DestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsEmbeddedPlusScte20DestinationSettings "Use generic-lens or generic-optics with 'embeddedPlusScte20DestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'smpteTtDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsSmpteTtDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe SmpteTtDestinationSettings)
cdsSmpteTtDestinationSettings = Lens.lens (smpteTtDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe SmpteTtDestinationSettings) (\s a -> s {smpteTtDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsSmpteTtDestinationSettings "Use generic-lens or generic-optics with 'smpteTtDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'webvttDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsWebvttDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe WebvttDestinationSettings)
cdsWebvttDestinationSettings = Lens.lens (webvttDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe WebvttDestinationSettings) (\s a -> s {webvttDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsWebvttDestinationSettings "Use generic-lens or generic-optics with 'webvttDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'embeddedDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = Lens.lens (embeddedDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe EmbeddedDestinationSettings) (\s a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsEmbeddedDestinationSettings "Use generic-lens or generic-optics with 'embeddedDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'burnInDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsBurnInDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe BurnInDestinationSettings)
cdsBurnInDestinationSettings = Lens.lens (burnInDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe BurnInDestinationSettings) (\s a -> s {burnInDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsBurnInDestinationSettings "Use generic-lens or generic-optics with 'burnInDestinationSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aribDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsAribDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe AribDestinationSettings)
cdsAribDestinationSettings = Lens.lens (aribDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe AribDestinationSettings) (\s a -> s {aribDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsAribDestinationSettings "Use generic-lens or generic-optics with 'aribDestinationSettings' instead." #-}

instance Lude.FromJSON CaptionDestinationSettings where
  parseJSON =
    Lude.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Lude.<$> (x Lude..:? "teletextDestinationSettings")
            Lude.<*> (x Lude..:? "ebuTtDDestinationSettings")
            Lude.<*> (x Lude..:? "rtmpCaptionInfoDestinationSettings")
            Lude.<*> (x Lude..:? "dvbSubDestinationSettings")
            Lude.<*> (x Lude..:? "scte27DestinationSettings")
            Lude.<*> (x Lude..:? "ttmlDestinationSettings")
            Lude.<*> (x Lude..:? "scte20PlusEmbeddedDestinationSettings")
            Lude.<*> (x Lude..:? "embeddedPlusScte20DestinationSettings")
            Lude.<*> (x Lude..:? "smpteTtDestinationSettings")
            Lude.<*> (x Lude..:? "webvttDestinationSettings")
            Lude.<*> (x Lude..:? "embeddedDestinationSettings")
            Lude.<*> (x Lude..:? "burnInDestinationSettings")
            Lude.<*> (x Lude..:? "aribDestinationSettings")
      )

instance Lude.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("teletextDestinationSettings" Lude..=)
              Lude.<$> teletextDestinationSettings,
            ("ebuTtDDestinationSettings" Lude..=)
              Lude.<$> ebuTtDDestinationSettings,
            ("rtmpCaptionInfoDestinationSettings" Lude..=)
              Lude.<$> rtmpCaptionInfoDestinationSettings,
            ("dvbSubDestinationSettings" Lude..=)
              Lude.<$> dvbSubDestinationSettings,
            ("scte27DestinationSettings" Lude..=)
              Lude.<$> scte27DestinationSettings,
            ("ttmlDestinationSettings" Lude..=)
              Lude.<$> ttmlDestinationSettings,
            ("scte20PlusEmbeddedDestinationSettings" Lude..=)
              Lude.<$> scte20PlusEmbeddedDestinationSettings,
            ("embeddedPlusScte20DestinationSettings" Lude..=)
              Lude.<$> embeddedPlusScte20DestinationSettings,
            ("smpteTtDestinationSettings" Lude..=)
              Lude.<$> smpteTtDestinationSettings,
            ("webvttDestinationSettings" Lude..=)
              Lude.<$> webvttDestinationSettings,
            ("embeddedDestinationSettings" Lude..=)
              Lude.<$> embeddedDestinationSettings,
            ("burnInDestinationSettings" Lude..=)
              Lude.<$> burnInDestinationSettings,
            ("aribDestinationSettings" Lude..=)
              Lude.<$> aribDestinationSettings
          ]
      )
