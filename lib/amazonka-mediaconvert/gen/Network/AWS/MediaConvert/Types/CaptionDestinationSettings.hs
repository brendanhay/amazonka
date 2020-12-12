{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDestinationSettings
  ( CaptionDestinationSettings (..),

    -- * Smart constructor
    mkCaptionDestinationSettings,

    -- * Lenses
    cdsTeletextDestinationSettings,
    cdsDvbSubDestinationSettings,
    cdsTtmlDestinationSettings,
    cdsDestinationType,
    cdsEmbeddedDestinationSettings,
    cdsSccDestinationSettings,
    cdsBurninDestinationSettings,
    cdsImscDestinationSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.BurninDestinationSettings
import Network.AWS.MediaConvert.Types.CaptionDestinationType
import Network.AWS.MediaConvert.Types.DvbSubDestinationSettings
import Network.AWS.MediaConvert.Types.EmbeddedDestinationSettings
import Network.AWS.MediaConvert.Types.ImscDestinationSettings
import Network.AWS.MediaConvert.Types.SccDestinationSettings
import Network.AWS.MediaConvert.Types.TeletextDestinationSettings
import Network.AWS.MediaConvert.Types.TtmlDestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
--
-- /See:/ 'mkCaptionDestinationSettings' smart constructor.
data CaptionDestinationSettings = CaptionDestinationSettings'
  { teletextDestinationSettings ::
      Lude.Maybe
        TeletextDestinationSettings,
    dvbSubDestinationSettings ::
      Lude.Maybe DvbSubDestinationSettings,
    ttmlDestinationSettings ::
      Lude.Maybe TtmlDestinationSettings,
    destinationType ::
      Lude.Maybe CaptionDestinationType,
    embeddedDestinationSettings ::
      Lude.Maybe
        EmbeddedDestinationSettings,
    sccDestinationSettings ::
      Lude.Maybe SccDestinationSettings,
    burninDestinationSettings ::
      Lude.Maybe BurninDestinationSettings,
    imscDestinationSettings ::
      Lude.Maybe ImscDestinationSettings
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
-- * 'burninDestinationSettings' - Burn-In Destination Settings.
-- * 'destinationType' - Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
-- * 'dvbSubDestinationSettings' - DVB-Sub Destination Settings
-- * 'embeddedDestinationSettings' - Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
-- * 'imscDestinationSettings' - Settings specific to IMSC caption outputs.
-- * 'sccDestinationSettings' - Settings for SCC caption output.
-- * 'teletextDestinationSettings' - Settings for Teletext caption output
-- * 'ttmlDestinationSettings' - Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
mkCaptionDestinationSettings ::
  CaptionDestinationSettings
mkCaptionDestinationSettings =
  CaptionDestinationSettings'
    { teletextDestinationSettings =
        Lude.Nothing,
      dvbSubDestinationSettings = Lude.Nothing,
      ttmlDestinationSettings = Lude.Nothing,
      destinationType = Lude.Nothing,
      embeddedDestinationSettings = Lude.Nothing,
      sccDestinationSettings = Lude.Nothing,
      burninDestinationSettings = Lude.Nothing,
      imscDestinationSettings = Lude.Nothing
    }

-- | Settings for Teletext caption output
--
-- /Note:/ Consider using 'teletextDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTeletextDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe TeletextDestinationSettings)
cdsTeletextDestinationSettings = Lens.lens (teletextDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe TeletextDestinationSettings) (\s a -> s {teletextDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsTeletextDestinationSettings "Use generic-lens or generic-optics with 'teletextDestinationSettings' instead." #-}

-- | DVB-Sub Destination Settings
--
-- /Note:/ Consider using 'dvbSubDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDvbSubDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe DvbSubDestinationSettings)
cdsDvbSubDestinationSettings = Lens.lens (dvbSubDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe DvbSubDestinationSettings) (\s a -> s {dvbSubDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsDvbSubDestinationSettings "Use generic-lens or generic-optics with 'dvbSubDestinationSettings' instead." #-}

-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
--
-- /Note:/ Consider using 'ttmlDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsTtmlDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe TtmlDestinationSettings)
cdsTtmlDestinationSettings = Lens.lens (ttmlDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe TtmlDestinationSettings) (\s a -> s {ttmlDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsTtmlDestinationSettings "Use generic-lens or generic-optics with 'ttmlDestinationSettings' instead." #-}

-- | Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsDestinationType :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe CaptionDestinationType)
cdsDestinationType = Lens.lens (destinationType :: CaptionDestinationSettings -> Lude.Maybe CaptionDestinationType) (\s a -> s {destinationType = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | Settings specific to embedded/ancillary caption outputs, including 608/708 Channel destination number.
--
-- /Note:/ Consider using 'embeddedDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsEmbeddedDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe EmbeddedDestinationSettings)
cdsEmbeddedDestinationSettings = Lens.lens (embeddedDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe EmbeddedDestinationSettings) (\s a -> s {embeddedDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsEmbeddedDestinationSettings "Use generic-lens or generic-optics with 'embeddedDestinationSettings' instead." #-}

-- | Settings for SCC caption output.
--
-- /Note:/ Consider using 'sccDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsSccDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe SccDestinationSettings)
cdsSccDestinationSettings = Lens.lens (sccDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe SccDestinationSettings) (\s a -> s {sccDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsSccDestinationSettings "Use generic-lens or generic-optics with 'sccDestinationSettings' instead." #-}

-- | Burn-In Destination Settings.
--
-- /Note:/ Consider using 'burninDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsBurninDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe BurninDestinationSettings)
cdsBurninDestinationSettings = Lens.lens (burninDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe BurninDestinationSettings) (\s a -> s {burninDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsBurninDestinationSettings "Use generic-lens or generic-optics with 'burninDestinationSettings' instead." #-}

-- | Settings specific to IMSC caption outputs.
--
-- /Note:/ Consider using 'imscDestinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdsImscDestinationSettings :: Lens.Lens' CaptionDestinationSettings (Lude.Maybe ImscDestinationSettings)
cdsImscDestinationSettings = Lens.lens (imscDestinationSettings :: CaptionDestinationSettings -> Lude.Maybe ImscDestinationSettings) (\s a -> s {imscDestinationSettings = a} :: CaptionDestinationSettings)
{-# DEPRECATED cdsImscDestinationSettings "Use generic-lens or generic-optics with 'imscDestinationSettings' instead." #-}

instance Lude.FromJSON CaptionDestinationSettings where
  parseJSON =
    Lude.withObject
      "CaptionDestinationSettings"
      ( \x ->
          CaptionDestinationSettings'
            Lude.<$> (x Lude..:? "teletextDestinationSettings")
            Lude.<*> (x Lude..:? "dvbSubDestinationSettings")
            Lude.<*> (x Lude..:? "ttmlDestinationSettings")
            Lude.<*> (x Lude..:? "destinationType")
            Lude.<*> (x Lude..:? "embeddedDestinationSettings")
            Lude.<*> (x Lude..:? "sccDestinationSettings")
            Lude.<*> (x Lude..:? "burninDestinationSettings")
            Lude.<*> (x Lude..:? "imscDestinationSettings")
      )

instance Lude.ToJSON CaptionDestinationSettings where
  toJSON CaptionDestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("teletextDestinationSettings" Lude..=)
              Lude.<$> teletextDestinationSettings,
            ("dvbSubDestinationSettings" Lude..=)
              Lude.<$> dvbSubDestinationSettings,
            ("ttmlDestinationSettings" Lude..=)
              Lude.<$> ttmlDestinationSettings,
            ("destinationType" Lude..=) Lude.<$> destinationType,
            ("embeddedDestinationSettings" Lude..=)
              Lude.<$> embeddedDestinationSettings,
            ("sccDestinationSettings" Lude..=) Lude.<$> sccDestinationSettings,
            ("burninDestinationSettings" Lude..=)
              Lude.<$> burninDestinationSettings,
            ("imscDestinationSettings" Lude..=)
              Lude.<$> imscDestinationSettings
          ]
      )
