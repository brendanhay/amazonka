{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillarySourceSettings
  ( AncillarySourceSettings (..),

    -- * Smart constructor
    mkAncillarySourceSettings,

    -- * Lenses
    assConvert608To708,
    assTerminateCaptions,
    assSourceAncillaryChannelNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AncillaryConvert608To708
import Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
import qualified Network.AWS.Prelude as Lude

-- | Settings for ancillary captions source.
--
-- /See:/ 'mkAncillarySourceSettings' smart constructor.
data AncillarySourceSettings = AncillarySourceSettings'
  { convert608To708 ::
      Lude.Maybe AncillaryConvert608To708,
    terminateCaptions ::
      Lude.Maybe AncillaryTerminateCaptions,
    sourceAncillaryChannelNumber ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AncillarySourceSettings' with the minimum fields required to make a request.
--
-- * 'convert608To708' - Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
-- * 'sourceAncillaryChannelNumber' - Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
-- * 'terminateCaptions' - By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
mkAncillarySourceSettings ::
  AncillarySourceSettings
mkAncillarySourceSettings =
  AncillarySourceSettings'
    { convert608To708 = Lude.Nothing,
      terminateCaptions = Lude.Nothing,
      sourceAncillaryChannelNumber = Lude.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assConvert608To708 :: Lens.Lens' AncillarySourceSettings (Lude.Maybe AncillaryConvert608To708)
assConvert608To708 = Lens.lens (convert608To708 :: AncillarySourceSettings -> Lude.Maybe AncillaryConvert608To708) (\s a -> s {convert608To708 = a} :: AncillarySourceSettings)
{-# DEPRECATED assConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
--
-- /Note:/ Consider using 'terminateCaptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assTerminateCaptions :: Lens.Lens' AncillarySourceSettings (Lude.Maybe AncillaryTerminateCaptions)
assTerminateCaptions = Lens.lens (terminateCaptions :: AncillarySourceSettings -> Lude.Maybe AncillaryTerminateCaptions) (\s a -> s {terminateCaptions = a} :: AncillarySourceSettings)
{-# DEPRECATED assTerminateCaptions "Use generic-lens or generic-optics with 'terminateCaptions' instead." #-}

-- | Specifies the 608 channel number in the ancillary data track from which to extract captions. Unused for passthrough.
--
-- /Note:/ Consider using 'sourceAncillaryChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSourceAncillaryChannelNumber :: Lens.Lens' AncillarySourceSettings (Lude.Maybe Lude.Natural)
assSourceAncillaryChannelNumber = Lens.lens (sourceAncillaryChannelNumber :: AncillarySourceSettings -> Lude.Maybe Lude.Natural) (\s a -> s {sourceAncillaryChannelNumber = a} :: AncillarySourceSettings)
{-# DEPRECATED assSourceAncillaryChannelNumber "Use generic-lens or generic-optics with 'sourceAncillaryChannelNumber' instead." #-}

instance Lude.FromJSON AncillarySourceSettings where
  parseJSON =
    Lude.withObject
      "AncillarySourceSettings"
      ( \x ->
          AncillarySourceSettings'
            Lude.<$> (x Lude..:? "convert608To708")
            Lude.<*> (x Lude..:? "terminateCaptions")
            Lude.<*> (x Lude..:? "sourceAncillaryChannelNumber")
      )

instance Lude.ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("convert608To708" Lude..=) Lude.<$> convert608To708,
            ("terminateCaptions" Lude..=) Lude.<$> terminateCaptions,
            ("sourceAncillaryChannelNumber" Lude..=)
              Lude.<$> sourceAncillaryChannelNumber
          ]
      )
