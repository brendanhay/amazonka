{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AncillarySourceSettings
  ( AncillarySourceSettings (..),

    -- * Smart constructor
    mkAncillarySourceSettings,

    -- * Lenses
    assSourceAncillaryChannelNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Ancillary Source Settings
--
-- /See:/ 'mkAncillarySourceSettings' smart constructor.
newtype AncillarySourceSettings = AncillarySourceSettings'
  { sourceAncillaryChannelNumber ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AncillarySourceSettings' with the minimum fields required to make a request.
--
-- * 'sourceAncillaryChannelNumber' - Specifies the number (1 to 4) of the captions channel you want to extract from the ancillary captions. If you plan to convert the ancillary captions to another format, complete this field. If you plan to choose Embedded as the captions destination in the output (to pass through all the channels in the ancillary captions), leave this field blank because MediaLive ignores the field.
mkAncillarySourceSettings ::
  AncillarySourceSettings
mkAncillarySourceSettings =
  AncillarySourceSettings'
    { sourceAncillaryChannelNumber =
        Lude.Nothing
    }

-- | Specifies the number (1 to 4) of the captions channel you want to extract from the ancillary captions. If you plan to convert the ancillary captions to another format, complete this field. If you plan to choose Embedded as the captions destination in the output (to pass through all the channels in the ancillary captions), leave this field blank because MediaLive ignores the field.
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
            Lude.<$> (x Lude..:? "sourceAncillaryChannelNumber")
      )

instance Lude.ToJSON AncillarySourceSettings where
  toJSON AncillarySourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sourceAncillaryChannelNumber" Lude..=)
              Lude.<$> sourceAncillaryChannelNumber
          ]
      )
