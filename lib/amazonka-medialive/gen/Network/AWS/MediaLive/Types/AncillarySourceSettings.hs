{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AncillarySourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AncillarySourceSettings
  ( AncillarySourceSettings (..)
  -- * Smart constructor
  , mkAncillarySourceSettings
  -- * Lenses
  , assSourceAncillaryChannelNumber
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Ancillary Source Settings
--
-- /See:/ 'mkAncillarySourceSettings' smart constructor.
newtype AncillarySourceSettings = AncillarySourceSettings'
  { sourceAncillaryChannelNumber :: Core.Maybe Core.Natural
    -- ^ Specifies the number (1 to 4) of the captions channel you want to extract from the ancillary captions. If you plan to convert the ancillary captions to another format, complete this field. If you plan to choose Embedded as the captions destination in the output (to pass through all the channels in the ancillary captions), leave this field blank because MediaLive ignores the field.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AncillarySourceSettings' value with any optional fields omitted.
mkAncillarySourceSettings
    :: AncillarySourceSettings
mkAncillarySourceSettings
  = AncillarySourceSettings'{sourceAncillaryChannelNumber =
                               Core.Nothing}

-- | Specifies the number (1 to 4) of the captions channel you want to extract from the ancillary captions. If you plan to convert the ancillary captions to another format, complete this field. If you plan to choose Embedded as the captions destination in the output (to pass through all the channels in the ancillary captions), leave this field blank because MediaLive ignores the field.
--
-- /Note:/ Consider using 'sourceAncillaryChannelNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assSourceAncillaryChannelNumber :: Lens.Lens' AncillarySourceSettings (Core.Maybe Core.Natural)
assSourceAncillaryChannelNumber = Lens.field @"sourceAncillaryChannelNumber"
{-# INLINEABLE assSourceAncillaryChannelNumber #-}
{-# DEPRECATED sourceAncillaryChannelNumber "Use generic-lens or generic-optics with 'sourceAncillaryChannelNumber' instead"  #-}

instance Core.FromJSON AncillarySourceSettings where
        toJSON AncillarySourceSettings{..}
          = Core.object
              (Core.catMaybes
                 [("sourceAncillaryChannelNumber" Core..=) Core.<$>
                    sourceAncillaryChannelNumber])

instance Core.FromJSON AncillarySourceSettings where
        parseJSON
          = Core.withObject "AncillarySourceSettings" Core.$
              \ x ->
                AncillarySourceSettings' Core.<$>
                  (x Core..:? "sourceAncillaryChannelNumber")
