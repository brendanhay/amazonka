{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DescriptorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35DescriptorSettings
  ( Scte35DescriptorSettings (..)
  -- * Smart constructor
  , mkScte35DescriptorSettings
  -- * Lenses
  , sdsSegmentationDescriptorScte35DescriptorSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor as Types
import qualified Network.AWS.Prelude as Core

-- | SCTE-35 Descriptor settings.
--
-- /See:/ 'mkScte35DescriptorSettings' smart constructor.
newtype Scte35DescriptorSettings = Scte35DescriptorSettings'
  { segmentationDescriptorScte35DescriptorSettings :: Types.Scte35SegmentationDescriptor
    -- ^ SCTE-35 Segmentation Descriptor.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35DescriptorSettings' value with any optional fields omitted.
mkScte35DescriptorSettings
    :: Types.Scte35SegmentationDescriptor -- ^ 'segmentationDescriptorScte35DescriptorSettings'
    -> Scte35DescriptorSettings
mkScte35DescriptorSettings
  segmentationDescriptorScte35DescriptorSettings
  = Scte35DescriptorSettings'{segmentationDescriptorScte35DescriptorSettings}

-- | SCTE-35 Segmentation Descriptor.
--
-- /Note:/ Consider using 'segmentationDescriptorScte35DescriptorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsSegmentationDescriptorScte35DescriptorSettings :: Lens.Lens' Scte35DescriptorSettings Types.Scte35SegmentationDescriptor
sdsSegmentationDescriptorScte35DescriptorSettings = Lens.field @"segmentationDescriptorScte35DescriptorSettings"
{-# INLINEABLE sdsSegmentationDescriptorScte35DescriptorSettings #-}
{-# DEPRECATED segmentationDescriptorScte35DescriptorSettings "Use generic-lens or generic-optics with 'segmentationDescriptorScte35DescriptorSettings' instead"  #-}

instance Core.FromJSON Scte35DescriptorSettings where
        toJSON Scte35DescriptorSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("segmentationDescriptorScte35DescriptorSettings" Core..=
                       segmentationDescriptorScte35DescriptorSettings)])

instance Core.FromJSON Scte35DescriptorSettings where
        parseJSON
          = Core.withObject "Scte35DescriptorSettings" Core.$
              \ x ->
                Scte35DescriptorSettings' Core.<$>
                  (x Core..: "segmentationDescriptorScte35DescriptorSettings")
