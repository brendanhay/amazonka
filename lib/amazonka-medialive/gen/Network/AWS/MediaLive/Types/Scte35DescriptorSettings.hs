{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DescriptorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DescriptorSettings
  ( Scte35DescriptorSettings (..),

    -- * Smart constructor
    mkScte35DescriptorSettings,

    -- * Lenses
    sdsSegmentationDescriptorScte35DescriptorSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
import qualified Network.AWS.Prelude as Lude

-- | SCTE-35 Descriptor settings.
--
-- /See:/ 'mkScte35DescriptorSettings' smart constructor.
newtype Scte35DescriptorSettings = Scte35DescriptorSettings'
  { -- | SCTE-35 Segmentation Descriptor.
    segmentationDescriptorScte35DescriptorSettings :: Scte35SegmentationDescriptor
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35DescriptorSettings' with the minimum fields required to make a request.
--
-- * 'segmentationDescriptorScte35DescriptorSettings' - SCTE-35 Segmentation Descriptor.
mkScte35DescriptorSettings ::
  -- | 'segmentationDescriptorScte35DescriptorSettings'
  Scte35SegmentationDescriptor ->
  Scte35DescriptorSettings
mkScte35DescriptorSettings
  pSegmentationDescriptorScte35DescriptorSettings_ =
    Scte35DescriptorSettings'
      { segmentationDescriptorScte35DescriptorSettings =
          pSegmentationDescriptorScte35DescriptorSettings_
      }

-- | SCTE-35 Segmentation Descriptor.
--
-- /Note:/ Consider using 'segmentationDescriptorScte35DescriptorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsSegmentationDescriptorScte35DescriptorSettings :: Lens.Lens' Scte35DescriptorSettings Scte35SegmentationDescriptor
sdsSegmentationDescriptorScte35DescriptorSettings = Lens.lens (segmentationDescriptorScte35DescriptorSettings :: Scte35DescriptorSettings -> Scte35SegmentationDescriptor) (\s a -> s {segmentationDescriptorScte35DescriptorSettings = a} :: Scte35DescriptorSettings)
{-# DEPRECATED sdsSegmentationDescriptorScte35DescriptorSettings "Use generic-lens or generic-optics with 'segmentationDescriptorScte35DescriptorSettings' instead." #-}

instance Lude.FromJSON Scte35DescriptorSettings where
  parseJSON =
    Lude.withObject
      "Scte35DescriptorSettings"
      ( \x ->
          Scte35DescriptorSettings'
            Lude.<$> (x Lude..: "segmentationDescriptorScte35DescriptorSettings")
      )

instance Lude.ToJSON Scte35DescriptorSettings where
  toJSON Scte35DescriptorSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "segmentationDescriptorScte35DescriptorSettings"
                  Lude..= segmentationDescriptorScte35DescriptorSettings
              )
          ]
      )
