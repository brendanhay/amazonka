{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.Fragment
  ( Fragment (..),

    -- * Smart constructor
    mkFragment,

    -- * Lenses
    fFragmentLengthInMilliseconds,
    fFragmentNumber,
    fFragmentSizeInBytes,
    fProducerTimestamp,
    fServerTimestamp,
  )
where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types.FragmentNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a segment of video or other time-delimited data.
--
-- /See:/ 'mkFragment' smart constructor.
data Fragment = Fragment'
  { -- | The playback duration or other time value associated with the fragment.
    fragmentLengthInMilliseconds :: Core.Maybe Core.Integer,
    -- | The unique identifier of the fragment. This value monotonically increases based on the ingestion order.
    fragmentNumber :: Core.Maybe Types.FragmentNumber,
    -- | The total fragment size, including information about the fragment and contained media data.
    fragmentSizeInBytes :: Core.Maybe Core.Integer,
    -- | The timestamp from the producer corresponding to the fragment.
    producerTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The timestamp from the AWS server corresponding to the fragment.
    serverTimestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Fragment' value with any optional fields omitted.
mkFragment ::
  Fragment
mkFragment =
  Fragment'
    { fragmentLengthInMilliseconds = Core.Nothing,
      fragmentNumber = Core.Nothing,
      fragmentSizeInBytes = Core.Nothing,
      producerTimestamp = Core.Nothing,
      serverTimestamp = Core.Nothing
    }

-- | The playback duration or other time value associated with the fragment.
--
-- /Note:/ Consider using 'fragmentLengthInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFragmentLengthInMilliseconds :: Lens.Lens' Fragment (Core.Maybe Core.Integer)
fFragmentLengthInMilliseconds = Lens.field @"fragmentLengthInMilliseconds"
{-# DEPRECATED fFragmentLengthInMilliseconds "Use generic-lens or generic-optics with 'fragmentLengthInMilliseconds' instead." #-}

-- | The unique identifier of the fragment. This value monotonically increases based on the ingestion order.
--
-- /Note:/ Consider using 'fragmentNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFragmentNumber :: Lens.Lens' Fragment (Core.Maybe Types.FragmentNumber)
fFragmentNumber = Lens.field @"fragmentNumber"
{-# DEPRECATED fFragmentNumber "Use generic-lens or generic-optics with 'fragmentNumber' instead." #-}

-- | The total fragment size, including information about the fragment and contained media data.
--
-- /Note:/ Consider using 'fragmentSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFragmentSizeInBytes :: Lens.Lens' Fragment (Core.Maybe Core.Integer)
fFragmentSizeInBytes = Lens.field @"fragmentSizeInBytes"
{-# DEPRECATED fFragmentSizeInBytes "Use generic-lens or generic-optics with 'fragmentSizeInBytes' instead." #-}

-- | The timestamp from the producer corresponding to the fragment.
--
-- /Note:/ Consider using 'producerTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fProducerTimestamp :: Lens.Lens' Fragment (Core.Maybe Core.NominalDiffTime)
fProducerTimestamp = Lens.field @"producerTimestamp"
{-# DEPRECATED fProducerTimestamp "Use generic-lens or generic-optics with 'producerTimestamp' instead." #-}

-- | The timestamp from the AWS server corresponding to the fragment.
--
-- /Note:/ Consider using 'serverTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fServerTimestamp :: Lens.Lens' Fragment (Core.Maybe Core.NominalDiffTime)
fServerTimestamp = Lens.field @"serverTimestamp"
{-# DEPRECATED fServerTimestamp "Use generic-lens or generic-optics with 'serverTimestamp' instead." #-}

instance Core.FromJSON Fragment where
  parseJSON =
    Core.withObject "Fragment" Core.$
      \x ->
        Fragment'
          Core.<$> (x Core..:? "FragmentLengthInMilliseconds")
          Core.<*> (x Core..:? "FragmentNumber")
          Core.<*> (x Core..:? "FragmentSizeInBytes")
          Core.<*> (x Core..:? "ProducerTimestamp")
          Core.<*> (x Core..:? "ServerTimestamp")
