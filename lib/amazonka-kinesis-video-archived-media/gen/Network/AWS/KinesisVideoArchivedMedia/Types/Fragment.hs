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
    fServerTimestamp,
    fFragmentSizeInBytes,
    fFragmentNumber,
    fProducerTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a segment of video or other time-delimited data.
--
-- /See:/ 'mkFragment' smart constructor.
data Fragment = Fragment'
  { fragmentLengthInMilliseconds ::
      Lude.Maybe Lude.Integer,
    serverTimestamp :: Lude.Maybe Lude.Timestamp,
    fragmentSizeInBytes :: Lude.Maybe Lude.Integer,
    fragmentNumber :: Lude.Maybe Lude.Text,
    producerTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Fragment' with the minimum fields required to make a request.
--
-- * 'fragmentLengthInMilliseconds' - The playback duration or other time value associated with the fragment.
-- * 'fragmentNumber' - The unique identifier of the fragment. This value monotonically increases based on the ingestion order.
-- * 'fragmentSizeInBytes' - The total fragment size, including information about the fragment and contained media data.
-- * 'producerTimestamp' - The timestamp from the producer corresponding to the fragment.
-- * 'serverTimestamp' - The timestamp from the AWS server corresponding to the fragment.
mkFragment ::
  Fragment
mkFragment =
  Fragment'
    { fragmentLengthInMilliseconds = Lude.Nothing,
      serverTimestamp = Lude.Nothing,
      fragmentSizeInBytes = Lude.Nothing,
      fragmentNumber = Lude.Nothing,
      producerTimestamp = Lude.Nothing
    }

-- | The playback duration or other time value associated with the fragment.
--
-- /Note:/ Consider using 'fragmentLengthInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFragmentLengthInMilliseconds :: Lens.Lens' Fragment (Lude.Maybe Lude.Integer)
fFragmentLengthInMilliseconds = Lens.lens (fragmentLengthInMilliseconds :: Fragment -> Lude.Maybe Lude.Integer) (\s a -> s {fragmentLengthInMilliseconds = a} :: Fragment)
{-# DEPRECATED fFragmentLengthInMilliseconds "Use generic-lens or generic-optics with 'fragmentLengthInMilliseconds' instead." #-}

-- | The timestamp from the AWS server corresponding to the fragment.
--
-- /Note:/ Consider using 'serverTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fServerTimestamp :: Lens.Lens' Fragment (Lude.Maybe Lude.Timestamp)
fServerTimestamp = Lens.lens (serverTimestamp :: Fragment -> Lude.Maybe Lude.Timestamp) (\s a -> s {serverTimestamp = a} :: Fragment)
{-# DEPRECATED fServerTimestamp "Use generic-lens or generic-optics with 'serverTimestamp' instead." #-}

-- | The total fragment size, including information about the fragment and contained media data.
--
-- /Note:/ Consider using 'fragmentSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFragmentSizeInBytes :: Lens.Lens' Fragment (Lude.Maybe Lude.Integer)
fFragmentSizeInBytes = Lens.lens (fragmentSizeInBytes :: Fragment -> Lude.Maybe Lude.Integer) (\s a -> s {fragmentSizeInBytes = a} :: Fragment)
{-# DEPRECATED fFragmentSizeInBytes "Use generic-lens or generic-optics with 'fragmentSizeInBytes' instead." #-}

-- | The unique identifier of the fragment. This value monotonically increases based on the ingestion order.
--
-- /Note:/ Consider using 'fragmentNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFragmentNumber :: Lens.Lens' Fragment (Lude.Maybe Lude.Text)
fFragmentNumber = Lens.lens (fragmentNumber :: Fragment -> Lude.Maybe Lude.Text) (\s a -> s {fragmentNumber = a} :: Fragment)
{-# DEPRECATED fFragmentNumber "Use generic-lens or generic-optics with 'fragmentNumber' instead." #-}

-- | The timestamp from the producer corresponding to the fragment.
--
-- /Note:/ Consider using 'producerTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fProducerTimestamp :: Lens.Lens' Fragment (Lude.Maybe Lude.Timestamp)
fProducerTimestamp = Lens.lens (producerTimestamp :: Fragment -> Lude.Maybe Lude.Timestamp) (\s a -> s {producerTimestamp = a} :: Fragment)
{-# DEPRECATED fProducerTimestamp "Use generic-lens or generic-optics with 'producerTimestamp' instead." #-}

instance Lude.FromJSON Fragment where
  parseJSON =
    Lude.withObject
      "Fragment"
      ( \x ->
          Fragment'
            Lude.<$> (x Lude..:? "FragmentLengthInMilliseconds")
            Lude.<*> (x Lude..:? "ServerTimestamp")
            Lude.<*> (x Lude..:? "FragmentSizeInBytes")
            Lude.<*> (x Lude..:? "FragmentNumber")
            Lude.<*> (x Lude..:? "ProducerTimestamp")
      )
