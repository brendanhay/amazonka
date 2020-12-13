{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamSummary
  ( StreamSummary (..),

    -- * Smart constructor
    mkStreamSummary,

    -- * Lenses
    ssStreamVersion,
    ssStreamARN,
    ssDescription,
    ssStreamId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of a stream.
--
-- /See:/ 'mkStreamSummary' smart constructor.
data StreamSummary = StreamSummary'
  { -- | The stream version.
    streamVersion :: Lude.Maybe Lude.Natural,
    -- | The stream ARN.
    streamARN :: Lude.Maybe Lude.Text,
    -- | A description of the stream.
    description :: Lude.Maybe Lude.Text,
    -- | The stream ID.
    streamId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamSummary' with the minimum fields required to make a request.
--
-- * 'streamVersion' - The stream version.
-- * 'streamARN' - The stream ARN.
-- * 'description' - A description of the stream.
-- * 'streamId' - The stream ID.
mkStreamSummary ::
  StreamSummary
mkStreamSummary =
  StreamSummary'
    { streamVersion = Lude.Nothing,
      streamARN = Lude.Nothing,
      description = Lude.Nothing,
      streamId = Lude.Nothing
    }

-- | The stream version.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamVersion :: Lens.Lens' StreamSummary (Lude.Maybe Lude.Natural)
ssStreamVersion = Lens.lens (streamVersion :: StreamSummary -> Lude.Maybe Lude.Natural) (\s a -> s {streamVersion = a} :: StreamSummary)
{-# DEPRECATED ssStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamARN :: Lens.Lens' StreamSummary (Lude.Maybe Lude.Text)
ssStreamARN = Lens.lens (streamARN :: StreamSummary -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: StreamSummary)
{-# DEPRECATED ssStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | A description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StreamSummary (Lude.Maybe Lude.Text)
ssDescription = Lens.lens (description :: StreamSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StreamSummary)
{-# DEPRECATED ssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStreamId :: Lens.Lens' StreamSummary (Lude.Maybe Lude.Text)
ssStreamId = Lens.lens (streamId :: StreamSummary -> Lude.Maybe Lude.Text) (\s a -> s {streamId = a} :: StreamSummary)
{-# DEPRECATED ssStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

instance Lude.FromJSON StreamSummary where
  parseJSON =
    Lude.withObject
      "StreamSummary"
      ( \x ->
          StreamSummary'
            Lude.<$> (x Lude..:? "streamVersion")
            Lude.<*> (x Lude..:? "streamArn")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "streamId")
      )
