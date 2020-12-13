{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SequenceNumberRange
  ( SequenceNumberRange (..),

    -- * Smart constructor
    mkSequenceNumberRange,

    -- * Lenses
    snrStartingSequenceNumber,
    snrEndingSequenceNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The range of possible sequence numbers for the shard.
--
-- /See:/ 'mkSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { -- | The starting sequence number for the range.
    startingSequenceNumber :: Lude.Text,
    -- | The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
    endingSequenceNumber :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SequenceNumberRange' with the minimum fields required to make a request.
--
-- * 'startingSequenceNumber' - The starting sequence number for the range.
-- * 'endingSequenceNumber' - The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
mkSequenceNumberRange ::
  -- | 'startingSequenceNumber'
  Lude.Text ->
  SequenceNumberRange
mkSequenceNumberRange pStartingSequenceNumber_ =
  SequenceNumberRange'
    { startingSequenceNumber =
        pStartingSequenceNumber_,
      endingSequenceNumber = Lude.Nothing
    }

-- | The starting sequence number for the range.
--
-- /Note:/ Consider using 'startingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snrStartingSequenceNumber :: Lens.Lens' SequenceNumberRange Lude.Text
snrStartingSequenceNumber = Lens.lens (startingSequenceNumber :: SequenceNumberRange -> Lude.Text) (\s a -> s {startingSequenceNumber = a} :: SequenceNumberRange)
{-# DEPRECATED snrStartingSequenceNumber "Use generic-lens or generic-optics with 'startingSequenceNumber' instead." #-}

-- | The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
--
-- /Note:/ Consider using 'endingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snrEndingSequenceNumber :: Lens.Lens' SequenceNumberRange (Lude.Maybe Lude.Text)
snrEndingSequenceNumber = Lens.lens (endingSequenceNumber :: SequenceNumberRange -> Lude.Maybe Lude.Text) (\s a -> s {endingSequenceNumber = a} :: SequenceNumberRange)
{-# DEPRECATED snrEndingSequenceNumber "Use generic-lens or generic-optics with 'endingSequenceNumber' instead." #-}

instance Lude.FromJSON SequenceNumberRange where
  parseJSON =
    Lude.withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            Lude.<$> (x Lude..: "StartingSequenceNumber")
            Lude.<*> (x Lude..:? "EndingSequenceNumber")
      )
