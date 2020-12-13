{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SourceDescription
  ( SourceDescription (..),

    -- * Smart constructor
    mkSourceDescription,

    -- * Lenses
    sdKinesisStreamSourceDescription,
  )
where

import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about a Kinesis data stream used as the source for a Kinesis Data Firehose delivery stream.
--
-- /See:/ 'mkSourceDescription' smart constructor.
newtype SourceDescription = SourceDescription'
  { -- | The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
    kinesisStreamSourceDescription :: Lude.Maybe KinesisStreamSourceDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceDescription' with the minimum fields required to make a request.
--
-- * 'kinesisStreamSourceDescription' - The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
mkSourceDescription ::
  SourceDescription
mkSourceDescription =
  SourceDescription' {kinesisStreamSourceDescription = Lude.Nothing}

-- | The 'KinesisStreamSourceDescription' value for the source Kinesis data stream.
--
-- /Note:/ Consider using 'kinesisStreamSourceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdKinesisStreamSourceDescription :: Lens.Lens' SourceDescription (Lude.Maybe KinesisStreamSourceDescription)
sdKinesisStreamSourceDescription = Lens.lens (kinesisStreamSourceDescription :: SourceDescription -> Lude.Maybe KinesisStreamSourceDescription) (\s a -> s {kinesisStreamSourceDescription = a} :: SourceDescription)
{-# DEPRECATED sdKinesisStreamSourceDescription "Use generic-lens or generic-optics with 'kinesisStreamSourceDescription' instead." #-}

instance Lude.FromJSON SourceDescription where
  parseJSON =
    Lude.withObject
      "SourceDescription"
      ( \x ->
          SourceDescription'
            Lude.<$> (x Lude..:? "KinesisStreamSourceDescription")
      )
