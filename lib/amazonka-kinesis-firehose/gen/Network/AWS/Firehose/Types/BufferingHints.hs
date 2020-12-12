{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.BufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.BufferingHints
  ( BufferingHints (..),

    -- * Smart constructor
    mkBufferingHints,

    -- * Lenses
    bhSizeInMBs,
    bhIntervalInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes hints for the buffering to perform before delivering data to the destination. These options are treated as hints, and therefore Kinesis Data Firehose might choose to use different values when it is optimal. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- /See:/ 'mkBufferingHints' smart constructor.
data BufferingHints = BufferingHints'
  { sizeInMBs ::
      Lude.Maybe Lude.Natural,
    intervalInSeconds :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BufferingHints' with the minimum fields required to make a request.
--
-- * 'intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300. This parameter is optional but if you specify a value for it, you must also specify a value for @SizeInMBs@ , and vice versa.
-- * 'sizeInMBs' - Buffer incoming data to the specified size, in MiBs, before delivering it to the destination. The default value is 5. This parameter is optional but if you specify a value for it, you must also specify a value for @IntervalInSeconds@ , and vice versa.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MiB/sec, the value should be 10 MiB or higher.
mkBufferingHints ::
  BufferingHints
mkBufferingHints =
  BufferingHints'
    { sizeInMBs = Lude.Nothing,
      intervalInSeconds = Lude.Nothing
    }

-- | Buffer incoming data to the specified size, in MiBs, before delivering it to the destination. The default value is 5. This parameter is optional but if you specify a value for it, you must also specify a value for @IntervalInSeconds@ , and vice versa.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MiB/sec, the value should be 10 MiB or higher.
--
-- /Note:/ Consider using 'sizeInMBs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bhSizeInMBs :: Lens.Lens' BufferingHints (Lude.Maybe Lude.Natural)
bhSizeInMBs = Lens.lens (sizeInMBs :: BufferingHints -> Lude.Maybe Lude.Natural) (\s a -> s {sizeInMBs = a} :: BufferingHints)
{-# DEPRECATED bhSizeInMBs "Use generic-lens or generic-optics with 'sizeInMBs' instead." #-}

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300. This parameter is optional but if you specify a value for it, you must also specify a value for @SizeInMBs@ , and vice versa.
--
-- /Note:/ Consider using 'intervalInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bhIntervalInSeconds :: Lens.Lens' BufferingHints (Lude.Maybe Lude.Natural)
bhIntervalInSeconds = Lens.lens (intervalInSeconds :: BufferingHints -> Lude.Maybe Lude.Natural) (\s a -> s {intervalInSeconds = a} :: BufferingHints)
{-# DEPRECATED bhIntervalInSeconds "Use generic-lens or generic-optics with 'intervalInSeconds' instead." #-}

instance Lude.FromJSON BufferingHints where
  parseJSON =
    Lude.withObject
      "BufferingHints"
      ( \x ->
          BufferingHints'
            Lude.<$> (x Lude..:? "SizeInMBs") Lude.<*> (x Lude..:? "IntervalInSeconds")
      )

instance Lude.ToJSON BufferingHints where
  toJSON BufferingHints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SizeInMBs" Lude..=) Lude.<$> sizeInMBs,
            ("IntervalInSeconds" Lude..=) Lude.<$> intervalInSeconds
          ]
      )
