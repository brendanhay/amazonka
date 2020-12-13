{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ElasticsearchBufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchBufferingHints
  ( ElasticsearchBufferingHints (..),

    -- * Smart constructor
    mkElasticsearchBufferingHints,

    -- * Lenses
    ebhSizeInMBs,
    ebhIntervalInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the buffering to perform before delivering data to the Amazon ES destination.
--
-- /See:/ 'mkElasticsearchBufferingHints' smart constructor.
data ElasticsearchBufferingHints = ElasticsearchBufferingHints'
  { -- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
    --
    -- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
    sizeInMBs :: Lude.Maybe Lude.Natural,
    -- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
    intervalInSeconds :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchBufferingHints' with the minimum fields required to make a request.
--
-- * 'sizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
-- * 'intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
mkElasticsearchBufferingHints ::
  ElasticsearchBufferingHints
mkElasticsearchBufferingHints =
  ElasticsearchBufferingHints'
    { sizeInMBs = Lude.Nothing,
      intervalInSeconds = Lude.Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- /Note:/ Consider using 'sizeInMBs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebhSizeInMBs :: Lens.Lens' ElasticsearchBufferingHints (Lude.Maybe Lude.Natural)
ebhSizeInMBs = Lens.lens (sizeInMBs :: ElasticsearchBufferingHints -> Lude.Maybe Lude.Natural) (\s a -> s {sizeInMBs = a} :: ElasticsearchBufferingHints)
{-# DEPRECATED ebhSizeInMBs "Use generic-lens or generic-optics with 'sizeInMBs' instead." #-}

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'intervalInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebhIntervalInSeconds :: Lens.Lens' ElasticsearchBufferingHints (Lude.Maybe Lude.Natural)
ebhIntervalInSeconds = Lens.lens (intervalInSeconds :: ElasticsearchBufferingHints -> Lude.Maybe Lude.Natural) (\s a -> s {intervalInSeconds = a} :: ElasticsearchBufferingHints)
{-# DEPRECATED ebhIntervalInSeconds "Use generic-lens or generic-optics with 'intervalInSeconds' instead." #-}

instance Lude.FromJSON ElasticsearchBufferingHints where
  parseJSON =
    Lude.withObject
      "ElasticsearchBufferingHints"
      ( \x ->
          ElasticsearchBufferingHints'
            Lude.<$> (x Lude..:? "SizeInMBs") Lude.<*> (x Lude..:? "IntervalInSeconds")
      )

instance Lude.ToJSON ElasticsearchBufferingHints where
  toJSON ElasticsearchBufferingHints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SizeInMBs" Lude..=) Lude.<$> sizeInMBs,
            ("IntervalInSeconds" Lude..=) Lude.<$> intervalInSeconds
          ]
      )
