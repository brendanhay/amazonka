-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointBufferingHints
  ( HTTPEndpointBufferingHints (..),

    -- * Smart constructor
    mkHTTPEndpointBufferingHints,

    -- * Lenses
    httpebhSizeInMBs,
    httpebhIntervalInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the buffering options that can be applied before data is delivered to the HTTP endpoint destination. Kinesis Data Firehose treats these options as hints, and it might choose to use more optimal values. The @SizeInMBs@ and @IntervalInSeconds@ parameters are optional. However, if specify a value for one of them, you must also provide a value for the other.
--
-- /See:/ 'mkHTTPEndpointBufferingHints' smart constructor.
data HTTPEndpointBufferingHints = HTTPEndpointBufferingHints'
  { sizeInMBs ::
      Lude.Maybe Lude.Natural,
    intervalInSeconds ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointBufferingHints' with the minimum fields required to make a request.
--
-- * 'intervalInSeconds' - Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
-- * 'sizeInMBs' - Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
mkHTTPEndpointBufferingHints ::
  HTTPEndpointBufferingHints
mkHTTPEndpointBufferingHints =
  HTTPEndpointBufferingHints'
    { sizeInMBs = Lude.Nothing,
      intervalInSeconds = Lude.Nothing
    }

-- | Buffer incoming data to the specified size, in MBs, before delivering it to the destination. The default value is 5.
--
-- We recommend setting this parameter to a value greater than the amount of data you typically ingest into the delivery stream in 10 seconds. For example, if you typically ingest data at 1 MB/sec, the value should be 10 MB or higher.
--
-- /Note:/ Consider using 'sizeInMBs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpebhSizeInMBs :: Lens.Lens' HTTPEndpointBufferingHints (Lude.Maybe Lude.Natural)
httpebhSizeInMBs = Lens.lens (sizeInMBs :: HTTPEndpointBufferingHints -> Lude.Maybe Lude.Natural) (\s a -> s {sizeInMBs = a} :: HTTPEndpointBufferingHints)
{-# DEPRECATED httpebhSizeInMBs "Use generic-lens or generic-optics with 'sizeInMBs' instead." #-}

-- | Buffer incoming data for the specified period of time, in seconds, before delivering it to the destination. The default value is 300 (5 minutes).
--
-- /Note:/ Consider using 'intervalInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpebhIntervalInSeconds :: Lens.Lens' HTTPEndpointBufferingHints (Lude.Maybe Lude.Natural)
httpebhIntervalInSeconds = Lens.lens (intervalInSeconds :: HTTPEndpointBufferingHints -> Lude.Maybe Lude.Natural) (\s a -> s {intervalInSeconds = a} :: HTTPEndpointBufferingHints)
{-# DEPRECATED httpebhIntervalInSeconds "Use generic-lens or generic-optics with 'intervalInSeconds' instead." #-}

instance Lude.FromJSON HTTPEndpointBufferingHints where
  parseJSON =
    Lude.withObject
      "HTTPEndpointBufferingHints"
      ( \x ->
          HTTPEndpointBufferingHints'
            Lude.<$> (x Lude..:? "SizeInMBs") Lude.<*> (x Lude..:? "IntervalInSeconds")
      )

instance Lude.ToJSON HTTPEndpointBufferingHints where
  toJSON HTTPEndpointBufferingHints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SizeInMBs" Lude..=) Lude.<$> sizeInMBs,
            ("IntervalInSeconds" Lude..=) Lude.<$> intervalInSeconds
          ]
      )
