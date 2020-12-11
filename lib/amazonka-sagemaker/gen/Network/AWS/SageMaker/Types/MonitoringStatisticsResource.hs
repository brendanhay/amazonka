-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringStatisticsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStatisticsResource
  ( MonitoringStatisticsResource (..),

    -- * Smart constructor
    mkMonitoringStatisticsResource,

    -- * Lenses
    msrS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The statistics resource for a monitoring job.
--
-- /See:/ 'mkMonitoringStatisticsResource' smart constructor.
newtype MonitoringStatisticsResource = MonitoringStatisticsResource'
  { s3URI ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringStatisticsResource' with the minimum fields required to make a request.
--
-- * 's3URI' - The Amazon S3 URI for the statistics resource.
mkMonitoringStatisticsResource ::
  MonitoringStatisticsResource
mkMonitoringStatisticsResource =
  MonitoringStatisticsResource' {s3URI = Lude.Nothing}

-- | The Amazon S3 URI for the statistics resource.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msrS3URI :: Lens.Lens' MonitoringStatisticsResource (Lude.Maybe Lude.Text)
msrS3URI = Lens.lens (s3URI :: MonitoringStatisticsResource -> Lude.Maybe Lude.Text) (\s a -> s {s3URI = a} :: MonitoringStatisticsResource)
{-# DEPRECATED msrS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON MonitoringStatisticsResource where
  parseJSON =
    Lude.withObject
      "MonitoringStatisticsResource"
      ( \x ->
          MonitoringStatisticsResource' Lude.<$> (x Lude..:? "S3Uri")
      )

instance Lude.ToJSON MonitoringStatisticsResource where
  toJSON MonitoringStatisticsResource' {..} =
    Lude.object (Lude.catMaybes [("S3Uri" Lude..=) Lude.<$> s3URI])
