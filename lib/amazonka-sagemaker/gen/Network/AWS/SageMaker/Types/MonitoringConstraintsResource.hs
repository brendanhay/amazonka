{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringConstraintsResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringConstraintsResource
  ( MonitoringConstraintsResource (..),

    -- * Smart constructor
    mkMonitoringConstraintsResource,

    -- * Lenses
    mcrS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The constraints resource for a monitoring job.
--
-- /See:/ 'mkMonitoringConstraintsResource' smart constructor.
newtype MonitoringConstraintsResource = MonitoringConstraintsResource'
  { -- | The Amazon S3 URI for the constraints resource.
    s3URI :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringConstraintsResource' with the minimum fields required to make a request.
--
-- * 's3URI' - The Amazon S3 URI for the constraints resource.
mkMonitoringConstraintsResource ::
  MonitoringConstraintsResource
mkMonitoringConstraintsResource =
  MonitoringConstraintsResource' {s3URI = Lude.Nothing}

-- | The Amazon S3 URI for the constraints resource.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrS3URI :: Lens.Lens' MonitoringConstraintsResource (Lude.Maybe Lude.Text)
mcrS3URI = Lens.lens (s3URI :: MonitoringConstraintsResource -> Lude.Maybe Lude.Text) (\s a -> s {s3URI = a} :: MonitoringConstraintsResource)
{-# DEPRECATED mcrS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON MonitoringConstraintsResource where
  parseJSON =
    Lude.withObject
      "MonitoringConstraintsResource"
      ( \x ->
          MonitoringConstraintsResource' Lude.<$> (x Lude..:? "S3Uri")
      )

instance Lude.ToJSON MonitoringConstraintsResource where
  toJSON MonitoringConstraintsResource' {..} =
    Lude.object (Lude.catMaybes [("S3Uri" Lude..=) Lude.<$> s3URI])
