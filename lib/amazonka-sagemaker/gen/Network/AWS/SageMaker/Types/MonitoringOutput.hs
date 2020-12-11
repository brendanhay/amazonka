-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringOutput
  ( MonitoringOutput (..),

    -- * Smart constructor
    mkMonitoringOutput,

    -- * Lenses
    moS3Output,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MonitoringS3Output

-- | The output object for a monitoring job.
--
-- /See:/ 'mkMonitoringOutput' smart constructor.
newtype MonitoringOutput = MonitoringOutput'
  { s3Output ::
      MonitoringS3Output
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringOutput' with the minimum fields required to make a request.
--
-- * 's3Output' - The Amazon S3 storage location where the results of a monitoring job are saved.
mkMonitoringOutput ::
  -- | 's3Output'
  MonitoringS3Output ->
  MonitoringOutput
mkMonitoringOutput pS3Output_ =
  MonitoringOutput' {s3Output = pS3Output_}

-- | The Amazon S3 storage location where the results of a monitoring job are saved.
--
-- /Note:/ Consider using 's3Output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moS3Output :: Lens.Lens' MonitoringOutput MonitoringS3Output
moS3Output = Lens.lens (s3Output :: MonitoringOutput -> MonitoringS3Output) (\s a -> s {s3Output = a} :: MonitoringOutput)
{-# DEPRECATED moS3Output "Use generic-lens or generic-optics with 's3Output' instead." #-}

instance Lude.FromJSON MonitoringOutput where
  parseJSON =
    Lude.withObject
      "MonitoringOutput"
      (\x -> MonitoringOutput' Lude.<$> (x Lude..: "S3Output"))

instance Lude.ToJSON MonitoringOutput where
  toJSON MonitoringOutput' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("S3Output" Lude..= s3Output)])
