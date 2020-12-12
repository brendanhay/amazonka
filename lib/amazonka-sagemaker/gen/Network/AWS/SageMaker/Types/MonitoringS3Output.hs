{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringS3Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringS3Output
  ( MonitoringS3Output (..),

    -- * Smart constructor
    mkMonitoringS3Output,

    -- * Lenses
    msoS3UploadMode,
    msoS3URI,
    msoLocalPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode

-- | Information about where and how you want to store the results of a monitoring job.
--
-- /See:/ 'mkMonitoringS3Output' smart constructor.
data MonitoringS3Output = MonitoringS3Output'
  { s3UploadMode ::
      Lude.Maybe ProcessingS3UploadMode,
    s3URI :: Lude.Text,
    localPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringS3Output' with the minimum fields required to make a request.
--
-- * 'localPath' - The local path to the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job. LocalPath is an absolute path for the output data.
-- * 's3URI' - A URI that identifies the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job.
-- * 's3UploadMode' - Whether to upload the results of the monitoring job continuously or after the job completes.
mkMonitoringS3Output ::
  -- | 's3URI'
  Lude.Text ->
  -- | 'localPath'
  Lude.Text ->
  MonitoringS3Output
mkMonitoringS3Output pS3URI_ pLocalPath_ =
  MonitoringS3Output'
    { s3UploadMode = Lude.Nothing,
      s3URI = pS3URI_,
      localPath = pLocalPath_
    }

-- | Whether to upload the results of the monitoring job continuously or after the job completes.
--
-- /Note:/ Consider using 's3UploadMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msoS3UploadMode :: Lens.Lens' MonitoringS3Output (Lude.Maybe ProcessingS3UploadMode)
msoS3UploadMode = Lens.lens (s3UploadMode :: MonitoringS3Output -> Lude.Maybe ProcessingS3UploadMode) (\s a -> s {s3UploadMode = a} :: MonitoringS3Output)
{-# DEPRECATED msoS3UploadMode "Use generic-lens or generic-optics with 's3UploadMode' instead." #-}

-- | A URI that identifies the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msoS3URI :: Lens.Lens' MonitoringS3Output Lude.Text
msoS3URI = Lens.lens (s3URI :: MonitoringS3Output -> Lude.Text) (\s a -> s {s3URI = a} :: MonitoringS3Output)
{-# DEPRECATED msoS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

-- | The local path to the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job. LocalPath is an absolute path for the output data.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msoLocalPath :: Lens.Lens' MonitoringS3Output Lude.Text
msoLocalPath = Lens.lens (localPath :: MonitoringS3Output -> Lude.Text) (\s a -> s {localPath = a} :: MonitoringS3Output)
{-# DEPRECATED msoLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

instance Lude.FromJSON MonitoringS3Output where
  parseJSON =
    Lude.withObject
      "MonitoringS3Output"
      ( \x ->
          MonitoringS3Output'
            Lude.<$> (x Lude..:? "S3UploadMode")
            Lude.<*> (x Lude..: "S3Uri")
            Lude.<*> (x Lude..: "LocalPath")
      )

instance Lude.ToJSON MonitoringS3Output where
  toJSON MonitoringS3Output' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3UploadMode" Lude..=) Lude.<$> s3UploadMode,
            Lude.Just ("S3Uri" Lude..= s3URI),
            Lude.Just ("LocalPath" Lude..= localPath)
          ]
      )
