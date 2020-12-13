{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3Output
  ( ProcessingS3Output (..),

    -- * Smart constructor
    mkProcessingS3Output,

    -- * Lenses
    psoLocalPath,
    psoS3UploadMode,
    psoS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode

-- | Information about where and how you want to store the results of an processing job.
--
-- /See:/ 'mkProcessingS3Output' smart constructor.
data ProcessingS3Output = ProcessingS3Output'
  { -- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
    localPath :: Lude.Text,
    -- | Whether to upload the results of the processing job continuously or after the job completes.
    s3UploadMode :: ProcessingS3UploadMode,
    -- | A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
    s3URI :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingS3Output' with the minimum fields required to make a request.
--
-- * 'localPath' - The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
-- * 's3UploadMode' - Whether to upload the results of the processing job continuously or after the job completes.
-- * 's3URI' - A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
mkProcessingS3Output ::
  -- | 'localPath'
  Lude.Text ->
  -- | 's3UploadMode'
  ProcessingS3UploadMode ->
  -- | 's3URI'
  Lude.Text ->
  ProcessingS3Output
mkProcessingS3Output pLocalPath_ pS3UploadMode_ pS3URI_ =
  ProcessingS3Output'
    { localPath = pLocalPath_,
      s3UploadMode = pS3UploadMode_,
      s3URI = pS3URI_
    }

-- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psoLocalPath :: Lens.Lens' ProcessingS3Output Lude.Text
psoLocalPath = Lens.lens (localPath :: ProcessingS3Output -> Lude.Text) (\s a -> s {localPath = a} :: ProcessingS3Output)
{-# DEPRECATED psoLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

-- | Whether to upload the results of the processing job continuously or after the job completes.
--
-- /Note:/ Consider using 's3UploadMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psoS3UploadMode :: Lens.Lens' ProcessingS3Output ProcessingS3UploadMode
psoS3UploadMode = Lens.lens (s3UploadMode :: ProcessingS3Output -> ProcessingS3UploadMode) (\s a -> s {s3UploadMode = a} :: ProcessingS3Output)
{-# DEPRECATED psoS3UploadMode "Use generic-lens or generic-optics with 's3UploadMode' instead." #-}

-- | A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psoS3URI :: Lens.Lens' ProcessingS3Output Lude.Text
psoS3URI = Lens.lens (s3URI :: ProcessingS3Output -> Lude.Text) (\s a -> s {s3URI = a} :: ProcessingS3Output)
{-# DEPRECATED psoS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON ProcessingS3Output where
  parseJSON =
    Lude.withObject
      "ProcessingS3Output"
      ( \x ->
          ProcessingS3Output'
            Lude.<$> (x Lude..: "LocalPath")
            Lude.<*> (x Lude..: "S3UploadMode")
            Lude.<*> (x Lude..: "S3Uri")
      )

instance Lude.ToJSON ProcessingS3Output where
  toJSON ProcessingS3Output' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("LocalPath" Lude..= localPath),
            Lude.Just ("S3UploadMode" Lude..= s3UploadMode),
            Lude.Just ("S3Uri" Lude..= s3URI)
          ]
      )
