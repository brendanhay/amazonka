{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringS3Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringS3Output
  ( MonitoringS3Output (..)
  -- * Smart constructor
  , mkMonitoringS3Output
  -- * Lenses
  , msoS3Uri
  , msoLocalPath
  , msoS3UploadMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.MonitoringS3Uri as Types
import qualified Network.AWS.SageMaker.Types.ProcessingLocalPath as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3UploadMode as Types

-- | Information about where and how you want to store the results of a monitoring job.
--
-- /See:/ 'mkMonitoringS3Output' smart constructor.
data MonitoringS3Output = MonitoringS3Output'
  { s3Uri :: Types.MonitoringS3Uri
    -- ^ A URI that identifies the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job.
  , localPath :: Types.ProcessingLocalPath
    -- ^ The local path to the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job. LocalPath is an absolute path for the output data.
  , s3UploadMode :: Core.Maybe Types.ProcessingS3UploadMode
    -- ^ Whether to upload the results of the monitoring job continuously or after the job completes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringS3Output' value with any optional fields omitted.
mkMonitoringS3Output
    :: Types.MonitoringS3Uri -- ^ 's3Uri'
    -> Types.ProcessingLocalPath -- ^ 'localPath'
    -> MonitoringS3Output
mkMonitoringS3Output s3Uri localPath
  = MonitoringS3Output'{s3Uri, localPath,
                        s3UploadMode = Core.Nothing}

-- | A URI that identifies the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msoS3Uri :: Lens.Lens' MonitoringS3Output Types.MonitoringS3Uri
msoS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE msoS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

-- | The local path to the Amazon S3 storage location where Amazon SageMaker saves the results of a monitoring job. LocalPath is an absolute path for the output data.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msoLocalPath :: Lens.Lens' MonitoringS3Output Types.ProcessingLocalPath
msoLocalPath = Lens.field @"localPath"
{-# INLINEABLE msoLocalPath #-}
{-# DEPRECATED localPath "Use generic-lens or generic-optics with 'localPath' instead"  #-}

-- | Whether to upload the results of the monitoring job continuously or after the job completes.
--
-- /Note:/ Consider using 's3UploadMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msoS3UploadMode :: Lens.Lens' MonitoringS3Output (Core.Maybe Types.ProcessingS3UploadMode)
msoS3UploadMode = Lens.field @"s3UploadMode"
{-# INLINEABLE msoS3UploadMode #-}
{-# DEPRECATED s3UploadMode "Use generic-lens or generic-optics with 's3UploadMode' instead"  #-}

instance Core.FromJSON MonitoringS3Output where
        toJSON MonitoringS3Output{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3Uri" Core..= s3Uri),
                  Core.Just ("LocalPath" Core..= localPath),
                  ("S3UploadMode" Core..=) Core.<$> s3UploadMode])

instance Core.FromJSON MonitoringS3Output where
        parseJSON
          = Core.withObject "MonitoringS3Output" Core.$
              \ x ->
                MonitoringS3Output' Core.<$>
                  (x Core..: "S3Uri") Core.<*> x Core..: "LocalPath" Core.<*>
                    x Core..:? "S3UploadMode"
