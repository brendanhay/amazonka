{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3Output
  ( ProcessingS3Output (..)
  -- * Smart constructor
  , mkProcessingS3Output
  -- * Lenses
  , psoS3Uri
  , psoLocalPath
  , psoS3UploadMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ProcessingLocalPath as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3UploadMode as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | Information about where and how you want to store the results of an processing job.
--
-- /See:/ 'mkProcessingS3Output' smart constructor.
data ProcessingS3Output = ProcessingS3Output'
  { s3Uri :: Types.S3Uri
    -- ^ A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
  , localPath :: Types.ProcessingLocalPath
    -- ^ The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
  , s3UploadMode :: Types.ProcessingS3UploadMode
    -- ^ Whether to upload the results of the processing job continuously or after the job completes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingS3Output' value with any optional fields omitted.
mkProcessingS3Output
    :: Types.S3Uri -- ^ 's3Uri'
    -> Types.ProcessingLocalPath -- ^ 'localPath'
    -> Types.ProcessingS3UploadMode -- ^ 's3UploadMode'
    -> ProcessingS3Output
mkProcessingS3Output s3Uri localPath s3UploadMode
  = ProcessingS3Output'{s3Uri, localPath, s3UploadMode}

-- | A URI that identifies the Amazon S3 bucket where you want Amazon SageMaker to save the results of a processing job.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psoS3Uri :: Lens.Lens' ProcessingS3Output Types.S3Uri
psoS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE psoS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

-- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to save the results of an processing job. @LocalPath@ is an absolute path to the input data.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psoLocalPath :: Lens.Lens' ProcessingS3Output Types.ProcessingLocalPath
psoLocalPath = Lens.field @"localPath"
{-# INLINEABLE psoLocalPath #-}
{-# DEPRECATED localPath "Use generic-lens or generic-optics with 'localPath' instead"  #-}

-- | Whether to upload the results of the processing job continuously or after the job completes.
--
-- /Note:/ Consider using 's3UploadMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psoS3UploadMode :: Lens.Lens' ProcessingS3Output Types.ProcessingS3UploadMode
psoS3UploadMode = Lens.field @"s3UploadMode"
{-# INLINEABLE psoS3UploadMode #-}
{-# DEPRECATED s3UploadMode "Use generic-lens or generic-optics with 's3UploadMode' instead"  #-}

instance Core.FromJSON ProcessingS3Output where
        toJSON ProcessingS3Output{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3Uri" Core..= s3Uri),
                  Core.Just ("LocalPath" Core..= localPath),
                  Core.Just ("S3UploadMode" Core..= s3UploadMode)])

instance Core.FromJSON ProcessingS3Output where
        parseJSON
          = Core.withObject "ProcessingS3Output" Core.$
              \ x ->
                ProcessingS3Output' Core.<$>
                  (x Core..: "S3Uri") Core.<*> x Core..: "LocalPath" Core.<*>
                    x Core..: "S3UploadMode"
