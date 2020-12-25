{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3Input
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3Input
  ( ProcessingS3Input (..),

    -- * Smart constructor
    mkProcessingS3Input,

    -- * Lenses
    psiS3Uri,
    psiLocalPath,
    psiS3DataType,
    psiS3InputMode,
    psiS3CompressionType,
    psiS3DataDistributionType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.LocalPath as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3CompressionType as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3DataType as Types
import qualified Network.AWS.SageMaker.Types.ProcessingS3InputMode as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | Information about where and how you want to obtain the inputs for an processing job.
--
-- /See:/ 'mkProcessingS3Input' smart constructor.
data ProcessingS3Input = ProcessingS3Input'
  { -- | The URI for the Amazon S3 storage where you want Amazon SageMaker to download the artifacts needed to run a processing job.
    s3Uri :: Types.S3Uri,
    -- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to download the inputs to run a processing job. @LocalPath@ is an absolute path to the input data.
    localPath :: Types.LocalPath,
    -- | Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for the processing job. If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for the processing job.
    s3DataType :: Types.ProcessingS3DataType,
    -- | Whether to use @File@ or @Pipe@ input mode. In @File@ mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
    s3InputMode :: Types.ProcessingS3InputMode,
    -- | Whether to use @Gzip@ compression for Amazon S3 storage.
    s3CompressionType :: Core.Maybe Types.ProcessingS3CompressionType,
    -- | Whether the data stored in Amazon S3 is @FullyReplicated@ or @ShardedByS3Key@ .
    s3DataDistributionType :: Core.Maybe Types.ProcessingS3DataDistributionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingS3Input' value with any optional fields omitted.
mkProcessingS3Input ::
  -- | 's3Uri'
  Types.S3Uri ->
  -- | 'localPath'
  Types.LocalPath ->
  -- | 's3DataType'
  Types.ProcessingS3DataType ->
  -- | 's3InputMode'
  Types.ProcessingS3InputMode ->
  ProcessingS3Input
mkProcessingS3Input s3Uri localPath s3DataType s3InputMode =
  ProcessingS3Input'
    { s3Uri,
      localPath,
      s3DataType,
      s3InputMode,
      s3CompressionType = Core.Nothing,
      s3DataDistributionType = Core.Nothing
    }

-- | The URI for the Amazon S3 storage where you want Amazon SageMaker to download the artifacts needed to run a processing job.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiS3Uri :: Lens.Lens' ProcessingS3Input Types.S3Uri
psiS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED psiS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

-- | The local path to the Amazon S3 bucket where you want Amazon SageMaker to download the inputs to run a processing job. @LocalPath@ is an absolute path to the input data.
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiLocalPath :: Lens.Lens' ProcessingS3Input Types.LocalPath
psiLocalPath = Lens.field @"localPath"
{-# DEPRECATED psiLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

-- | Whether you use an @S3Prefix@ or a @ManifestFile@ for the data type. If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for the processing job. If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for the processing job.
--
-- /Note:/ Consider using 's3DataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiS3DataType :: Lens.Lens' ProcessingS3Input Types.ProcessingS3DataType
psiS3DataType = Lens.field @"s3DataType"
{-# DEPRECATED psiS3DataType "Use generic-lens or generic-optics with 's3DataType' instead." #-}

-- | Whether to use @File@ or @Pipe@ input mode. In @File@ mode, Amazon SageMaker copies the data from the input source onto the local Amazon Elastic Block Store (Amazon EBS) volumes before starting your training algorithm. This is the most commonly used input mode. In @Pipe@ mode, Amazon SageMaker streams input data from the source directly to your algorithm without using the EBS volume.
--
-- /Note:/ Consider using 's3InputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiS3InputMode :: Lens.Lens' ProcessingS3Input Types.ProcessingS3InputMode
psiS3InputMode = Lens.field @"s3InputMode"
{-# DEPRECATED psiS3InputMode "Use generic-lens or generic-optics with 's3InputMode' instead." #-}

-- | Whether to use @Gzip@ compression for Amazon S3 storage.
--
-- /Note:/ Consider using 's3CompressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiS3CompressionType :: Lens.Lens' ProcessingS3Input (Core.Maybe Types.ProcessingS3CompressionType)
psiS3CompressionType = Lens.field @"s3CompressionType"
{-# DEPRECATED psiS3CompressionType "Use generic-lens or generic-optics with 's3CompressionType' instead." #-}

-- | Whether the data stored in Amazon S3 is @FullyReplicated@ or @ShardedByS3Key@ .
--
-- /Note:/ Consider using 's3DataDistributionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psiS3DataDistributionType :: Lens.Lens' ProcessingS3Input (Core.Maybe Types.ProcessingS3DataDistributionType)
psiS3DataDistributionType = Lens.field @"s3DataDistributionType"
{-# DEPRECATED psiS3DataDistributionType "Use generic-lens or generic-optics with 's3DataDistributionType' instead." #-}

instance Core.FromJSON ProcessingS3Input where
  toJSON ProcessingS3Input {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3Uri" Core..= s3Uri),
            Core.Just ("LocalPath" Core..= localPath),
            Core.Just ("S3DataType" Core..= s3DataType),
            Core.Just ("S3InputMode" Core..= s3InputMode),
            ("S3CompressionType" Core..=) Core.<$> s3CompressionType,
            ("S3DataDistributionType" Core..=)
              Core.<$> s3DataDistributionType
          ]
      )

instance Core.FromJSON ProcessingS3Input where
  parseJSON =
    Core.withObject "ProcessingS3Input" Core.$
      \x ->
        ProcessingS3Input'
          Core.<$> (x Core..: "S3Uri")
          Core.<*> (x Core..: "LocalPath")
          Core.<*> (x Core..: "S3DataType")
          Core.<*> (x Core..: "S3InputMode")
          Core.<*> (x Core..:? "S3CompressionType")
          Core.<*> (x Core..:? "S3DataDistributionType")
