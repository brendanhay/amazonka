{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.OutputDataConfig
  ( OutputDataConfig (..),

    -- * Smart constructor
    mkOutputDataConfig,

    -- * Lenses
    odcS3Uri,
    odcKmsKeyId,
  )
where

import qualified Network.AWS.Comprehend.Types.KmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides configuration parameters for the output of topic detection jobs.
--
--
--
-- /See:/ 'mkOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | When you use the @OutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of the output file.
    --
    -- When the topic detection job is finished, the service creates an output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the ouput of the operation.
    s3Uri :: Types.S3Uri,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:
    --
    --
    --     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * KMS Key Alias: @"alias/ExampleAlias"@
    --
    --
    --     * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDataConfig' value with any optional fields omitted.
mkOutputDataConfig ::
  -- | 's3Uri'
  Types.S3Uri ->
  OutputDataConfig
mkOutputDataConfig s3Uri =
  OutputDataConfig' {s3Uri, kmsKeyId = Core.Nothing}

-- | When you use the @OutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of the output file.
--
-- When the topic detection job is finished, the service creates an output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the ouput of the operation.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcS3Uri :: Lens.Lens' OutputDataConfig Types.S3Uri
odcS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED odcS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * KMS Key Alias: @"alias/ExampleAlias"@
--
--
--     * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
--
--
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcKmsKeyId :: Lens.Lens' OutputDataConfig (Core.Maybe Types.KmsKeyId)
odcKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED odcKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON OutputDataConfig where
  toJSON OutputDataConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3Uri" Core..= s3Uri),
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON OutputDataConfig where
  parseJSON =
    Core.withObject "OutputDataConfig" Core.$
      \x ->
        OutputDataConfig'
          Core.<$> (x Core..: "S3Uri") Core.<*> (x Core..:? "KmsKeyId")
