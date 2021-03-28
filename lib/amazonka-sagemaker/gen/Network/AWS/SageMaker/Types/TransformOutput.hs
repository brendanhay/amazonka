{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformOutput
  ( TransformOutput (..)
  -- * Smart constructor
  , mkTransformOutput
  -- * Lenses
  , toS3OutputPath
  , toAccept
  , toAssembleWith
  , toKmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Accept as Types
import qualified Network.AWS.SageMaker.Types.AssemblyType as Types
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | Describes the results of a transform job.
--
-- /See:/ 'mkTransformOutput' smart constructor.
data TransformOutput = TransformOutput'
  { s3OutputPath :: Types.S3OutputPath
    -- ^ The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ .
--
-- For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
  , accept :: Core.Maybe Types.Accept
    -- ^ The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
  , assembleWith :: Core.Maybe Types.AssemblyType
    -- ^ Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats: 
--
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Alias name: @alias/ExampleAlias@ 
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@ 
--
--
-- If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./ 
-- The KMS key policy must grant permission to the IAM role that you specify in your 'CreateModel' request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformOutput' value with any optional fields omitted.
mkTransformOutput
    :: Types.S3OutputPath -- ^ 's3OutputPath'
    -> TransformOutput
mkTransformOutput s3OutputPath
  = TransformOutput'{s3OutputPath, accept = Core.Nothing,
                     assembleWith = Core.Nothing, kmsKeyId = Core.Nothing}

-- | The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ .
--
-- For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toS3OutputPath :: Lens.Lens' TransformOutput Types.S3OutputPath
toS3OutputPath = Lens.field @"s3OutputPath"
{-# INLINEABLE toS3OutputPath #-}
{-# DEPRECATED s3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead"  #-}

-- | The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAccept :: Lens.Lens' TransformOutput (Core.Maybe Types.Accept)
toAccept = Lens.field @"accept"
{-# INLINEABLE toAccept #-}
{-# DEPRECATED accept "Use generic-lens or generic-optics with 'accept' instead"  #-}

-- | Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
--
-- /Note:/ Consider using 'assembleWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAssembleWith :: Lens.Lens' TransformOutput (Core.Maybe Types.AssemblyType)
toAssembleWith = Lens.field @"assembleWith"
{-# INLINEABLE toAssembleWith #-}
{-# DEPRECATED assembleWith "Use generic-lens or generic-optics with 'assembleWith' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats: 
--
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ 
--
--
--     * Alias name: @alias/ExampleAlias@ 
--
--
--     * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@ 
--
--
-- If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./ 
-- The KMS key policy must grant permission to the IAM role that you specify in your 'CreateModel' request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toKmsKeyId :: Lens.Lens' TransformOutput (Core.Maybe Types.KmsKeyId)
toKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE toKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

instance Core.FromJSON TransformOutput where
        toJSON TransformOutput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3OutputPath" Core..= s3OutputPath),
                  ("Accept" Core..=) Core.<$> accept,
                  ("AssembleWith" Core..=) Core.<$> assembleWith,
                  ("KmsKeyId" Core..=) Core.<$> kmsKeyId])

instance Core.FromJSON TransformOutput where
        parseJSON
          = Core.withObject "TransformOutput" Core.$
              \ x ->
                TransformOutput' Core.<$>
                  (x Core..: "S3OutputPath") Core.<*> x Core..:? "Accept" Core.<*>
                    x Core..:? "AssembleWith"
                    Core.<*> x Core..:? "KmsKeyId"
