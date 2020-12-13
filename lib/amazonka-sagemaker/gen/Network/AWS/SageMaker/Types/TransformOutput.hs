{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformOutput
  ( TransformOutput (..),

    -- * Smart constructor
    mkTransformOutput,

    -- * Lenses
    toS3OutputPath,
    toAssembleWith,
    toAccept,
    toKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AssemblyType

-- | Describes the results of a transform job.
--
-- /See:/ 'mkTransformOutput' smart constructor.
data TransformOutput = TransformOutput'
  { -- | The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ .
    --
    -- For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
    s3OutputPath :: Lude.Text,
    -- | Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
    assembleWith :: Lude.Maybe AssemblyType,
    -- | The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
    accept :: Lude.Maybe Lude.Text,
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
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformOutput' with the minimum fields required to make a request.
--
-- * 's3OutputPath' - The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ .
--
-- For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
-- * 'assembleWith' - Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
-- * 'accept' - The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
-- * 'kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:
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
mkTransformOutput ::
  -- | 's3OutputPath'
  Lude.Text ->
  TransformOutput
mkTransformOutput pS3OutputPath_ =
  TransformOutput'
    { s3OutputPath = pS3OutputPath_,
      assembleWith = Lude.Nothing,
      accept = Lude.Nothing,
      kmsKeyId = Lude.Nothing
    }

-- | The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ .
--
-- For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toS3OutputPath :: Lens.Lens' TransformOutput Lude.Text
toS3OutputPath = Lens.lens (s3OutputPath :: TransformOutput -> Lude.Text) (\s a -> s {s3OutputPath = a} :: TransformOutput)
{-# DEPRECATED toS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
--
-- /Note:/ Consider using 'assembleWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAssembleWith :: Lens.Lens' TransformOutput (Lude.Maybe AssemblyType)
toAssembleWith = Lens.lens (assembleWith :: TransformOutput -> Lude.Maybe AssemblyType) (\s a -> s {assembleWith = a} :: TransformOutput)
{-# DEPRECATED toAssembleWith "Use generic-lens or generic-optics with 'assembleWith' instead." #-}

-- | The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
toAccept :: Lens.Lens' TransformOutput (Lude.Maybe Lude.Text)
toAccept = Lens.lens (accept :: TransformOutput -> Lude.Maybe Lude.Text) (\s a -> s {accept = a} :: TransformOutput)
{-# DEPRECATED toAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

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
toKMSKeyId :: Lens.Lens' TransformOutput (Lude.Maybe Lude.Text)
toKMSKeyId = Lens.lens (kmsKeyId :: TransformOutput -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: TransformOutput)
{-# DEPRECATED toKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON TransformOutput where
  parseJSON =
    Lude.withObject
      "TransformOutput"
      ( \x ->
          TransformOutput'
            Lude.<$> (x Lude..: "S3OutputPath")
            Lude.<*> (x Lude..:? "AssembleWith")
            Lude.<*> (x Lude..:? "Accept")
            Lude.<*> (x Lude..:? "KmsKeyId")
      )

instance Lude.ToJSON TransformOutput where
  toJSON TransformOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3OutputPath" Lude..= s3OutputPath),
            ("AssembleWith" Lude..=) Lude.<$> assembleWith,
            ("Accept" Lude..=) Lude.<$> accept,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )
