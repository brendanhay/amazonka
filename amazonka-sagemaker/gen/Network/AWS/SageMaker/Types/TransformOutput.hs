{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AssemblyType

-- | Describes the results of a transform job.
--
-- /See:/ 'newTransformOutput' smart constructor.
data TransformOutput = TransformOutput'
  { -- | The MIME type used to specify the output data. Amazon SageMaker uses the
    -- MIME type with each http call to transfer data from the transform job.
    accept :: Prelude.Maybe Prelude.Text,
    -- | Defines how to assemble the results of the transform job as a single S3
    -- object. Choose a format that is most convenient to you. To concatenate
    -- the results in binary format, specify @None@. To add a newline character
    -- at the end of every transformed record, specify @Line@.
    assembleWith :: Prelude.Maybe AssemblyType,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt the model artifacts at rest using Amazon S3 server-side
    -- encryption. The @KmsKeyId@ can be any of the following formats:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias name ARN:
    --     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
    --
    -- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
    -- KMS key for Amazon S3 for your role\'s account. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your CreateModel request. For more information, see
    -- <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
    -- in the /AWS Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 path where you want Amazon SageMaker to store the results
    -- of the transform job. For example,
    -- @s3:\/\/bucket-name\/key-name-prefix@.
    --
    -- For every S3 object used as input for the transform job, batch transform
    -- stores the transformed data with an .@out@ suffix in a corresponding
    -- subfolder in the location in the output prefix. For example, for the
    -- input data stored at
    -- @s3:\/\/bucket-name\/input-name-prefix\/dataset01\/data.csv@, batch
    -- transform stores the transformed data at
    -- @s3:\/\/bucket-name\/output-name-prefix\/input-name-prefix\/data.csv.out@.
    -- Batch transform doesn\'t upload partially processed objects. For an
    -- input S3 object that contains multiple records, it creates an .@out@
    -- file only if the transform job succeeds on the entire file. When the
    -- input contains multiple S3 objects, the batch transform job processes
    -- the listed S3 objects and uploads only the output for successfully
    -- processed objects. If any object fails in the transform job batch
    -- transform marks the job as failed to prompt investigation.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransformOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accept', 'transformOutput_accept' - The MIME type used to specify the output data. Amazon SageMaker uses the
-- MIME type with each http call to transfer data from the transform job.
--
-- 'assembleWith', 'transformOutput_assembleWith' - Defines how to assemble the results of the transform job as a single S3
-- object. Choose a format that is most convenient to you. To concatenate
-- the results in binary format, specify @None@. To add a newline character
-- at the end of every transformed record, specify @Line@.
--
-- 'kmsKeyId', 'transformOutput_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt the model artifacts at rest using Amazon S3 server-side
-- encryption. The @KmsKeyId@ can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your CreateModel request. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 's3OutputPath', 'transformOutput_s3OutputPath' - The Amazon S3 path where you want Amazon SageMaker to store the results
-- of the transform job. For example,
-- @s3:\/\/bucket-name\/key-name-prefix@.
--
-- For every S3 object used as input for the transform job, batch transform
-- stores the transformed data with an .@out@ suffix in a corresponding
-- subfolder in the location in the output prefix. For example, for the
-- input data stored at
-- @s3:\/\/bucket-name\/input-name-prefix\/dataset01\/data.csv@, batch
-- transform stores the transformed data at
-- @s3:\/\/bucket-name\/output-name-prefix\/input-name-prefix\/data.csv.out@.
-- Batch transform doesn\'t upload partially processed objects. For an
-- input S3 object that contains multiple records, it creates an .@out@
-- file only if the transform job succeeds on the entire file. When the
-- input contains multiple S3 objects, the batch transform job processes
-- the listed S3 objects and uploads only the output for successfully
-- processed objects. If any object fails in the transform job batch
-- transform marks the job as failed to prompt investigation.
newTransformOutput ::
  -- | 's3OutputPath'
  Prelude.Text ->
  TransformOutput
newTransformOutput pS3OutputPath_ =
  TransformOutput'
    { accept = Prelude.Nothing,
      assembleWith = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The MIME type used to specify the output data. Amazon SageMaker uses the
-- MIME type with each http call to transfer data from the transform job.
transformOutput_accept :: Lens.Lens' TransformOutput (Prelude.Maybe Prelude.Text)
transformOutput_accept = Lens.lens (\TransformOutput' {accept} -> accept) (\s@TransformOutput' {} a -> s {accept = a} :: TransformOutput)

-- | Defines how to assemble the results of the transform job as a single S3
-- object. Choose a format that is most convenient to you. To concatenate
-- the results in binary format, specify @None@. To add a newline character
-- at the end of every transformed record, specify @Line@.
transformOutput_assembleWith :: Lens.Lens' TransformOutput (Prelude.Maybe AssemblyType)
transformOutput_assembleWith = Lens.lens (\TransformOutput' {assembleWith} -> assembleWith) (\s@TransformOutput' {} a -> s {assembleWith = a} :: TransformOutput)

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt the model artifacts at rest using Amazon S3 server-side
-- encryption. The @KmsKeyId@ can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your CreateModel request. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
-- in the /AWS Key Management Service Developer Guide/.
transformOutput_kmsKeyId :: Lens.Lens' TransformOutput (Prelude.Maybe Prelude.Text)
transformOutput_kmsKeyId = Lens.lens (\TransformOutput' {kmsKeyId} -> kmsKeyId) (\s@TransformOutput' {} a -> s {kmsKeyId = a} :: TransformOutput)

-- | The Amazon S3 path where you want Amazon SageMaker to store the results
-- of the transform job. For example,
-- @s3:\/\/bucket-name\/key-name-prefix@.
--
-- For every S3 object used as input for the transform job, batch transform
-- stores the transformed data with an .@out@ suffix in a corresponding
-- subfolder in the location in the output prefix. For example, for the
-- input data stored at
-- @s3:\/\/bucket-name\/input-name-prefix\/dataset01\/data.csv@, batch
-- transform stores the transformed data at
-- @s3:\/\/bucket-name\/output-name-prefix\/input-name-prefix\/data.csv.out@.
-- Batch transform doesn\'t upload partially processed objects. For an
-- input S3 object that contains multiple records, it creates an .@out@
-- file only if the transform job succeeds on the entire file. When the
-- input contains multiple S3 objects, the batch transform job processes
-- the listed S3 objects and uploads only the output for successfully
-- processed objects. If any object fails in the transform job batch
-- transform marks the job as failed to prompt investigation.
transformOutput_s3OutputPath :: Lens.Lens' TransformOutput Prelude.Text
transformOutput_s3OutputPath = Lens.lens (\TransformOutput' {s3OutputPath} -> s3OutputPath) (\s@TransformOutput' {} a -> s {s3OutputPath = a} :: TransformOutput)

instance Prelude.FromJSON TransformOutput where
  parseJSON =
    Prelude.withObject
      "TransformOutput"
      ( \x ->
          TransformOutput'
            Prelude.<$> (x Prelude..:? "Accept")
            Prelude.<*> (x Prelude..:? "AssembleWith")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable TransformOutput

instance Prelude.NFData TransformOutput

instance Prelude.ToJSON TransformOutput where
  toJSON TransformOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Accept" Prelude..=) Prelude.<$> accept,
            ("AssembleWith" Prelude..=) Prelude.<$> assembleWith,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )
